package io.forge.jam.protocol.refine

import java.util.concurrent.atomic.AtomicInteger

import io.forge.jam.core.{JamBytes, Parallel}
import io.forge.jam.protocol.accumulation.{PvmInstance, ServiceCode}
import io.forge.jam.pvm.engine.InterpretedModule
import io.forge.jam.pvm.memory.InstanceMemoryBudget
import org.scalatest.funsuite.AnyFunSuite

/** Services run in parallel (one accumulation batch) share the instance memory budget: however many declare a large
  * heap, the instances alive at once stay within it, and every run computes what it computes when run alone.
  */
class PvmRunnerMemoryBudgetSpec extends AnyFunSuite:

  private val MiB = 1L << 20
  private val pageSize = 4096
  private val heapBase = 0x20000 // two 64 KiB zones: no read-only and no read-write data, so the heap starts here

  private def le(v: Int, n: Int): Array[Byte] = Array.tabulate(n)(i => (v >>> (8 * i)).toByte)

  /** `ecalli 100; jump_ind r0` (halt): the host call does the work. */
  private val program: Array[Byte] =
    val code = Array[Byte](10, 100, 50, 0)
    Array[Byte](0, 0, code.length.toByte) ++ code ++ Array[Byte](0x05)

  private def module(heapPages: Int): InterpretedModule =
    val blob = le(0, 3) ++ le(0, 3) ++ le(heapPages, 2) ++ le(4096, 3) ++ le(program.length, 4) ++ program
    ServiceCode.module(JamBytes(Array.fill[Byte](31)(0x7b) :+ (heapPages % 251).toByte), blob).get

  private val live = new AtomicInteger()
  private val peakLive = new AtomicInteger()

  /** The host call of service `id`: optionally grows the heap, then writes the service's result at the end of the
    * heap and returns it as the run's output. It stays in the call for a moment so that parallel runs overlap
    * (no assertion depends on that), or, with `gather`, until that many instances have been alive at once.
    */
  private final class Service(
      id: Int,
      heapPages: Int,
      growPages: Int = 0,
      nested: () => Unit = () => (),
      gather: Int = 0
  ) extends HostCallDispatcher:
    def getGasCost(hostCallId: Int, instance: PvmInstance): Long = 10L + id
    def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
      val now = live.incrementAndGet()
      peakLive.accumulateAndGet(now, math.max(_, _))
      try
        if gather > 0 then
          val deadline = System.nanoTime() + 30_000_000_000L
          while peakLive.get() < gather && System.nanoTime() < deadline do Thread.sleep(1)
        else Thread.sleep(40)
        nested()
        if growPages > 0 then instance.growHeapPages(growPages.toLong)
        val result = s"service-$id-result".getBytes("UTF-8")
        val at = heapBase + (heapPages + growPages) * pageSize - 64
        assert(instance.writeBytes(at, result), "the end of the heap must be writable")
        instance.setReg(7, at.toLong)
        instance.setReg(8, result.length.toLong)
      finally live.decrementAndGet()

  private type Outcome = (PvmRunner.PvmExit, Long, Seq[Byte])

  private def run(m: InterpretedModule, service: Service, budget: InstanceMemoryBudget): Outcome =
    val (exit, gas, out) = PvmRunner.run(m, Array[Byte](1), 1_000_000L, 0, service, memoryBudget = budget)
    (exit, gas, out.toSeq)

  test("16 services declaring a 16 MiB heap, in parallel: live instance memory stays within the budget, results as when run one by one") {
    val heapPages = 4096 // 16 MiB
    val m = module(heapPages)
    val ids = (1 to 16).toList
    val unlimited = new InstanceMemoryBudget(Long.MaxValue)
    val sequential = ids.map(i => run(m, new Service(i, heapPages), unlimited))
    assert(sequential.forall(_._1 == PvmRunner.PvmExit.Halt))
    assert(sequential.map(o => new String(o._3.toArray, "UTF-8")) == ids.map(i => s"service-$i-result"))

    // Without a limit: every thread of the pool has an instance alive at once. Each run stays in its
    // host call until four are alive, which the pool allows on four or more cores.
    val manyCores = Runtime.getRuntime.availableProcessors() >= 4
    peakLive.set(0)
    unlimited.resetPeak()
    val gather = if manyCores then 4 else 0
    assert(Parallel.map(ids)(i => run(m, new Service(i, heapPages, gather = gather), unlimited)) == sequential)
    val unlimitedLive = peakLive.get()
    val unlimitedBytes = unlimited.peakBytes

    // With a 64 MiB budget: three 16 MiB instances fit (each also has a stack and its argument), a fourth waits.
    val budget = new InstanceMemoryBudget(64 * MiB)
    peakLive.set(0)
    assert(Parallel.map(ids)(i => run(m, new Service(i, heapPages), budget)) == sequential)
    assert(budget.peakBytes <= 64 * MiB)
    assert(peakLive.get() <= 3)
    assert(budget.inUseBytes == 0)

    info(
      s"peak live instances: ${peakLive.get()} with the budget, $unlimitedLive without " +
        s"(${budget.peakBytes / MiB} MiB against ${unlimitedBytes / MiB} MiB reserved)"
    )
    if manyCores then
      assert(unlimitedLive > 3, "without the budget more instances are alive at once")
      assert(unlimitedBytes > 64 * MiB)
  }

  test("services that grow their heap during the run are counted too, and do not deadlock when all want more") {
    val heapPages = 256 // 1 MiB declared
    val growPages = 4096 // then 16 MiB more
    val m = module(heapPages)
    val ids = (1 to 8).toList
    val unlimited = new InstanceMemoryBudget(Long.MaxValue)
    val sequential = ids.map(i => run(m, new Service(i, heapPages, growPages), unlimited))
    assert(sequential.forall(_._1 == PvmRunner.PvmExit.Halt))
    assert(sequential.map(o => new String(o._3.toArray, "UTF-8")) == ids.map(i => s"service-$i-result"))

    // All eight start (1 MiB each fits), then each wants 17 MiB more than the 24 MiB budget has left for all.
    val budget = new InstanceMemoryBudget(24 * MiB)
    assert(Parallel.map(ids)(i => run(m, new Service(i, heapPages, growPages), budget)) == sequential)
    assert(budget.inUseBytes == 0)
    // The budget plus one run let through; during a grow the old and the new buffer are both counted.
    assert(budget.peakBytes <= 24 * MiB + 2 * (heapPages + growPages) * pageSize.toLong)
  }

  test("an instance larger than the whole budget still runs, and a run started from inside a run does not wait for it") {
    val heapPages = 4096
    val m = module(heapPages)
    val budget = new InstanceMemoryBudget(1 * MiB)
    val alone = run(m, new Service(1, heapPages), budget)
    assert(alone._1 == PvmRunner.PvmExit.Halt)

    var inner: Outcome = null
    val outer = run(m, new Service(2, heapPages, nested = () => inner = run(m, new Service(3, heapPages), budget)), budget)
    assert(outer._1 == PvmRunner.PvmExit.Halt)
    assert(inner._1 == PvmRunner.PvmExit.Halt)
    assert(new String(inner._3.toArray, "UTF-8") == "service-3-result")
    assert(budget.inUseBytes == 0)
  }
