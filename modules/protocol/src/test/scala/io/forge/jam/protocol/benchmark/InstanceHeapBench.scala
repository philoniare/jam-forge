package io.forge.jam.protocol.benchmark

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import io.forge.jam.core.{JamBytes, Parallel}
import io.forge.jam.protocol.accumulation.{PvmInstance, ServiceCode}
import io.forge.jam.protocol.refine.{HostCallDispatcher, PvmRunner}

/** Memory held by PVM instances that run at the same time, as the services of one accumulation batch do:
  * `--services=16` services each declaring `--heap-pages=65535` (the largest declarable heap, about 256 MiB) run
  * through `Parallel.map`, each making one host call that lasts `--hold-ms=300`.
  *
  * Prints the most instances alive at once, the JVM heap in use at that moment (sampled inside the host calls,
  * while the instances are alive) and the wall time of the batch.
  *
  * {{{ sbt "protocol/Test/runMain io.forge.jam.protocol.benchmark.InstanceHeapBench" }}}
  */
object InstanceHeapBench:

  private def le(v: Int, n: Int): Array[Byte] = Array.tabulate(n)(i => (v >>> (8 * i)).toByte)

  def main(args: Array[String]): Unit =
    val opts = args.collect { case s"--$k=$v" => k -> v }.toMap
    val services = opts.get("services").map(_.toInt).getOrElse(16)
    val heapPages = opts.get("heap-pages").map(_.toInt).getOrElse(65535)
    val holdMs = opts.get("hold-ms").map(_.toLong).getOrElse(300L)
    val reps = opts.get("reps").map(_.toInt).getOrElse(3)

    // `ecalli 100; jump_ind r0` (halt).
    val code = Array[Byte](10, 100, 50, 0)
    val program = Array[Byte](0, 0, code.length.toByte) ++ code ++ Array[Byte](0x05)
    val blob = le(0, 3) ++ le(0, 3) ++ le(heapPages, 2) ++ le(4096, 3) ++ le(program.length, 4) ++ program
    val module = ServiceCode.module(JamBytes(Array.fill[Byte](32)(0x7c)), blob).get

    val live = new AtomicInteger()
    val peakLive = new AtomicInteger()
    val peakHeap = new AtomicLong()
    val runtime = Runtime.getRuntime
    val hostCalls = new HostCallDispatcher:
      def getGasCost(hostCallId: Int, instance: PvmInstance): Long = 10L
      def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
        peakLive.accumulateAndGet(live.incrementAndGet(), math.max(_, _))
        try
          val until = System.nanoTime() + holdMs * 1000000L
          while System.nanoTime() < until do
            peakHeap.accumulateAndGet(runtime.totalMemory() - runtime.freeMemory(), math.max(_, _))
            Thread.sleep(10)
        finally live.decrementAndGet()

    println(
      s"services=$services heapPages=$heapPages (${heapPages.toLong * 4096 >> 20} MiB each) " +
        s"cores=${runtime.availableProcessors()} maxHeap=${runtime.maxMemory() >> 20} MiB"
    )
    for rep <- 1 to reps do
      System.gc()
      peakLive.set(0)
      peakHeap.set(0)
      val t0 = System.nanoTime()
      val results = Parallel.map((1 to services).toList) { _ =>
        PvmRunner.run(module, Array[Byte](1), 1_000_000L, 0, hostCalls)._1
      }
      val ms = (System.nanoTime() - t0) / 1000000
      require(results.forall(_ == PvmRunner.PvmExit.Halt), s"unexpected exits: $results")
      println(
        s"RESULT instance-heap rep=$rep peak_live_instances=${peakLive.get()} " +
          s"peak_live_instance_mib=${peakLive.get().toLong * heapPages * 4096 >> 20} " +
          s"peak_heap_used_mib=${peakHeap.get() >> 20} wall_ms=$ms"
      )
