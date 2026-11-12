package io.forge.jam.pvm.engine

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import org.scalatest.funsuite.AnyFunSuite

class ModuleCacheSpec extends AnyFunSuite:

  private def module(): InterpretedModule = InterpretedModule.createForTest(Array[Byte](0), Array[Byte](1), true)

  test("a hit returns the cached module without building again"):
    val cache = ModuleCache[String](4)
    val builds = AtomicInteger()
    val code = Array[Byte](1, 2, 3)
    val a = cache.getOrCreate("h", code) { builds.incrementAndGet(); Some(module()) }
    val b = cache.getOrCreate("h", code.clone()) { builds.incrementAndGet(); Some(module()) }
    assert(a.get eq b.get)
    assert(builds.get == 1)

  test("different code under the same key builds a new module"):
    val cache = ModuleCache[String](4)
    val a = cache.getOrCreate("h", Array[Byte](1))(Some(module()))
    val b = cache.getOrCreate("h", Array[Byte](2))(Some(module()))
    assert(!(a.get eq b.get))
    assert(cache.getOrCreate("h", Array[Byte](2))(fail("rebuilt")).get eq b.get)

  test("failures are not cached and the cache is bounded"):
    val cache = ModuleCache[Int](2)
    assert(cache.getOrCreate(0, Array[Byte](0))(None).isEmpty)
    assert(cache.size == 0)
    for k <- 1 to 5 do cache.getOrCreate(k, Array[Byte](k.toByte))(Some(module()))
    assert(cache.size == 2)

  test("concurrent misses on one key all return the same module"):
    val cache = ModuleCache[String](4)
    given ExecutionContext = ExecutionContext.global
    val code = Array[Byte](9)
    val results = Await.result(Future.sequence((1 to 16).map(_ => Future(cache.getOrCreate("h", code)(Some(module())).get))), 30.seconds)
    assert(results.forall(_ eq cache.getOrCreate("h", code)(fail("rebuilt")).get))

  /** A module of `codeBytes` one-byte trap instructions; its weight is dominated by the per-pc tables. */
  private def moduleOf(codeBytes: Int): InterpretedModule =
    val bitmask = Array.fill[Byte]((codeBytes + 7) / 8)(0xff.toByte)
    if codeBytes % 8 != 0 then bitmask(bitmask.length - 1) = ((1 << (codeBytes % 8)) - 1).toByte
    InterpretedModule.createForTest(new Array[Byte](codeBytes), bitmask, true)

  test("entries are evicted, least recently used first, to stay within the byte budget"):
    val code = (k: Int) => Array[Byte](k.toByte)
    val weight = 1L + moduleOf(10_000).retainedBytes
    assert(weight > 50_000)
    val cache = ModuleCache[Int](100, maxBytes = weight * 5 / 2)
    val built = (1 to 5).map(k => k -> cache.getOrCreate(k, code(k))(Some(moduleOf(10_000))).get).toMap
    assert(cache.size == 2)
    assert(cache.retainedBytes == 2 * weight)
    assert(cache.getOrCreate(4, code(4))(fail("4 was evicted")).get eq built(4))
    assert(cache.getOrCreate(5, code(5))(fail("5 was evicted")).get eq built(5))
    // 4 is now the least recently used: adding one more drops it and keeps 5.
    cache.getOrCreate(4, code(4))(fail("4 was evicted"))
    cache.getOrCreate(5, code(5))(fail("5 was evicted"))
    cache.getOrCreate(6, code(6))(Some(moduleOf(10_000)))
    assert(cache.size == 2)
    assert(cache.getOrCreate(5, code(5))(fail("5 was evicted")).get eq built(5))
    assert(!(cache.getOrCreate(4, code(4))(Some(moduleOf(10_000))).get eq built(4)))

  test("an entry over the whole budget is kept while it is the most recent, then dropped"):
    val cache = ModuleCache[Int](100, maxBytes = 1000)
    val big = cache.getOrCreate(1, Array[Byte](1))(Some(moduleOf(10_000))).get
    assert(cache.size == 1)
    assert(cache.getOrCreate(1, Array[Byte](1))(fail("rebuilt")).get eq big)
    cache.getOrCreate(2, Array[Byte](2))(Some(module()))
    assert(cache.size == 1)
    assert(cache.getOrCreate(2, Array[Byte](2))(fail("rebuilt")).isDefined)

  test("a module that grows by compiling blocks is weighed again"):
    val idle = moduleOf(10_000)
    val before = idle.retainedBytes
    val grown = moduleOf(10_000)
    // Running from every pc compiles every instruction as its own block.
    for pc <- 0 until 10_000 do
      val inst = InterpretedInstance.fromModule(grown)
      inst.setGas(1000L)
      inst.setNextProgramCounter(io.forge.jam.pvm.types.ProgramCounter(pc))
      inst.run()
    assert(grown.retainedBytes > before + 10_000L * 64)

    // Budget for the two modules as inserted, not for the grown one.
    val cache = ModuleCache[Int](100, maxBytes = 2 * (1 + before) + 1000)
    val fresh = moduleOf(10_000)
    cache.getOrCreate(1, Array[Byte](1))(Some(fresh))
    cache.getOrCreate(2, Array[Byte](2))(Some(moduleOf(10_000)))
    assert(cache.size == 2)
    for pc <- 0 until 10_000 do
      val inst = InterpretedInstance.fromModule(fresh)
      inst.setGas(1000L)
      inst.setNextProgramCounter(io.forge.jam.pvm.types.ProgramCounter(pc))
      inst.run()
    // Lookups alone weigh the entries again every 256th time.
    for _ <- 0 until 600 do cache.getOrCreate(2, Array[Byte](2))(fail("rebuilt"))
    assert(cache.size == 1)
    assert(cache.getOrCreate(2, Array[Byte](2))(fail("rebuilt")).isDefined)
