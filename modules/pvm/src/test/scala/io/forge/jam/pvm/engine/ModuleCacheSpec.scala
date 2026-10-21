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
