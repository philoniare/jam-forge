package io.forge.jam.protocol.refine

import java.util.concurrent.{Executors, TimeUnit}

import io.forge.jam.core.JamBytes
import io.forge.jam.pvm.engine.InterpretedModule
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoundedModuleCacheSpec extends AnyFunSuite with Matchers:

  private val module = InterpretedModule.createForTest(code = Array[Byte](0), bitmask = Array[Byte](0x01))
  private val keys = (0 until 64).map(i => JamBytes(Array[Byte](i.toByte)))

  test("many threads reading and filling one cache neither fail nor lose its bound") {
    val cache = new BoundedModuleCache(8)
    val threads = 8
    val pool = Executors.newFixedThreadPool(threads)
    try
      val runs = (0 until threads).map { t =>
        pool.submit[Unit] { () =>
          val rnd = new scala.util.Random(t)
          for _ <- 0 until 200_000 do cache.getOrCompile(keys(rnd.nextInt(keys.size)))(Some(module)) shouldBe Some(module)
        }
      }
      runs.foreach(_.get(60, TimeUnit.SECONDS))
      cache.size should be <= 8
    finally pool.shutdownNow()
  }
