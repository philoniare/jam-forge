package io.forge.jam.node

import java.util.concurrent.{CopyOnWriteArrayList, CountDownLatch, TimeUnit}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

/** The node's pools stop newest first and each is waited for, so nothing runs once `close` returns. */
class NodeThreadsSpec extends AnyFunSuite with Matchers:

  test("pools are named, daemon, and closed newest first, each after its running task ended") {
    val threads = new NodeThreads
    val order = new CopyOnWriteArrayList[String]()
    def busy(name: String, pool: java.util.concurrent.ExecutorService): Unit =
      val started = new CountDownLatch(1)
      pool.execute { () =>
        started.countDown()
        try Thread.sleep(60_000)
        catch
          case _: InterruptedException =>
            Thread.sleep(50) // still winding down when the interrupt lands
            order.add(name)
      }
      started.await(5, TimeUnit.SECONDS) shouldBe true
    val names = new CopyOnWriteArrayList[(String, Boolean)]()
    val first = threads.single("t-first")
    first.submit(() => names.add((Thread.currentThread.getName, Thread.currentThread.isDaemon))).get(5, TimeUnit.SECONDS)
    busy("first", first)
    busy("second", threads.fixed("t-second", 2))
    busy("third", threads.scheduled("t-third"))
    threads.close(5000) shouldBe Nil
    order.asScala.toList shouldBe List("third", "second", "first")
    names.asScala.toList shouldBe List(("t-first", true))
    first.isTerminated shouldBe true
  }

  test("a pool whose task ignores the interrupt is reported, and the others still close") {
    val threads = new NodeThreads
    val release = new CountDownLatch(1)
    val stuck = threads.single("t-stuck")
    val started = new CountDownLatch(1)
    stuck.execute { () =>
      started.countDown()
      var done = false
      while !done do
        try done = release.await(60, TimeUnit.SECONDS)
        catch case _: InterruptedException => ()
    }
    started.await(5, TimeUnit.SECONDS) shouldBe true
    val fine = threads.cached("t-fine")
    try
      threads.close(200) shouldBe List("t-stuck")
      fine.isTerminated shouldBe true
    finally release.countDown()
  }

  test("a pool made after close is refused, since nothing would stop it") {
    val threads = new NodeThreads
    threads.close(100)
    an[IllegalStateException] should be thrownBy threads.single("t-late")
  }

  test("numbered threads carry the pool's name and a count") {
    val threads = new NodeThreads
    val pool = threads.fixed("t-n", 2, numbered = true)
    val seen = (1 to 2).map(_ => pool.submit(() => Thread.currentThread.getName)).map(_.get(5, TimeUnit.SECONDS))
    seen.toSet.subsetOf(Set("t-n-1", "t-n-2")) shouldBe true
    threads.close(1000) shouldBe Nil
  }
