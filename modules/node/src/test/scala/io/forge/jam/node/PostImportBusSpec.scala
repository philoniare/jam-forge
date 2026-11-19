package io.forge.jam.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PostImportBusSpec extends AnyFunSuite with Matchers:

  test("publish delivers in order, on a different thread, and never blocks the caller") {
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[Int]()
    val callerThread = Thread.currentThread()
    @volatile var deliveryThread: Thread = null
    val bus = new PostImportBus[Int, Null]((slot, _) => {
      deliveryThread = Thread.currentThread()
      Thread.sleep(20) // simulate slow listener
      seen.add(slot)
      ()
    })

    val t0 = System.nanoTime()
    (1 to 5).foreach(i => bus.publish(i, null))
    val publishNanos = System.nanoTime() - t0

    publishNanos should be < 20_000_000L

    bus.close()

    seen.toArray.toList shouldBe List(1, 2, 3, 4, 5)
    deliveryThread should not be callerThread
  }

  private def held(capacity: Int, maxWaitMillis: Long = 30_000L) =
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[Int]()
    val release = new java.util.concurrent.CountDownLatch(1)
    val bus = new PostImportBus[Int, Null](
      (slot, _) => { release.await(); seen.add(slot); () },
      capacity,
      maxWaitMillis
    )
    (bus, seen, release)

  test("a publisher that finds the queue full waits for the listener: every event is delivered, in order") {
    val (bus, seen, release) = held(capacity = 4)
    val published = new java.util.concurrent.atomic.AtomicInteger()
    val publisher = new Thread(() => (1 to 20).foreach { i => bus.publish(i, null); published.incrementAndGet() })
    publisher.start()
    Thread.sleep(400)
    publisher.isAlive shouldBe true
    published.get should be <= 5
    release.countDown()
    publisher.join(10_000)
    publisher.isAlive shouldBe false
    bus.close()
    seen.toArray.toList shouldBe (1 to 20).toList
  }

  test("a publisher that may not wait drops what does not fit") {
    val (bus, seen, release) = held(capacity = 2)
    val t0 = System.nanoTime()
    (1 to 10).foreach(i => bus.publish(i, null, mayWait = false))
    (System.nanoTime() - t0) should be < 200_000_000L
    release.countDown()
    bus.close()
    val delivered = seen.toArray.toList.map(_.asInstanceOf[Int])
    delivered.take(2) shouldBe List(1, 2)
    delivered shouldBe delivered.sorted
    delivered.size should (be >= 2 and be <= 3)
  }

  test("a publisher gives up on a listener that stays stuck, and drops the event") {
    val (bus, seen, release) = held(capacity = 1, maxWaitMillis = 200L)
    val t0 = System.nanoTime()
    (1 to 3).foreach(i => bus.publish(i, null))
    val waited = (System.nanoTime() - t0) / 1_000_000L
    waited should (be >= 200L and be < 5_000L)
    release.countDown()
    bus.close()
    seen.size should be <= 2
  }

