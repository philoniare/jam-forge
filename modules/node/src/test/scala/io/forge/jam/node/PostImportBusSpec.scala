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
