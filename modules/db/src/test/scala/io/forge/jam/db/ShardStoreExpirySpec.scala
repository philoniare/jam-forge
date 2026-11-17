package io.forge.jam.db

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicLong

import io.forge.jam.core.primitives.Hash
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ShardStoreExpirySpec extends AnyFunSuite with Matchers:

  private def withStore(clock: () => Long)(body: ShardStore => Unit): Unit =
    val dir = Files.createTempDirectory("shard-expiry")
    val store = ShardStore.open(dir, clock = clock)
    try body(store)
    finally
      store.close()
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  private def root(i: Int): Hash = Hash(Array.fill[Byte](32)(i.toByte))

  test("a stale time entry for a root stored again since does not expire it; the entry itself is dropped") {
    var now = 100L
    withStore(() => now) { store =>
      store.put(root(1), 0, Array[Byte](1))
      now = 200L
      store.put(root(1), 1, Array[Byte](2))
      store.putTimeEntryForTesting(root(1), 100L)
      store.timeEntriesForTesting(root(1)) shouldBe List(100L, 200L)

      store.deleteStoredBefore(150L) shouldBe 0
      store.heldIndexes(root(1)) shouldBe List(0, 1)
      store.timeEntriesForTesting(root(1)) shouldBe List(200L)
      store.deleteStoredBefore(250L) shouldBe 2
      store.heldIndexes(root(1)) shouldBe Nil
      store.timeEntriesForTesting(root(1)) shouldBe Nil
    }
  }

  test("concurrent stores of one root at different times leave one time entry, and an expiry racing with them keeps every unit stored after its cutoff") {
    val clock = new AtomicLong(1000L)
    withStore(() => clock.incrementAndGet()) { store =>
      val pool = Executors.newFixedThreadPool(8)
      try
        for round <- 0 until 20 do
          val start = new CountDownLatch(1)
          val roots = (0 until 4).map(i => root(round * 4 + i))
          val cutoff = clock.get()
          val tasks = for r <- roots; v <- 0 until 16 yield pool.submit((() => {
            start.await()
            store.put(r, v, Array[Byte](v.toByte))
          }): Runnable)
          val expiries = (0 until 4).map(_ => pool.submit((() => { start.await(); store.deleteStoredBefore(cutoff) }): java.util.concurrent.Callable[Int]))
          start.countDown()
          tasks.foreach(_.get(30, TimeUnit.SECONDS))
          expiries.foreach(_.get(30, TimeUnit.SECONDS))
          roots.foreach { r =>
            withClue(s"round $round: ") {
              store.timeEntriesForTesting(r).size shouldBe 1
              store.heldIndexes(r) shouldBe (0 until 16).toList
            }
          }
          store.deleteStoredBefore(cutoff) shouldBe 0
          roots.foreach(r => store.heldIndexes(r).size shouldBe 16)
      finally pool.shutdownNow()
    }
  }
