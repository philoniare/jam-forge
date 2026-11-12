package io.forge.jam.pvm.memory

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.funsuite.AnyFunSuite

class InstanceMemoryBudgetSpec extends AnyFunSuite:

  private def thread(body: => Unit): Thread =
    val t = new Thread(() => body)
    t.setDaemon(true)
    t.start()
    t

  /** Waits until `n` runs are queued on the budget: the state the next step of a test depends on, not a delay. */
  private def awaitWaiting(budget: InstanceMemoryBudget, n: Int): Unit =
    val deadline = System.nanoTime() + 30_000_000_000L
    while budget.waitingCount != n && System.nanoTime() < deadline do Thread.onSpinWait()
    assert(budget.waitingCount == n, s"expected $n waiting run(s)")

  private def joined(ts: Thread*): Boolean =
    ts.foreach(_.join(10000))
    ts.forall(!_.isAlive)

  test("reservations within the budget do not wait; one that does not fit waits for a release") {
    val budget = new InstanceMemoryBudget(100)
    val a = budget.reserve(60)
    val b = budget.reserve(40)
    assert(budget.inUseBytes == 100)

    val entered = new CountDownLatch(1)
    val t = thread { val c = budget.reserve(30); entered.countDown(); c.close() }
    awaitWaiting(budget, 1)
    assert(entered.getCount == 1, "a reservation over the budget must wait")
    assert(budget.inUseBytes == 100)
    b.close()
    assert(entered.await(5, TimeUnit.SECONDS))
    a.close()
    assert(joined(t))
    assert(budget.inUseBytes == 0)
    assert(budget.peakBytes == 100)
  }

  test("a reservation larger than the whole budget runs, alone") {
    val budget = new InstanceMemoryBudget(100)
    val small = budget.reserve(10)
    val entered = new CountDownLatch(1)
    val done = new CountDownLatch(1)
    val t = thread { val big = budget.reserve(500); entered.countDown(); done.await(); big.close() }
    awaitWaiting(budget, 1)
    assert(entered.getCount == 1, "it must wait until nothing else is reserved")
    small.close()
    assert(entered.await(5, TimeUnit.SECONDS))
    assert(budget.inUseBytes == 500)

    // Nothing else starts while it runs.
    val other = new CountDownLatch(1)
    val o = thread { val r = budget.reserve(1); other.countDown(); r.close() }
    awaitWaiting(budget, 1)
    assert(other.getCount == 1)
    assert(budget.inUseBytes == 500)
    done.countDown()
    assert(other.await(5, TimeUnit.SECONDS))
    assert(joined(t, o))
    assert(budget.inUseBytes == 0)
  }

  test("waiting runs start in arrival order: a later, smaller one never overtakes an earlier one") {
    val budget = new InstanceMemoryBudget(100)
    val holder = budget.reserve(50)

    // `large` (90) arrives first and cannot start; `small` (20) arrives second and would fit next to the holder.
    val largeIn = new CountDownLatch(1)
    val largeOut = new CountDownLatch(1)
    val smallIn = new CountDownLatch(1)
    val large = thread { val r = budget.reserve(90); largeIn.countDown(); largeOut.await(); r.close() }
    awaitWaiting(budget, 1)
    val small = thread { val r = budget.reserve(20); smallIn.countDown(); r.close() }
    awaitWaiting(budget, 2)
    // It queues behind `large` although 50 + 20 fits: otherwise a stream of small runs could starve a large one.
    assert(smallIn.getCount == 1 && largeIn.getCount == 1)
    assert(budget.inUseBytes == 50)

    holder.close()
    assert(largeIn.await(5, TimeUnit.SECONDS))
    // `large` runs; `small` is still waiting behind it (90 + 20 does not fit), so it did not go first.
    awaitWaiting(budget, 1)
    assert(smallIn.getCount == 1)
    assert(budget.inUseBytes == 90)

    largeOut.countDown()
    assert(smallIn.await(5, TimeUnit.SECONDS))
    assert(joined(large, small))
    assert(budget.inUseBytes == 0)
  }

  test("a large run is not starved by small ones that keep arriving") {
    val budget = new InstanceMemoryBudget(100)
    val holder = budget.reserve(30)
    val largeIn = new CountDownLatch(1)
    val large = thread { val r = budget.reserve(100); largeIn.countDown(); r.close() }
    awaitWaiting(budget, 1)

    // 200 small runs arrive one after another while `large` waits; each would fit beside the holder.
    val started = new AtomicInteger()
    val smalls = (1 to 200).map { i =>
      val t = thread { val r = budget.reserve(10); started.incrementAndGet(); r.close() }
      awaitWaiting(budget, 1 + i)
      t
    }
    assert(started.get() == 0, "none may start ahead of the large run")
    assert(largeIn.getCount == 1)

    holder.close()
    assert(largeIn.await(5, TimeUnit.SECONDS))
    assert(joined((large +: smalls)*))
    assert(started.get() == 200)
    assert(budget.inUseBytes == 0)
  }

  test("runs that all wait to grow do not deadlock: they go ahead one at a time") {
    val budget = new InstanceMemoryBudget(100)
    val bothHold = new CountDownLatch(2)
    val finished = new AtomicInteger()
    def run(): Thread = thread {
      val r = budget.reserve(40)
      bothHold.countDown()
      bothHold.await()
      r.grow(50) // 80 + 50 > 100 for either of them
      r.close()
      finished.incrementAndGet()
    }
    val a = run()
    val b = run()
    assert(joined(a, b), "both runs must finish")
    assert(finished.get() == 2)
    assert(budget.inUseBytes == 0)
    // Over the budget by at most the one run that was let through.
    assert(budget.peakBytes <= 100 + 50)
  }

  test("only one run at a time is over the budget, however often the runs grow") {
    val budget = new InstanceMemoryBudget(100)
    val allHold = new CountDownLatch(4)
    val runs = (0 until 4).map { _ =>
      thread {
        val r = budget.reserve(25)
        allHold.countDown()
        allHold.await()
        for _ <- 0 until 5 do
          r.grow(50)
          Thread.`yield`()
        r.close()
      }
    }
    assert(joined(runs*), "all runs must finish")
    assert(budget.inUseBytes == 0)
    // The budget plus what one run grew by; four runs grown in turn would reach 1100.
    assert(budget.peakBytes <= 100 + 5 * 50)
  }

  test("a single run may grow past the budget; a replaced buffer is given back") {
    val budget = new InstanceMemoryBudget(100)
    val r = budget.reserve(80)
    r.grow(300)
    assert(budget.inUseBytes == 380)
    r.shrink(80)
    assert(budget.inUseBytes == 300)
    r.close()
    assert(budget.inUseBytes == 0)
    r.close() // closing twice gives nothing back twice
    assert(budget.inUseBytes == 0)
  }

  test("a nested reservation on a thread that already holds one never waits") {
    val budget = new InstanceMemoryBudget(100)
    val outer = budget.reserve(100)
    val inner = budget.reserve(100)
    inner.grow(100)
    assert(budget.inUseBytes == 300)
    inner.close()
    // The outer reservation is the thread's current one again.
    val again = budget.reserve(100)
    again.close()
    outer.close()
    assert(budget.inUseBytes == 0)
    // With nothing held, the thread's next reservation is a top-level one and is counted as such.
    val top = budget.reserve(100)
    val entered = new CountDownLatch(1)
    val t = thread { val r = budget.reserve(1); entered.countDown(); r.close() }
    awaitWaiting(budget, 1)
    assert(entered.getCount == 1)
    top.close()
    assert(joined(t))
  }

  test("many threads reserving, growing and releasing all finish and leave nothing reserved") {
    val budget = new InstanceMemoryBudget(1000)
    val pool = Executors.newFixedThreadPool(16)
    try
      val futures = (0 until 16).map { t =>
        pool.submit[Unit] { () =>
          val rnd = new java.util.Random(t)
          for _ <- 0 until 2000 do
            val r = budget.reserve(rnd.nextInt(700).toLong)
            if rnd.nextInt(4) == 0 then r.grow(rnd.nextInt(900).toLong)
            if rnd.nextInt(8) == 0 then r.shrink(rnd.nextInt(100).toLong)
            if rnd.nextInt(16) == 0 then Thread.`yield`()
            r.close()
          ()
        }
      }
      futures.foreach(_.get(60, TimeUnit.SECONDS))
      assert(budget.inUseBytes == 0)
      // The budget plus one run let through while every run was waiting to grow.
      assert(budget.peakBytes <= 1000 + 700 + 900)
    finally pool.shutdownNow()
  }
