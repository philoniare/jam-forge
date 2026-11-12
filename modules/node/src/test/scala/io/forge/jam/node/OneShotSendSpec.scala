package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, ConcurrentLinkedQueue, CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import io.forge.jam.network.JamnpStream
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** One-shot sends do not wait on a peer on the egress thread, and inbound checks do not run on the caller's. */
class OneShotSendSpec extends AnyFunSuite with Matchers:

  private def eventually(what: String, seconds: Int = 10)(cond: => Boolean): Unit =
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(seconds.toLong)
    while !cond && System.nanoTime() < deadline do Thread.sleep(10)
    assert(cond, s"timed out waiting for $what")

  private def singleThread(name: String) =
    Executors.newSingleThreadExecutor { r =>
      val t = new Thread(r, name); t.setDaemon(true); t
    }

  test("a peer that grants no stream does not hold up the sends behind it") {
    val egress = singleThread("jam-egress")
    try
      val stuck = new Object
      val other = new Object
      val reached = new CountDownLatch(1)
      val started = System.nanoTime()
      // Twenty sends to a peer that never grants a stream, then one to another peer.
      for _ <- 1 to 20 do
        OneShotSend.submit(egress, stuck, () => new CompletableFuture[JamnpStream](), List(Array[Byte](1)), _ => ())
      OneShotSend.submit(
        egress,
        other,
        () =>
          reached.countDown()
          new CompletableFuture[JamnpStream](),
        List(Array[Byte](2)),
        _ => ()
      )
      // Waiting for each open in turn, the last send would start after 20 × 10 s.
      reached.await(2, TimeUnit.SECONDS) shouldBe true
      (System.nanoTime() - started) / 1000000 should be < 2000L
      OneShotSend.waitingOn(stuck) shouldBe 20
    finally egress.shutdownNow()
  }

  test("sends waiting on one peer are capped, the rest dropped, and the count returns to zero when they time out") {
    val egress = singleThread("jam-egress")
    try
      val stuck = new Object
      val opens = new AtomicInteger()
      val failures = new ConcurrentLinkedQueue[String]()
      for _ <- 1 to 100 do
        OneShotSend.submit(
          egress,
          stuck,
          () =>
            opens.incrementAndGet()
            new CompletableFuture[JamnpStream](),
          List(Array[Byte](1)),
          failures.add(_),
          openTimeoutMillis = 500
        )
      eventually("all sends to be started or dropped")(opens.get() + failures.size >= 100)
      opens.get() shouldBe OneShotSend.MaxWaitingPerPeer
      failures.size shouldBe 100 - OneShotSend.MaxWaitingPerPeer
      OneShotSend.waitingOn(stuck) shouldBe OneShotSend.MaxWaitingPerPeer
      eventually("the waiting sends to time out")(OneShotSend.waitingOn(stuck) == 0)
      failures.size shouldBe 100

      // A failed open releases its place at once.
      val failed = new CompletableFuture[JamnpStream]()
      failed.completeExceptionally(new RuntimeException("refused"))
      val why = new ConcurrentLinkedQueue[String]()
      OneShotSend.submit(egress, stuck, () => failed, List(Array[Byte](1)), why.add(_))
      eventually("the failure to be reported")(why.size == 1)
      why.peek() should include("refused")
      OneShotSend.waitingOn(stuck) shouldBe 0
    finally egress.shutdownNow()
  }

  test("inbound work runs off the caller's thread and is refused when too much waits") {
    val inbound = InboundWork.pooled()
    try
      val release = new CountDownLatch(1)
      val threads = new ConcurrentLinkedQueue[String]()
      val ran = new AtomicInteger()
      val started = System.nanoTime()
      var admitted = 0
      var refused = 0
      for _ <- 1 to InboundWork.MaxQueuedMessages + 500 do
        val ok = inbound.submit(null, 100L) {
          threads.add(Thread.currentThread().getName)
          release.await()
          ran.incrementAndGet()
          ()
        }
        if ok then admitted += 1 else refused += 1
      // Submitting never waited on the work.
      (System.nanoTime() - started) / 1000000 should be < 5000L
      refused should be > 0
      admitted should be <= InboundWork.MaxQueuedMessages + InboundWork.Threads
      release.countDown()
      eventually("the admitted work to run")(ran.get() == admitted)
      threads.forEach(name => name should startWith("jam-inbound-"))

      // Without a pool the work runs where it is handed over.
      var here = ""
      InboundWork.direct.submit(null, 1L) { here = Thread.currentThread().getName } shouldBe true
      here shouldBe Thread.currentThread().getName
    finally inbound.shutdown()
  }
