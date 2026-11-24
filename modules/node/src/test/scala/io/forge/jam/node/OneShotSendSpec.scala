package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, ConcurrentLinkedQueue, CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import io.forge.jam.network.MessageStream
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

  test("a one-shot stream is told that nothing the peer answers is wanted, before anything is sent on it") {
    val egress = singleThread("jam-egress")
    try
      val calls = new ConcurrentLinkedQueue[String]()
      val stream = new MessageStream:
        def kind: Byte = 1
        def onMessage(f: Array[Byte] => Unit): MessageStream = this
        override def ignoreInbound(): Unit = calls.add("ignoreInbound"): Unit
        def onClosed(f: () => Unit): MessageStream = this
        def send(message: Array[Byte]): Unit = calls.add(s"send ${message.length}"): Unit
        def finish(): Unit = calls.add("finish"): Unit
        def close(): Unit = ()
        def reset(): Unit = calls.add("reset"): Unit
        def isOpen: Boolean = true
        def id: Long = 0L
        def pendingOutboundBytes: Long = 0L
      val settled = new CompletableFuture[Boolean]()
      OneShotSend.submit(
        egress,
        null,
        () => CompletableFuture.completedFuture[MessageStream](stream),
        List(Array[Byte](1, 2), Array[Byte](3)),
        _ => (),
        settled = ok => settled.complete(ok): Unit
      )
      settled.get(10, TimeUnit.SECONDS) shouldBe true
      calls.toArray.toList shouldBe List("ignoreInbound", "send 2", "send 1", "finish")
    finally egress.shutdownNow()
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
        OneShotSend.submit(egress, stuck, () => new CompletableFuture[MessageStream](), List(Array[Byte](1)), _ => ())
      OneShotSend.submit(
        egress,
        other,
        () =>
          reached.countDown()
          new CompletableFuture[MessageStream](),
        List(Array[Byte](2)),
        _ => ()
      )
      // Waiting for each open in turn, the last send would start after 20 × 10 s.
      reached.await(2, TimeUnit.SECONDS) shouldBe true
      (System.nanoTime() - started) / 1000000 should be < 2000L
      OneShotSend.waitingOn(stuck) shouldBe 20
    finally egress.shutdownNow()
  }

  test("a send dropped from a full egress queue is settled as not sent, at once") {
    val egress = WorkLimits.dropOldestExecutor("egress-spec", threads = 1, capacity = 1, r => {
      val t = new Thread(r, "egress-spec"); t.setDaemon(true); t
    })
    try
      val holding = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      egress.execute(() => { holding.countDown(); release.await() })
      holding.await(5, TimeUnit.SECONDS) shouldBe true
      val settled = new ConcurrentLinkedQueue[(Int, Boolean)]()
      val dropped = new ConcurrentLinkedQueue[Integer]()
      def send(i: Int): Unit =
        OneShotSend.submit(
          egress,
          new Object,
          () => CompletableFuture.failedFuture[MessageStream](new RuntimeException("no peer")),
          List(Array[Byte](1)),
          _ => (),
          settled = ok => { settled.add((i, ok)); () },
          dropped = () => { dropped.add(i); () }
        )
      send(1) // waits in the queue
      send(2) // takes its place
      settled.toArray.toList shouldBe List((1, false))
      dropped.toArray.toList shouldBe List(1)
      egress.dropped shouldBe 1L
      release.countDown()
      eventually("the kept send to run")(settled.size == 2)
      settled.toArray.toList shouldBe List((1, false), (2, false))
      dropped.size shouldBe 1
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
            new CompletableFuture[MessageStream](),
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
      val failed = new CompletableFuture[MessageStream]()
      failed.completeExceptionally(new RuntimeException("refused"))
      val why = new ConcurrentLinkedQueue[String]()
      OneShotSend.submit(egress, stuck, () => failed, List(Array[Byte](1)), why.add(_))
      eventually("the failure to be reported")(why.size == 1)
      why.peek() should include("refused")
      OneShotSend.waitingOn(stuck) shouldBe 0
    finally egress.shutdownNow()
  }

  test("submitEach sends every message on its own stream as one queue entry, past the per-peer cap, and stops at a failure") {
    val egress = singleThread("jam-egress")
    try
      val streams = new AtomicInteger()
      val received = new ConcurrentLinkedQueue[Int]()
      val record: io.forge.jam.network.PeerStreamHandler = (_, stream) =>
        streams.incrementAndGet()
        stream.onMessage(m => received.add(m.length): Unit)
      val (toPeer, _) = io.forge.jam.network.Loopback.pair(Array.fill[Byte](32)(1), Array.fill[Byte](32)(2), _ => None, _ => Some(record))
      val queuedBefore = new AtomicInteger()
      val counting = new java.util.concurrent.AbstractExecutorService:
        def execute(r: Runnable): Unit = { queuedBefore.incrementAndGet(); egress.execute(r) }
        def shutdown(): Unit = ()
        def shutdownNow(): java.util.List[Runnable] = java.util.List.of()
        def isShutdown: Boolean = false
        def isTerminated: Boolean = false
        def awaitTermination(t: Long, u: TimeUnit): Boolean = true
      val count = 3 * OneShotSend.MaxWaitingPerPeer
      val failures = new ConcurrentLinkedQueue[String]()
      val messages = (0 until count).map(i => Array[Byte](i.toByte, (i >> 8).toByte))
      OneShotSend.submitEach(counting, toPeer, () => toPeer.openStream(1.toByte), messages, failures.add(_))
      queuedBefore.get shouldBe 1
      eventually("every message to arrive")(received.size == count)
      streams.get shouldBe count
      failures shouldBe empty
      OneShotSend.waitingOn(toPeer) shouldBe 0

      val opens = new AtomicInteger()
      val refused = new CompletableFuture[MessageStream]()
      refused.completeExceptionally(new RuntimeException("refused"))
      OneShotSend.submitEach(egress, toPeer, () => { opens.incrementAndGet(); refused }, messages, failures.add(_))
      eventually("the failure to be reported")(failures.size == 2)
      opens.get shouldBe 1
      failures.peek() should include("refused")
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
