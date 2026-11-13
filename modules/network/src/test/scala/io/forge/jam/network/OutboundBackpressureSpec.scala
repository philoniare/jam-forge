package io.forge.jam.network

import java.net.InetSocketAddress
import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue, TimeUnit}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** A peer that stops reading must not make the sender queue its messages without bound: the bytes waiting for it
  * stay under [[OutboundLimits]], its streams are reset and the connection is closed once it has taken nothing for
  * the stall limit. A peer that keeps reading, on the same node, is not affected.
  */
class OutboundBackpressureSpec extends AnyFunSuite with Matchers:

  private val MiB = 1024 * 1024

  /** Small limits so the test needs little memory and time; the mechanism is the same as with the defaults. */
  private val limits = OutboundLimits(
    maxStreamPendingBytes = 4L * MiB,
    maxConnectionPendingBytes = 8L * MiB,
    sendWaitMillis = 300,
    upHighWaterBytes = 256 * 1024,
    stallMillis = 5000
  )
  private val clientConfig = JamnpConfig(genesisHashPrefix = "deadbeef")
  private val serverConfig = clientConfig.copy(outbound = limits)

  /** Each response message is 1 MiB; a CE 128 request asks for `n` of them (its first byte). */
  private val Chunk = MiB

  private def eventually(what: String, seconds: Int = 20, pollMillis: Long = 20)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(pollMillis)
    if !cond then fail(s"timed out waiting for: $what")

  private def withServer(test: (JamnpNode, LinkedBlockingQueue[JamnpConnection]) => Unit): Unit =
    val server = new JamnpNode(NodeIdentity.generate(), serverConfig)
    val accepted = new LinkedBlockingQueue[JamnpConnection]()
    server.onConnectionChange((c, up) => if up then accepted.offer(c))
    // CE 128 stand-in: answers on the event loop with `n` chunks, as the node's responders do.
    server.registerHandler(
      StreamKind.BlockRequest,
      (_: JamnpConnection, stream: JamnpStream) =>
        stream.onMessage { request =>
          for _ <- 1 to request(0).toInt do stream.send(new Array[Byte](Chunk))
          stream.finish()
        }
        ()
    )
    try
      server.start(new InetSocketAddress("127.0.0.1", 0))
      test(server, accepted)
    finally server.shutdown()

  private def connect(server: JamnpNode, accepted: LinkedBlockingQueue[JamnpConnection]): (JamnpNode, JamnpConnection, JamnpConnection) =
    val node = new JamnpNode(NodeIdentity.generate(), clientConfig)
    val conn = node.connect(new InetSocketAddress("127.0.0.1", server.boundPort)).get(10, TimeUnit.SECONDS)
    val serverSide = accepted.poll(10, TimeUnit.SECONDS)
    serverSide should not be null
    (node, conn, serverSide)

  /** One request for `chunks` chunks; the number of response bytes received. */
  private def fetch(conn: JamnpConnection, chunks: Int): Long =
    val closed = new LinkedBlockingQueue[java.lang.Boolean]()
    val received = new java.util.concurrent.atomic.AtomicLong()
    val stream = conn.openStream(StreamKind.BlockRequest).get(10, TimeUnit.SECONDS)
    stream.onMessage(m => { received.addAndGet(m.length.toLong); () })
    stream.onClosed(() => { closed.offer(true); () })
    stream.send(Array[Byte](chunks.toByte))
    stream.finish()
    if closed.poll(20, TimeUnit.SECONDS) == null then fail("no response to a normal request")
    received.get()

  test("request/response: a peer that stops reading holds a bounded number of bytes and is dropped; a healthy peer is served") {
    withServer { (server, accepted) =>
      val (healthyNode, healthy, _) = connect(server, accepted)
      val (stalledNode, stalled, stalledServerSide) = connect(server, accepted)
      val stop = new java.util.concurrent.CountDownLatch(1)
      try
        fetch(healthy, 1) shouldBe Chunk.toLong

        // The stalled peer asks for 1 MiB on each of 30 streams, then stops: its event loop is blocked, so it
        // neither reads nor acknowledges anything more.
        val parked = new ConcurrentLinkedQueue[JamnpStream]()
        server.registerHandler(
          StreamKind.StateRequest,
          (_: JamnpConnection, stream: JamnpStream) => stream.onMessage(_ => { parked.add(stream); () })
        )
        for _ <- 1 to 30 do
          val s = stalled.openStream(StreamKind.StateRequest).get(10, TimeUnit.SECONDS)
          s.send(Array[Byte](1))
        eventually("all requests to arrive")(parked.size == 30)
        val stopped = new java.util.concurrent.CountDownLatch(1)
        stalled.quic.eventLoop().execute { () => stopped.countDown(); stop.await() }
        stopped.await(5, TimeUnit.SECONDS) shouldBe true

        // The responses are sent from the event loop, as the node's responders send theirs.
        var peak = 0L
        def sample(): Unit = peak = math.max(peak, stalledServerSide.pendingOutboundBytes)
        parked.forEach { stream =>
          stream.channel.eventLoop().execute { () =>
            stream.send(new Array[Byte](Chunk))
            stream.finish()
          }
          sample()
        }
        val until = System.currentTimeMillis() + 1000
        while System.currentTimeMillis() < until do
          sample()
          Thread.sleep(5)

        // Never more than the connection limit, plus the one message a stream may always send; 30 MiB without it.
        peak should be <= limits.maxConnectionPendingBytes + Chunk + 4
        // What was accepted is still waiting for the peer, and the responses over the limit were refused by reset.
        stalledServerSide.pendingOutboundBytes should be > 0L
        eventually("refused responses to be reset")(parked.stream().filter(!_.isOpen).count() >= 20)

        // The healthy peer on the same node is served throughout, including a response that fills its stream limit.
        fetch(healthy, 2) shouldBe 2L * Chunk
        fetch(healthy, 1) shouldBe Chunk.toLong

        // The stalled peer has taken nothing for the stall limit: the next send to it closes the connection, and
        // nothing stays queued for it.
        Thread.sleep(limits.stallMillis + 500)
        stalledServerSide.isOpen shouldBe true
        stalledServerSide.openStream(StreamKind.AssuranceDistribution).get(10, TimeUnit.SECONDS).send(Array[Byte](1))
        eventually("the stalled connection to be closed")(!stalledServerSide.isOpen)
        eventually("its queued bytes to be released")(stalledServerSide.pendingOutboundBytes == 0)

        fetch(healthy, 1) shouldBe Chunk.toLong
      finally
        stop.countDown()
        healthyNode.shutdown()
        stalledNode.shutdown()
    }
  }

  test("a send from an ordinary thread waits for room, then resets the stream") {
    withServer { (server, accepted) =>
      val (node, conn, serverSide) = connect(server, accepted)
      try
        val opened = new LinkedBlockingQueue[JamnpStream]()
        server.registerHandler(
          StreamKind.StateRequest,
          (_: JamnpConnection, stream: JamnpStream) => { opened.offer(stream); () }
        )
        val s = conn.openStream(StreamKind.StateRequest).get(10, TimeUnit.SECONDS)
        s.channel.config().setAutoRead(false)
        s.send(Array[Byte](0))
        val out = opened.poll(10, TimeUnit.SECONDS)
        out should not be null

        // Up to 100 x 1 MiB from this (ordinary) thread to a peer that reads nothing: a send waits while earlier
        // ones drain, and once the peer's flow-control credit (32 MiB per stream) is used up it waits
        // `sendWaitMillis` for room that does not come, and the stream is reset.
        var peak = 0L
        var i = 0
        val t0 = System.nanoTime()
        while i < 100 && out.isOpen do
          out.send(new Array[Byte](Chunk))
          peak = math.max(peak, serverSide.pendingOutboundBytes)
          i += 1
        val elapsedMs = (System.nanoTime() - t0) / 1000000

        peak should be <= limits.maxStreamPendingBytes + 4
        eventually("the stream to be reset")(!out.isOpen)
        elapsedMs should be >= limits.sendWaitMillis
        eventually("its queued bytes to be released")(serverSide.pendingOutboundBytes == 0)
        // The connection itself is fine: only that stream was given up.
        serverSide.isOpen shouldBe true
        fetch(conn, 1) shouldBe Chunk.toLong
      finally node.shutdown()
    }
  }

  test("UP stream: announcements to a peer that stops reading are dropped, then the peer is disconnected") {
    withServer { (server, accepted) =>
      val opened = new LinkedBlockingQueue[JamnpStream]()
      server.registerHandler(
        StreamKind.BlockAnnouncement,
        (_: JamnpConnection, stream: JamnpStream) => { opened.offer(stream); () }
      )
      val (healthyNode, healthy, healthyServerSide) = connect(server, accepted)
      val (stalledNode, stalled, stalledServerSide) = connect(server, accepted)
      try
        val got = new java.util.concurrent.atomic.AtomicInteger()
        val healthyUp = healthy.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
        healthyUp.onMessage(_ => { got.incrementAndGet(); () })
        healthyUp.send(Array[Byte](0))
        val toHealthy = opened.poll(10, TimeUnit.SECONDS)
        val stalledUp = stalled.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
        stalledUp.channel.config().setAutoRead(false)
        stalledUp.send(Array[Byte](0))
        val toStalled = opened.poll(10, TimeUnit.SECONDS)
        toHealthy should not be null
        toStalled should not be null

        // 64 KiB announcements to both until the stalled peer is dropped: well past the 32 MiB of stream credit it
        // granted. Each is sent once the healthy peer has taken the one before, as fast as the link allows.
        val announcement = new Array[Byte](64 * 1024)
        var peak = 0L
        var sent = 0
        val deadline = System.currentTimeMillis() + 60000
        while stalledServerSide.isOpen && System.currentTimeMillis() < deadline do
          toHealthy.send(announcement)
          toStalled.send(announcement)
          sent += 1
          peak = math.max(peak, stalledServerSide.pendingOutboundBytes)
          // Polled tightly: the loop runs over 500 times, and a coarse poll is most of its time.
          eventually("the healthy peer to take an announcement", pollMillis = 1)(toHealthy.pendingOutboundBytes == 0)

        peak should be <= limits.upHighWaterBytes + announcement.length + 4
        stalledServerSide.droppedOutboundMessages should be > 0L
        stalledServerSide.isOpen shouldBe false

        // The healthy peer got every announcement and none was dropped for it.
        healthyServerSide.isOpen shouldBe true
        healthyServerSide.droppedOutboundMessages shouldBe 0L
        eventually("the healthy peer to receive all announcements")(got.get() == sent)
      finally
        healthyNode.shutdown()
        stalledNode.shutdown()
    }
  }
