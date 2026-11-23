package io.forge.jam.network

import java.net.InetSocketAddress
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, Semaphore, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import io.netty.channel.{ChannelFuture, ChannelFutureListener, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.incubator.codec.quic.{QuicStreamChannel, QuicStreamType}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Streams that end without both sides finishing them must not
  * stay open, or the connection runs out of QUIC stream credit (256 per
  * direction) and refuses every later stream.
  */
class StreamLeakSpec extends AnyFunSuite with Matchers:

  private val ReleaseSeconds = 60

  private val config =
    JamnpConfig(genesisHashPrefix = "deadbeef", maxIdleTimeoutMillis = 4 * ReleaseSeconds * 1000L)

  /** More than the 256 concurrent streams a connection allows: a leak of one stream per round exhausts them. */
  private val Rounds = 300

  /** Streams held open at once: under the limit, so two batches exceed it. */
  private val Batch = 200

  /** A responder that never answers and never finishes. */
  private val silent: StreamHandler = (_: JamnpConnection, stream: JamnpStream) =>
    stream.onMessage(_ => ())
    ()

  private def eventually(what: String, seconds: Int = 15)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(20)
    if !cond then fail(s"timed out waiting for: $what")

  private def await(what: String, detail: => String)(done: Long => Boolean): Unit =
    if !done(ReleaseSeconds.toLong) then fail(s"timed out after $ReleaseSeconds s waiting for: $what ($detail)")

  private def stillConnected(conns: JamnpConnection*): Unit =
    withClue("connection closed: ")(conns.foreach(_.isOpen shouldBe true))

  private def releasedOnPeerStreams(conn: JamnpConnection): Semaphore =
    val released = new Semaphore(0)
    val counter = new ChannelInboundHandlerAdapter:
      override def channelRead(ctx: ChannelHandlerContext, msg: Object): Unit =
        msg match
          case ch: QuicStreamChannel =>
            ch.closeFuture()
              .addListener(new ChannelFutureListener {
                override def operationComplete(f: ChannelFuture): Unit = released.release()
              })
            ()
          case _ => ()
        ctx.fireChannelRead(msg)
    conn.quic
      .eventLoop()
      .submit(new Runnable { def run(): Unit = { conn.quic.pipeline().addFirst(counter); () } })
      .get(10, TimeUnit.SECONDS)
    released

  private def openWithCredit(conn: JamnpConnection, kind: Byte): JamnpStream =
    eventually("stream credit from the peer", ReleaseSeconds)(
      conn.quic.peerAllowedStreams(QuicStreamType.BIDIRECTIONAL) > 0
    )
    conn.openStream(kind).get(10, TimeUnit.SECONDS)

  private final class Relay(server: InetSocketAddress) extends AutoCloseable:
    private val front = new java.net.DatagramSocket(new InetSocketAddress("127.0.0.1", 0))
    private val back = new java.net.DatagramSocket(new InetSocketAddress("127.0.0.1", 0))
    @volatile private var client: java.net.SocketAddress = null

    val dropFromClient = new AtomicInteger()

    def address: InetSocketAddress = new InetSocketAddress("127.0.0.1", front.getLocalPort)

    private def pump(from: java.net.DatagramSocket)(forward: java.net.DatagramPacket => Unit): Unit =
      val t = new Thread(() =>
        val packet = new java.net.DatagramPacket(new Array[Byte](65535), 65535)
        try
          while true do
            packet.setLength(65535)
            from.receive(packet)
            forward(packet)
        catch case _: java.io.IOException => () // closed
      )
      t.setDaemon(true)
      t.start()

    pump(front) { p =>
      client = p.getSocketAddress
      if dropFromClient.getAndUpdate(n => math.max(0, n - 1)) == 0 then
        back.send(new java.net.DatagramPacket(p.getData, p.getLength, server))
    }
    pump(back) { p =>
      val to = client
      if to != null then front.send(new java.net.DatagramPacket(p.getData, p.getLength, to))
    }

    def close(): Unit =
      front.close()
      back.close()

  /** A CE 128-style echo request on `conn`; the response text. */
  private def echo(conn: JamnpConnection, text: String): String =
    val responses = new LinkedBlockingQueue[Array[Byte]]()
    val stream = conn.openStream(StreamKind.BlockRequest).get(10, TimeUnit.SECONDS)
    stream.onMessage(responses.offer(_))
    stream.send(text.getBytes("UTF-8"))
    stream.finish()
    val response = responses.poll(10, TimeUnit.SECONDS)
    if response == null then fail("no response to a normal request")
    new String(response, "UTF-8")

  private def withPair(
      test: (JamnpNode, JamnpConnection, () => JamnpConnection) => Unit,
      serverConfig: JamnpConfig = config
  ): Unit =
    val serverNode = new JamnpNode(NodeIdentity.generate(), serverConfig)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    val accepted = new LinkedBlockingQueue[JamnpConnection]()
    serverNode.onConnectionChange((c, up) => if up then accepted.offer(c))
    serverNode.registerHandler(
      StreamKind.BlockRequest,
      (_: JamnpConnection, stream: JamnpStream) =>
        stream.onMessage { request =>
          stream.send(request)
          stream.finish()
        }
    )
    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)
      val serverSide = accepted.poll(10, TimeUnit.SECONDS)
      serverSide should not be null
      test(serverNode, conn, () => serverSide)
    finally
      clientNode.shutdown()
      serverNode.shutdown()

  test("streams of an unhandled kind do not use up the connection's stream credit") {
    withPair { (_, conn, serverSide) =>
      for i <- 1 to Rounds do
        val closed = new LinkedBlockingQueue[java.lang.Boolean]()
        val stream =
          try conn.openStream(StreamKind.JudgmentPublication).get(10, TimeUnit.SECONDS)
          catch
            case e: Exception =>
              fail(
                s"stream $i of an unhandled kind could not be opened (open streams: client " +
                  s"${conn.openStreamCount}, server ${serverSide().openStreamCount}): $e"
              )
        stream.onClosed(() => closed.offer(true))
        // A one-shot send, as the node's distribution helpers do it.
        stream.send("hello".getBytes("UTF-8"))
        stream.finish()
        if closed.poll(10, TimeUnit.SECONDS) == null then fail(s"stream $i was never rejected")

      echo(conn, "still-alive") shouldBe "still-alive"
      eventually("client streams to close")(conn.openStreamCount == 0)
      eventually("server streams to close")(serverSide().openStreamCount == 0)
    }
  }

  test("one-shot sends answered with a FIN leave no stream open") {
    withPair { (serverNode, conn, serverSide) =>
      serverNode.registerHandler(
        StreamKind.AssuranceDistribution,
        (_: JamnpConnection, stream: JamnpStream) => stream.onMessage(_ => stream.finish())
      )
      for i <- 1 to Rounds do
        val stream =
          try conn.openStream(StreamKind.AssuranceDistribution).get(10, TimeUnit.SECONDS)
          catch case e: Exception => fail(s"one-shot stream $i could not be opened: $e")
        val closed = new LinkedBlockingQueue[java.lang.Boolean]()
        stream.onClosed(() => closed.offer(true))
        stream.send("item".getBytes("UTF-8"))
        stream.finish()
        if closed.poll(10, TimeUnit.SECONDS) == null then fail(s"one-shot stream $i was never finished")

      echo(conn, "still-alive") shouldBe "still-alive"
      eventually("client streams to close")(conn.openStreamCount == 0)
      eventually("server streams to close")(serverSide().openStreamCount == 0)
    }
  }

  test("a request the initiator gives up on is released on both sides") {
    withPair(
      { (serverNode, conn, serverSide) =>
        // A responder that never answers and never finishes.
        serverNode.registerHandler(StreamKind.BundleRequest, silent)
        for i <- 1 to Rounds do
          val stream =
            try conn.openStream(StreamKind.BundleRequest).get(10, TimeUnit.SECONDS)
            catch
              case e: Exception =>
                fail(
                  s"request stream $i could not be opened (open streams: client ${conn.openStreamCount}, " +
                    s"server ${serverSide().openStreamCount}): $e"
                )
          stream.send("request".getBytes("UTF-8"))
          stream.finish()
          // What a requester does when its wait times out.
          stream.reset()
          // A reset that follows the initiator's FIN returns the credit at
          // once, but the responder's channel only goes when it writes or
          // idles; let it drain so the rounds do not pile up.
          if i % 100 == 0 then eventually("server streams to close")(serverSide().openStreamCount == 0)

        echo(conn, "still-alive") shouldBe "still-alive"
        eventually("client streams to close")(conn.openStreamCount == 0)
        eventually("server streams to close")(serverSide().openStreamCount == 0)
      },
      serverConfig = config.copy(streamIdleTimeoutMillis = 500)
    )
  }

  test("a CE stream left idle by its opener is reset by the accepting side") {
    withPair(
      { (serverNode, conn, serverSide) =>
        serverNode.registerHandler(StreamKind.BundleRequest, silent)
        val closed = new LinkedBlockingQueue[java.lang.Boolean]()
        // Two batches, each under the limit, together over it.
        for _ <- 1 to 2 do
          for i <- 1 to Batch do
            val stream = openWithCredit(conn, StreamKind.BundleRequest)
            stream.onClosed(() => closed.offer(true))
            // Half send a request and go quiet; half never send a message.
            if i % 2 == 0 then stream.send("request".getBytes("UTF-8"))
          eventually("server streams to be reset")(serverSide().openStreamCount == 0)
          eventually("client streams to see the reset")(conn.openStreamCount == 0)
        closed.size shouldBe 2 * Batch
        echo(conn, "still-alive") shouldBe "still-alive"
      },
      serverConfig = config.copy(streamIdleTimeoutMillis = 300)
    )
  }

  test("a CE stream the responder never answers is reset by the opening side") {
    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config.copy(streamIdleTimeoutMillis = 300))
    val accepted = new LinkedBlockingQueue[JamnpConnection]()
    serverNode.onConnectionChange((c, up) => if up then accepted.offer(c))
    serverNode.registerHandler(StreamKind.BundleRequest, silent)
    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)
      val serverSide = accepted.poll(10, TimeUnit.SECONDS)
      serverSide should not be null
      val released = releasedOnPeerStreams(serverSide)
      def counts = s"open streams: client ${conn.openStreamCount}, server ${serverSide.openStreamCount}"
      for _ <- 1 to 2 do
        val reset = new CountDownLatch(Batch)
        for _ <- 1 to Batch do
          val stream = openWithCredit(conn, StreamKind.BundleRequest)
          stream.onClosed(() => reset.countDown())
          stream.send("request".getBytes("UTF-8"))
        await("client streams to be reset", counts)(reset.await(_, TimeUnit.SECONDS))
        await("server streams to see the reset", counts)(released.tryAcquire(Batch, _, TimeUnit.SECONDS))
        stillConnected(conn, serverSide)
        eventually("client streams to close")(conn.openStreamCount == 0)
        eventually("server streams to close")(serverSide.openStreamCount == 0)
    finally
      clientNode.shutdown()
      serverNode.shutdown()
  }

  test("a CE stream its opener resets before the acceptor has read it is released at once") {
    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config.copy(streamIdleTimeoutMillis = 300))
    val accepted = new LinkedBlockingQueue[JamnpConnection]()
    val handled = new AtomicInteger()
    serverNode.onConnectionChange((c, up) => if up then accepted.offer(c))
    serverNode.registerHandler(
      StreamKind.BundleRequest,
      (_: JamnpConnection, stream: JamnpStream) =>
        handled.incrementAndGet()
        stream.onMessage(_ => ())
        ()
    )
    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)
      val serverSide = accepted.poll(10, TimeUnit.SECONDS)
      serverSide should not be null
      val released = releasedOnPeerStreams(serverSide)
      def counts = s"open streams: client ${conn.openStreamCount}, server ${serverSide.openStreamCount}"
      val reset = new CountDownLatch(Batch)
      serverSide.quic.eventLoop().execute(() => { reset.await(30, TimeUnit.SECONDS); () })
      val request = new Array[Byte](1024)
      for _ <- 1 to Batch do
        val stream = conn.openStream(StreamKind.BundleRequest).get(10, TimeUnit.SECONDS)
        stream.onClosed(() => reset.countDown())
        stream.send(request)
      await("client streams to be reset", counts)(reset.await(_, TimeUnit.SECONDS))
      await("server streams to see the reset", counts)(released.tryAcquire(Batch, _, TimeUnit.SECONDS))
      stillConnected(conn, serverSide)
      withClue("streams that reached the acceptor's handler: ") { handled.get() should be < Batch }
      eventually("client streams to close")(conn.openStreamCount == 0)
      eventually("server streams to close")(serverSide.openStreamCount == 0)
    finally
      clientNode.shutdown()
      serverNode.shutdown()
  }

  test("an opener's reset reaches the acceptor even when later datagrams overtake it") {
    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)
    val accepted = new LinkedBlockingQueue[JamnpConnection]()
    val requests = new LinkedBlockingQueue[Array[Byte]]()
    serverNode.onConnectionChange((c, up) => if up then accepted.offer(c))
    serverNode.registerHandler(
      StreamKind.BundleRequest,
      (_: JamnpConnection, stream: JamnpStream) =>
        stream.onMessage(requests.offer(_))
        ()
    )
    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val relay = new Relay(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
      try
        val conn = clientNode.connect(relay.address).get(10, TimeUnit.SECONDS)
        val serverSide = accepted.poll(10, TimeUnit.SECONDS)
        serverSide should not be null
        val released = releasedOnPeerStreams(serverSide)
        def counts = s"open streams: client ${conn.openStreamCount}, server ${serverSide.openStreamCount}"
        val stream = conn.openStream(StreamKind.BundleRequest).get(10, TimeUnit.SECONDS)
        stream.send("request".getBytes("UTF-8"))
        requests.poll(10, TimeUnit.SECONDS) should not be null
        Thread.sleep(300)
        relay.dropFromClient.set(1)
        stream.reset()
        await("server stream to see the reset", counts)(released.tryAcquire(1, _, TimeUnit.SECONDS))
        stillConnected(conn, serverSide)
        relay.dropFromClient.get() shouldBe 0
      finally relay.close()
    finally
      clientNode.shutdown()
      serverNode.shutdown()
  }

  test("an idle UP stream is left open") {
    val idle = config.copy(streamIdleTimeoutMillis = 200)
    val serverNode = new JamnpNode(NodeIdentity.generate(), idle)
    val clientNode = new JamnpNode(NodeIdentity.generate(), idle)
    val accepted = new LinkedBlockingQueue[JamnpStream]()
    serverNode.registerHandler(
      StreamKind.BlockAnnouncement,
      (_: JamnpConnection, stream: JamnpStream) =>
        stream.onMessage(_ => ())
        accepted.offer(stream)
        ()
    )
    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)
      val stream = conn.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
      stream.send("handshake".getBytes("UTF-8"))
      val serverStream = accepted.poll(10, TimeUnit.SECONDS)
      serverStream should not be null
      Thread.sleep(600) // three idle timeouts
      stream.isOpen shouldBe true
      serverStream.isOpen shouldBe true
    finally
      clientNode.shutdown()
      serverNode.shutdown()
  }
