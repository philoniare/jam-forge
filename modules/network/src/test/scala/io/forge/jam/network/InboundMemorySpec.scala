package io.forge.jam.network

import java.net.InetSocketAddress
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** What a peer can make this node keep in memory before any handler has agreed to take it is bounded: a message on
  * a stream the peer opened may be no larger than its kind ever needs, and the frames still being received count,
  * for the bytes of them that have arrived, against a budget per connection and one for the whole node.
  */
class InboundMemorySpec extends AnyFunSuite with Matchers:

  private val KiB = 1024
  private def config(inbound: InboundLimits = InboundLimits()) = JamnpConfig(genesisHashPrefix = "deadbeef", inbound = inbound)

  private def eventually(what: String, seconds: Int = 10)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(10)
    if !cond then fail(s"timed out waiting for: $what")

  private final class Server(inbound: InboundLimits = InboundLimits()):
    val node = new JamnpNode(NodeIdentity.generate(), config(inbound))
    val received = new LinkedBlockingQueue[(Byte, Int)]()
    val connections = new LinkedBlockingQueue[JamnpConnection]()
    List(StreamKind.BlockRequest, StreamKind.WorkPackageSubmission).foreach { kind =>
      node.registerHandler(
        kind,
        (conn: JamnpConnection, stream: JamnpStream) =>
          connections.offer(conn)
          stream.onMessage(msg => { received.offer((kind, msg.length)); () })
      )
    }
    node.start(new InetSocketAddress("127.0.0.1", 0))
    def connect(client: JamnpNode): JamnpConnection =
      client.connect(new InetSocketAddress("127.0.0.1", node.boundPort)).get(10, TimeUnit.SECONDS)

  private final class Opened(val stream: JamnpStream):
    val ended = new CountDownLatch(1)
    stream.onClosed(() => ended.countDown())

    /** The length prefix of a frame of `length` bytes and its first `sent` bytes: the frame is left unfinished. */
    def startFrame(length: Int, sent: Int = 10): Unit =
      val buf = stream.channel.alloc().buffer(4 + sent)
      buf.writeIntLE(length)
      buf.writeBytes(new Array[Byte](sent))
      stream.channel.writeAndFlush(buf).sync()

    def almostAll(length: Int): Unit = startFrame(length, length - Unsent)

    def finishFrame(remaining: Int): Unit =
      val buf = stream.channel.alloc().buffer(remaining)
      buf.writeBytes(new Array[Byte](remaining))
      stream.channel.writeAndFlush(buf).sync()

    def wasReset: Boolean = ended.await(5, TimeUnit.SECONDS)
    def stillOpen: Boolean = !ended.await(300, TimeUnit.MILLISECONDS)

  private def open(conn: JamnpConnection, kind: Byte): Opened =
    new Opened(conn.openStream(kind).get(10, TimeUnit.SECONDS))

  private val Unsent = 10

  private def heldFor(length: Int): Long = 4L + length - Unsent

  test("a message on a peer-opened stream may be no larger than its kind needs") {
    val server = new Server()
    val client = new JamnpNode(NodeIdentity.generate(), config())
    try
      val conn = server.connect(client)
      val request = open(conn, StreamKind.BlockRequest)
      request.stream.send(new Array[Byte](100 * KiB))
      request.wasReset shouldBe true
      // A work-package submission carries megabytes; the same message is taken there.
      val submission = open(conn, StreamKind.WorkPackageSubmission)
      submission.stream.send(new Array[Byte](100 * KiB))
      server.received.poll(10, TimeUnit.SECONDS) shouldBe (StreamKind.WorkPackageSubmission, 100 * KiB)
      // And a request of its usual size still is.
      val small = open(conn, StreamKind.BlockRequest)
      small.stream.send(new Array[Byte](37))
      server.received.poll(10, TimeUnit.SECONDS) shouldBe (StreamKind.BlockRequest, 37)
    finally
      client.shutdown()
      server.node.shutdown()
  }

  test("unfinished frames count against their connection: one over the budget is refused, and the bytes are given back") {
    val server = new Server(InboundLimits(maxConnectionBytes = 1024L * KiB))
    val client = new JamnpNode(NodeIdentity.generate(), config())
    try
      val conn = server.connect(client)
      val first = open(conn, StreamKind.WorkPackageSubmission)
      first.almostAll(600 * KiB)
      eventually("the first frame to be counted")(server.node.heldInboundBytes == heldFor(600 * KiB))

      val second = open(conn, StreamKind.WorkPackageSubmission)
      second.almostAll(600 * KiB)
      second.wasReset shouldBe true
      first.stillOpen shouldBe true
      eventually("the refused frame's bytes to be given back")(server.node.heldInboundBytes == heldFor(600 * KiB))

      // Once the frame is complete it is the handler's, and the budget is free again.
      first.finishFrame(Unsent)
      server.received.poll(10, TimeUnit.SECONDS) shouldBe (StreamKind.WorkPackageSubmission, 600 * KiB)
      eventually("the finished frame's bytes to be given back")(server.node.heldInboundBytes == 0L)

      // A stream that ends with its frame unfinished gives its bytes back too.
      val third = open(conn, StreamKind.WorkPackageSubmission)
      third.almostAll(600 * KiB)
      eventually("the third frame to be counted")(server.node.heldInboundBytes == heldFor(600 * KiB))
      third.stream.reset()
      eventually("the abandoned frame's bytes to be given back")(server.node.heldInboundBytes == 0L)
    finally
      client.shutdown()
      server.node.shutdown()
  }

  test("the length a frame announces holds nothing: frames begun and left count for the bytes that were sent") {
    val server = new Server(InboundLimits(maxConnectionBytes = 1024L * KiB, maxBytes = 1536L * KiB))
    val idle = new JamnpNode(NodeIdentity.generate(), config())
    val busy = new JamnpNode(NodeIdentity.generate(), config())
    try
      val idleConn = server.connect(idle)
      val begun = (1 to 40).map { _ =>
        val stream = open(idleConn, StreamKind.WorkPackageSubmission)
        stream.startFrame(24 * 1024 * KiB)
        stream
      }
      eventually("the frames begun to be counted")(server.node.heldInboundBytes == 40L * 14)
      begun.foreach(_.stillOpen shouldBe true)

      val whole = open(server.connect(busy), StreamKind.WorkPackageSubmission)
      whole.stream.send(new Array[Byte](900 * KiB))
      server.received.poll(10, TimeUnit.SECONDS) shouldBe (StreamKind.WorkPackageSubmission, 900 * KiB)
      eventually("only the frames begun to stay counted")(server.node.heldInboundBytes == 40L * 14)
    finally
      idle.shutdown()
      busy.shutdown()
      server.node.shutdown()
  }

  test("unfinished frames of all connections count against the node's budget") {
    val server = new Server(InboundLimits(maxConnectionBytes = 1024L * KiB, maxBytes = 1536L * KiB))
    val clientA = new JamnpNode(NodeIdentity.generate(), config())
    val clientB = new JamnpNode(NodeIdentity.generate(), config())
    try
      val a = open(server.connect(clientA), StreamKind.WorkPackageSubmission)
      a.almostAll(900 * KiB)
      eventually("the first connection's frame to be counted")(server.node.heldInboundBytes == heldFor(900 * KiB))
      val b = open(server.connect(clientB), StreamKind.WorkPackageSubmission)
      b.almostAll(900 * KiB)
      b.wasReset shouldBe true
      a.stillOpen shouldBe true
    finally
      clientA.shutdown()
      clientB.shutdown()
      server.node.shutdown()
  }

  test("an answer refused for our own budget is told apart from the peer resetting the stream") {
    val server = new Server(InboundLimits(maxConnectionBytes = 64L * KiB))
    val client = new JamnpNode(NodeIdentity.generate(), config())
    client.registerHandler(
      StreamKind.ShardDistribution,
      (_: JamnpConnection, stream: JamnpStream) =>
        stream.onMessage { msg =>
          if msg(0) == 1 then
            stream.send(new Array[Byte](10))
            stream.send(new Array[Byte](200 * KiB))
            stream.finish()
          else stream.reset()
        }
    )
    try
      val accepted = new LinkedBlockingQueue[JamnpConnection]()
      server.node.onConnectionChange((c, up) => if up then accepted.offer(c))
      server.connect(client)
      val toClient = accepted.poll(10, TimeUnit.SECONDS)
      def ask(first: Byte): (JamnpStream, Int) =
        val stream = toClient.openStream(StreamKind.ShardDistribution).get(10, TimeUnit.SECONDS)
        val ended = new CountDownLatch(1)
        val got = new java.util.concurrent.atomic.AtomicInteger()
        stream.onMessage(_ => { got.incrementAndGet(); () })
        stream.onClosed(() => ended.countDown())
        stream.send(Array[Byte](first))
        stream.finish()
        ended.await(10, TimeUnit.SECONDS) shouldBe true
        (stream, got.get)
      val (refused, received) = ask(1)
      received shouldBe 1
      refused.refusedHere shouldBe true
      ask(2)._1.refusedHere shouldBe false
    finally
      client.shutdown()
      server.node.shutdown()
  }

  test("a stream opened only to send keeps nothing of what the peer answers on it") {
    val answering = new JamnpNode(NodeIdentity.generate(), config())
    val answered = new LinkedBlockingQueue[JamnpStream]()
    answering.registerHandler(
      StreamKind.AssuranceDistribution,
      (_: JamnpConnection, stream: JamnpStream) =>
        stream.onMessage { _ =>
          stream.send(new Array[Byte](200 * KiB))
          answered.offer(stream)
          ()
        }
    )
    val sender = new JamnpNode(NodeIdentity.generate(), config())
    try
      answering.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = sender.connect(new InetSocketAddress("127.0.0.1", answering.boundPort)).get(10, TimeUnit.SECONDS)
      def answerWritten(): Unit =
        val stream = answered.poll(10, TimeUnit.SECONDS)
        eventually("the answer to be written")(stream.pendingOutboundBytes == 0L)

      val unread = conn.openStream(StreamKind.AssuranceDistribution).get(10, TimeUnit.SECONDS)
      unread.send(Array[Byte](1))
      answerWritten()
      eventually("the answer to wait for a receiver")(unread.pendingInboundBytes == 200L * KiB)

      val before = conn.openStream(StreamKind.AssuranceDistribution).get(10, TimeUnit.SECONDS)
      before.ignoreInbound()
      before.send(Array[Byte](1))
      before.finish()
      answerWritten()
      unread.ignoreInbound()
      unread.pendingInboundBytes shouldBe 0L
      Thread.sleep(500)
      before.pendingInboundBytes shouldBe 0L
      val late = new LinkedBlockingQueue[Array[Byte]]()
      before.onMessage(m => { late.offer(m); () })
      late.poll(200, TimeUnit.MILLISECONDS) shouldBe null
    finally
      sender.shutdown()
      answering.shutdown()
  }

  test("bytes a handler holds for a connection share its budget with the frames still arriving") {
    val server = new Server(InboundLimits(maxConnectionBytes = 1024L * KiB))
    val client = new JamnpNode(NodeIdentity.generate(), config())
    try
      val conn = server.connect(client)
      val probe = open(conn, StreamKind.BlockRequest)
      probe.stream.send(new Array[Byte](8))
      val serverSide = server.connections.poll(10, TimeUnit.SECONDS)
      serverSide.holdInbound(700L * KiB) shouldBe true
      serverSide.holdInbound(700L * KiB) shouldBe false
      serverSide.heldInboundBytes shouldBe 700L * KiB

      val arriving = open(conn, StreamKind.WorkPackageSubmission)
      arriving.almostAll(600 * KiB)
      arriving.wasReset shouldBe true

      serverSide.releaseInbound(700L * KiB)
      eventually("everything to be given back")(server.node.heldInboundBytes == 0L)
      val later = open(conn, StreamKind.WorkPackageSubmission)
      later.almostAll(600 * KiB)
      eventually("a frame to be taken once the handler let go")(server.node.heldInboundBytes == heldFor(600 * KiB))
    finally
      client.shutdown()
      server.node.shutdown()
  }
