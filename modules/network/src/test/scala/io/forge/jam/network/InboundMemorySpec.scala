package io.forge.jam.network

import java.net.InetSocketAddress
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** What a peer can make this node keep in memory before any handler has agreed to take it is bounded: a message on
  * a stream the peer opened may be no larger than its kind ever needs, and the frames still being received count
  * against a budget per connection and one for the whole node.
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

    def finishFrame(remaining: Int): Unit =
      val buf = stream.channel.alloc().buffer(remaining)
      buf.writeBytes(new Array[Byte](remaining))
      stream.channel.writeAndFlush(buf).sync()

    def wasReset: Boolean = ended.await(5, TimeUnit.SECONDS)
    def stillOpen: Boolean = !ended.await(300, TimeUnit.MILLISECONDS)

  private def open(conn: JamnpConnection, kind: Byte): Opened =
    new Opened(conn.openStream(kind).get(10, TimeUnit.SECONDS))

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
      first.startFrame(600 * KiB)
      eventually("the first frame to be counted")(server.node.heldInboundBytes == 600L * KiB)

      val second = open(conn, StreamKind.WorkPackageSubmission)
      second.startFrame(600 * KiB)
      second.wasReset shouldBe true
      first.stillOpen shouldBe true
      server.node.heldInboundBytes shouldBe 600L * KiB

      // Once the frame is complete it is the handler's, and the budget is free again.
      first.finishFrame(600 * KiB - 10)
      server.received.poll(10, TimeUnit.SECONDS) shouldBe (StreamKind.WorkPackageSubmission, 600 * KiB)
      eventually("the finished frame's bytes to be given back")(server.node.heldInboundBytes == 0L)

      // A stream that ends with its frame unfinished gives its bytes back too.
      val third = open(conn, StreamKind.WorkPackageSubmission)
      third.startFrame(600 * KiB)
      eventually("the third frame to be counted")(server.node.heldInboundBytes == 600L * KiB)
      third.stream.reset()
      eventually("the abandoned frame's bytes to be given back")(server.node.heldInboundBytes == 0L)
    finally
      client.shutdown()
      server.node.shutdown()
  }

  test("unfinished frames of all connections count against the node's budget") {
    val server = new Server(InboundLimits(maxConnectionBytes = 1024L * KiB, maxBytes = 1536L * KiB))
    val clientA = new JamnpNode(NodeIdentity.generate(), config())
    val clientB = new JamnpNode(NodeIdentity.generate(), config())
    try
      val a = open(server.connect(clientA), StreamKind.WorkPackageSubmission)
      a.startFrame(900 * KiB)
      eventually("the first connection's frame to be counted")(server.node.heldInboundBytes == 900L * KiB)
      val b = open(server.connect(clientB), StreamKind.WorkPackageSubmission)
      b.startFrame(900 * KiB)
      b.wasReset shouldBe true
      a.stillOpen shouldBe true
    finally
      clientA.shutdown()
      clientB.shutdown()
      server.node.shutdown()
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
      arriving.startFrame(600 * KiB)
      arriving.wasReset shouldBe true

      serverSide.releaseInbound(700L * KiB)
      server.node.heldInboundBytes shouldBe 0L
      val later = open(conn, StreamKind.WorkPackageSubmission)
      later.startFrame(600 * KiB)
      eventually("a frame to be taken once the handler let go")(server.node.heldInboundBytes == 600L * KiB)
    finally
      client.shutdown()
      server.node.shutdown()
  }
