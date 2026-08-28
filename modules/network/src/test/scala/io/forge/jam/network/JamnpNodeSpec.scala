package io.forge.jam.network

import java.net.InetSocketAddress
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JamnpNodeSpec extends AnyFunSuite with Matchers:

  private val config = JamnpConfig(genesisHashPrefix = "deadbeef")

  private def await[A](q: LinkedBlockingQueue[A], seconds: Int = 10): A =
    val v = q.poll(seconds.toLong, TimeUnit.SECONDS)
    if v == null then fail("timed out waiting for message")
    v

  test("preferred initiator rule is symmetric and total") {
    val a = Array.fill[Byte](32)(0x01)
    val b = Array.fill[Byte](32)(0x02)
    val p1 = PreferredInitiator.of(a, b)
    val p2 = PreferredInitiator.of(b, a)
    p1 shouldBe p2
    (p1 eq a) || (p1 eq b) shouldBe true

    // Flipping the top bit of one key's last byte flips the choice.
    val bHigh = b.clone(); bHigh(31) = 0x80.toByte
    PreferredInitiator.of(a, bHigh) should not be theSameInstanceAs(
      PreferredInitiator.of(a, b) match
        case x if x eq a => a
        case _           => bHigh
    )
  }

  test("CE-style request/response: kind dispatch, framing, FIN semantics") {
    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)

    // CE 128-style responder: one request message → two response messages + FIN.
    serverNode.registerHandler(
      StreamKind.BlockRequest,
      (conn: JamnpConnection, stream: JamnpStream) =>
        stream.onMessage { request =>
          stream.send(("resp1:" + new String(request, "UTF-8")).getBytes("UTF-8"))
          stream.send("resp2".getBytes("UTF-8"))
          stream.finish()
        }
    )

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      conn.negotiatedAlpn shouldBe "jamnp-s/0/deadbeef"
      conn.peerKey shouldBe serverNode.identity.publicKeyBytes

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.BlockRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.onClosed(() => closed.offer(true))

      stream.send("blocks-please".getBytes("UTF-8"))
      stream.finish()

      new String(await(responses), "UTF-8") shouldBe "resp1:blocks-please"
      new String(await(responses), "UTF-8") shouldBe "resp2"
      await(closed) shouldBe true
    finally
      clientNode.shutdown()
      serverNode.shutdown()
  }

  test("UP-style persistent stream exchanges messages both ways") {
    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)

    val serverReceived = new LinkedBlockingQueue[String]()
    serverNode.registerHandler(
      StreamKind.BlockAnnouncement,
      (conn: JamnpConnection, stream: JamnpStream) =>
        stream.onMessage { msg =>
          serverReceived.offer(new String(msg, "UTF-8"))
          stream.send("server-handshake".getBytes("UTF-8"))
        }
    )

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val fromServer = new LinkedBlockingQueue[String]()
      val stream = conn.openStream(StreamKind.BlockAnnouncement).get(10, TimeUnit.SECONDS)
      stream.onMessage(msg => fromServer.offer(new String(msg, "UTF-8")))

      stream.send("client-handshake".getBytes("UTF-8"))
      await(serverReceived) shouldBe "client-handshake"
      await(fromServer) shouldBe "server-handshake"

      // Stream stays open; further announcements flow.
      stream.send("announce-1".getBytes("UTF-8"))
      await(serverReceived) shouldBe "announce-1"
      stream.isOpen shouldBe true
    finally
      clientNode.shutdown()
      serverNode.shutdown()
  }

  test("streams of unregistered kinds are rejected") {
    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val closed = new LinkedBlockingQueue[Boolean]()
      val stream = conn.openStream(StreamKind.JudgmentPublication).get(10, TimeUnit.SECONDS)
      stream.onClosed(() => closed.offer(true))
      stream.send("hello".getBytes("UTF-8"))

      await(closed) shouldBe true
    finally
      clientNode.shutdown()
      serverNode.shutdown()
  }

  test("large frames round-trip (multi-megabyte messages)") {
    val serverNode = new JamnpNode(NodeIdentity.generate(), config)
    val clientNode = new JamnpNode(NodeIdentity.generate(), config)

    serverNode.registerHandler(
      StreamKind.BundleRequest,
      (conn: JamnpConnection, stream: JamnpStream) =>
        stream.onMessage { request =>
          stream.send(request) // echo
          stream.finish()
        }
    )

    try
      serverNode.start(new InetSocketAddress("127.0.0.1", 0))
      val conn = clientNode
        .connect(new InetSocketAddress("127.0.0.1", serverNode.boundPort))
        .get(10, TimeUnit.SECONDS)

      val payload = new Array[Byte](3 * 1024 * 1024)
      new java.util.Random(42).nextBytes(payload)

      val responses = new LinkedBlockingQueue[Array[Byte]]()
      val stream = conn.openStream(StreamKind.BundleRequest).get(10, TimeUnit.SECONDS)
      stream.onMessage(responses.offer(_))
      stream.send(payload)
      stream.finish()

      val echoed = await(responses, seconds = 30)
      echoed.length shouldBe payload.length
      java.util.Arrays.equals(echoed, payload) shouldBe true
    finally
      clientNode.shutdown()
      serverNode.shutdown()
  }
