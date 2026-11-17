package io.forge.jam.node

import java.net.InetSocketAddress
import java.util.concurrent.{CountDownLatch, TimeUnit}

import io.forge.jam.network.{JamnpConfig, PeerConnection, JamnpNode, MessageStream, NodeIdentity, StreamKind}
import io.forge.jam.node.StreamRequest.{Failure, Until}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** One request/response exchange over a real connection: what counts as the answer, and that a stream which brought
  * none is given up on both sides.
  */
class StreamRequestSpec extends AnyFunSuite with Matchers:

  private val kind = StreamKind.BlockRequest

  /** A server answering each request as `respond` says, and counting the streams that ended on its side. */
  private def withPeer(respond: (MessageStream, Array[Byte]) => Unit)(body: (PeerConnection, CountDownLatch) => Unit): Unit =
    val config = JamnpConfig(genesisHashPrefix = "deadbeef")
    val server = new JamnpNode(NodeIdentity.generate(), config)
    val client = new JamnpNode(NodeIdentity.generate(), config)
    val ended = new CountDownLatch(1)
    server.registerPeerHandler(
      kind,
      (_: PeerConnection, stream: MessageStream) =>
        stream.onMessage(m => respond(stream, m))
        stream.onClosed(() => ended.countDown())
    )
    server.start(new InetSocketAddress("127.0.0.1", 0))
    try body(client.connect(new InetSocketAddress("127.0.0.1", server.boundPort)).get(10, TimeUnit.SECONDS), ended)
    finally
      client.shutdown()
      server.shutdown()

  private def exchange(conn: PeerConnection, until: Until = Until.Closed, waitMs: Long = 5000, max: Int = 16) =
    StreamRequest.exchange(conn, kind, Seq(Array[Byte](1, 2, 3)), 5000, () => waitMs, until, max).map(_.map(_.toSeq))

  test("the answer is every message the peer sent before closing, up to the limit") {
    withPeer { (stream, request) =>
      stream.send(request)
      stream.send(request.reverse)
      stream.finish()
    } { (conn, _) =>
      exchange(conn) shouldBe Right(List(Seq[Byte](1, 2, 3), Seq[Byte](3, 2, 1)))
      exchange(conn, max = 1) shouldBe Right(List(Seq[Byte](1, 2, 3)))
    }
  }

  test("with excessFails a message past the limit fails the exchange, and so do bytes past maxBytes") {
    withPeer { (stream, request) =>
      stream.send(request)
      stream.send(request)
      stream.send(request)
      stream.finish()
    } { (conn, _) =>
      def strict(maxMessages: Int, maxBytes: Long) =
        StreamRequest.exchange(conn, kind, Seq(Array[Byte](1, 2, 3)), 5000, () => 5000, Until.Closed, maxMessages, excessFails = true, maxBytes = maxBytes)
      strict(2, Long.MaxValue) shouldBe a[Left[?, ?]]
      strict(2, Long.MaxValue).left.toOption.get shouldBe a[Failure.Excess]
      strict(3, 8).left.toOption.get shouldBe a[Failure.Excess]
      strict(3, 9).map(_.size) shouldBe Right(3)
    }
  }

  test("with FirstMessage the answer does not wait for the peer to close") {
    withPeer((stream, request) => stream.send(request)) { (conn, _) =>
      val started = System.currentTimeMillis()
      exchange(conn, Until.FirstMessage) shouldBe Right(List(Seq[Byte](1, 2, 3)))
      (System.currentTimeMillis() - started) should be < 4000L
    }
  }

  test("a peer that closes without a message gives an empty answer") {
    withPeer((stream, _) => stream.finish()) { (conn, _) =>
      exchange(conn) shouldBe Right(Nil)
      exchange(conn, Until.FirstMessage) shouldBe Right(Nil)
    }
  }

  test("a peer that neither answers nor closes times out, and the stream ends on its side too") {
    withPeer((_, _) => ()) { (conn, ended) =>
      exchange(conn, waitMs = 300) shouldBe Left(Failure.TimedOut)
      ended.await(5, TimeUnit.SECONDS) shouldBe true
    }
  }

  test("a peer that answers but never closes times out when the close is awaited") {
    withPeer((stream, request) => stream.send(request)) { (conn, ended) =>
      exchange(conn, waitMs = 300) shouldBe Left(Failure.TimedOut)
      ended.await(5, TimeUnit.SECONDS) shouldBe true
    }
  }

  test("no time left to open, or to wait, is a failure without a wait") {
    withPeer((stream, request) => { stream.send(request); stream.finish() }) { (conn, _) =>
      StreamRequest.exchange(conn, kind, Seq(Array[Byte](1)), 0, () => 5000) should matchPattern {
        case Left(_: Failure.NotOpened) =>
      }
      exchange(conn, waitMs = 0) shouldBe Left(Failure.TimedOut)
    }
  }
