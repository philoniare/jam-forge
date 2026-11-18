package io.forge.jam.network

import java.util.concurrent.{CopyOnWriteArrayList, CountDownLatch, TimeUnit}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

/** The in-memory transport behaves as the services expect of a connection: ordered messages, a FIN after them, a
  * reset seen by both ends.
  */
class LoopbackSpec extends AnyFunSuite with Matchers:

  private val (keyA, keyB) = (Array.fill[Byte](32)(1), Array.fill[Byte](32)(2))
  private val kind = StreamKind.BlockRequest

  private def pair(serve: PeerStreamHandler): (Loopback.Connection, Loopback.Connection) =
    Loopback.pair(keyA, keyB, _ => None, k => Option.when(k == kind)(serve))

  /** Everything that reached one end, with "FIN" where its close listener fired. */
  private final class Seen(stream: MessageStream):
    val events = new CopyOnWriteArrayList[String]()
    val ended = new CountDownLatch(1)
    stream.onMessage(m => events.add(new String(m)): Unit)
    stream.onClosed { () => events.add("FIN"); ended.countDown() }
    def all: List[String] =
      ended.await(5, TimeUnit.SECONDS) shouldBe true
      events.asScala.toList

  test("a request is answered: messages arrive in order, then the FIN, and each side knows the other's key") {
    val served = new CopyOnWriteArrayList[String]()
    val (a, b) = pair { (conn, stream) =>
      served.add(conn.peerKey.head.toString)
      stream.onMessage { m =>
        stream.send(m.reverse)
        stream.send("again".getBytes)
        stream.finish()
      }
    }
    (a.initiator, b.initiator, a.peerKey.head, b.peerKey.head) shouldBe ((true, false, 2, 1))
    val stream = a.openStream(kind).get(5, TimeUnit.SECONDS)
    val seen = new Seen(stream)
    stream.send("abc".getBytes)
    stream.finish()
    seen.all shouldBe List("cba", "again", "FIN")
    served.asScala.toList shouldBe List("1")
    a.openStreamCount shouldBe 0
    b.openStreamCount shouldBe 0
  }

  test("a stream that sees the peer's FIN before finishing its own side stops counting once it finishes") {
    val (a, b) = pair((_, stream) => stream.onMessage { m => stream.send(m); stream.finish() }: Unit)
    val stream = a.openStream(kind).get(5, TimeUnit.SECONDS)
    val seen = new Seen(stream)
    stream.send("abc".getBytes)
    seen.all shouldBe List("abc", "FIN")
    a.openStreamCount shouldBe 1
    stream.finish()
    eventually(a.openStreamCount == 0 && b.openStreamCount == 0) shouldBe true
  }

  private def eventually(condition: => Boolean): Boolean =
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
    while !condition && System.nanoTime() < deadline do Thread.sleep(5)
    condition

  test("messages sent before a receiver is registered wait for it") {
    val (a, _) = pair((_, stream) => { stream.send("early".getBytes); stream.finish() })
    val stream = a.openStream(kind).get(5, TimeUnit.SECONDS)
    Thread.sleep(100)
    new Seen(stream).all shouldBe List("early", "FIN")
  }

  test("a reset ends the stream on both sides and nothing sent after it arrives") {
    val theirs = new java.util.concurrent.LinkedBlockingQueue[Seen]()
    val (a, _) = pair((_, stream) => theirs.add(new Seen(stream)): Unit)
    val stream = a.openStream(kind).get(5, TimeUnit.SECONDS)
    val ours = new Seen(stream)
    stream.send("one".getBytes)
    val remote = theirs.poll(5, TimeUnit.SECONDS)
    stream.reset()
    stream.send("lost".getBytes)
    remote.all.last shouldBe "FIN"
    remote.events.asScala should not contain "lost"
    ours.all shouldBe List("FIN")
    stream.isOpen shouldBe false
  }

  test("a kind nobody handles is reset, and a closed connection opens no stream") {
    val (a, b) = pair((_, _) => ())
    val stream = a.openStream(StreamKind.StateRequest).get(5, TimeUnit.SECONDS)
    new Seen(stream).all shouldBe List("FIN")
    b.close()
    a.isOpen shouldBe false
    a.openStream(kind).isCompletedExceptionally shouldBe true
  }
