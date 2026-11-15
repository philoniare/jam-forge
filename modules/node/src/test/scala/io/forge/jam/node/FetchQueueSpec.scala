package io.forge.jam.node

import io.forge.jam.core.Hashing
import io.forge.jam.core.primitives.Hash
import io.forge.jam.node.FetchQueue.Request
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FetchQueueSpec extends AnyFunSuite with Matchers:

  private final class Peer(val name: String, var open: Boolean = true)

  private def block(n: Int): Hash = Hashing.blake2b256(Array[Byte](n.toByte))

  private def queue(maxInFlight: Int = 4) =
    new FetchQueue[Peer](maxInFlight, maxPerPeer = 8, maxPending = 64, maxAlternates = 3, isOpen = _.open)

  private def request(peer: Peer, from: Hash) = new Request(peer, from, maxBlocks = 1, onDone = () => ())

  test("a peer holds one fetch at a time; its next one waits while other peers' fetches go out") {
    val q = queue()
    val (a, b) = (new Peer("a"), new Peer("b"))
    val (a1, a2, b1) = (request(a, block(1)), request(a, block(2)), request(b, block(3)))
    List(a1, a2, b1).foreach(q.offer(_) shouldBe Nil)

    q.next() shouldBe Some(a1)
    q.next() shouldBe Some(b1)
    q.next() shouldBe None
    q.size shouldBe 1

    q.finished(a1, have = true) shouldBe Nil
    q.next() shouldBe Some(a2)
  }

  test("no more fetches are out at once than the limit") {
    val q = queue(maxInFlight = 2)
    val requests = (1 to 3).map(n => request(new Peer(s"p$n"), block(n)))
    requests.foreach(q.offer(_) shouldBe Nil)

    q.next() shouldBe Some(requests(0))
    q.next() shouldBe Some(requests(1))
    q.next() shouldBe None

    q.finished(requests(0), have = true) shouldBe Nil
    q.next() shouldBe Some(requests(2))
  }

  test("a peer that did not answer loses its waiting fetches, and another peer that announced the block is asked") {
    val q = queue()
    val (a, c) = (new Peer("a"), new Peer("c"))
    val (a1, a2, a3) = (request(a, block(1)), request(a, block(2)), request(a, block(3)))
    val c2 = request(c, block(2))
    List(a1, a2, a3, c2).foreach(q.offer(_) shouldBe Nil)
    q.next() shouldBe Some(a1)

    q.finished(a1, have = false, answered = false) shouldBe List(a2, a3)
    q.next() shouldBe Some(c2)
    q.next() shouldBe None
    q.size shouldBe 0
  }

  test("a peer that answered without the block keeps its waiting fetches") {
    val q = queue()
    val a = new Peer("a")
    val (a1, a2) = (request(a, block(1)), request(a, block(2)))
    List(a1, a2).foreach(q.offer(_) shouldBe Nil)
    q.next() shouldBe Some(a1)

    q.finished(a1, have = false, answered = true) shouldBe Nil
    q.next() shouldBe Some(a2)
  }
