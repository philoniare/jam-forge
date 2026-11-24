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

  test("strangers do not hold every fetch: one is kept for known peers, whose fetches also go out first") {
    val known = new Peer("validator")
    val q = new FetchQueue[Peer](
      maxInFlight = 4, maxPerPeer = 8, maxPending = 64, maxAlternates = 3, isOpen = _.open,
      preferred = _ eq known, keptForPreferred = 1
    )
    val strangers = (1 to 6).map(i => new Peer(s"stranger-$i"))
    val theirs = strangers.zipWithIndex.map((p, i) => request(p, block(i)))
    theirs.foreach(q.offer(_) shouldBe Nil)
    q.next() shouldBe Some(theirs(0))
    q.next() shouldBe Some(theirs(1))
    q.next() shouldBe Some(theirs(2))
    q.next() shouldBe None
    q.size shouldBe 3
    val ours = request(known, block(50))
    q.offer(ours) shouldBe Nil
    q.next() shouldBe Some(ours)
    q.next() shouldBe None
    q.finished(theirs(0), have = true)
    q.next() shouldBe Some(theirs(3))
    q.finished(ours, have = true)
    q.next() shouldBe None
    val moreKnown = (1 to 4).map(i => new Peer(s"known-$i"))
    val all = new FetchQueue[Peer](
      maxInFlight = 4, maxPerPeer = 8, maxPending = 64, maxAlternates = 3, isOpen = _.open,
      preferred = moreKnown.contains, keptForPreferred = 1
    )
    val known4 = moreKnown.zipWithIndex.map((p, i) => request(p, block(60 + i)))
    known4.foreach(all.offer(_) shouldBe Nil)
    known4.foreach(r => all.next() shouldBe Some(r))
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
