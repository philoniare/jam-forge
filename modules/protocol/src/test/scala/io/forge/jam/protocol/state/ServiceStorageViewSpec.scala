package io.forge.jam.protocol.state

import io.forge.jam.core.JamBytes
import io.forge.jam.core.trie.{StateTrie, InMemoryTrieBackend}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ServiceStorageViewSpec extends AnyFlatSpec with Matchers:

  private def key(first: Int, second: Int): JamBytes =
    val a = new Array[Byte](31)
    a(0) = first.toByte
    a(1) = second.toByte
    JamBytes(a)

  "ServiceStorageView.enumerate" should "filter pending writes by the requested prefix" in {
    val view = new ServiceStorageView(StateTrie.empty(new InMemoryTrieBackend()))
    val inPrefix = key(0xab, 0x01)
    val outOfPrefix = key(0xcd, 0x02)
    view.putByStateKey(inPrefix, JamBytes(Array[Byte](1, 2, 3)))
    view.putByStateKey(outOfPrefix, JamBytes(Array[Byte](4, 5, 6)))

    // Prefix = first byte 0xab (8 bits).
    val keys = view.enumerate(key(0xab, 0x00), 8).map(_._1).toList

    keys should contain(inPrefix)
    keys should not contain outOfPrefix
  }

  private def bytes(xs: Int*): JamBytes = JamBytes(xs.map(_.toByte).toArray)

  "ServiceStorageView.fork" should "GP: keep each accpar service's writes apart until absorbed (eq. 12.18)" in {
    val parent = new ServiceStorageView(StateTrie.empty(new InMemoryTrieBackend()))
    val staged = key(0x01, 0x00)
    parent.putByStateKey(staged, bytes(1))

    val a = parent.fork()
    val b = parent.fork()
    a.getByStateKey(staged) shouldBe Some(bytes(1))
    a.putByStateKey(key(0x02, 0x00), bytes(2))
    b.deleteByStateKey(staged)

    b.getByStateKey(key(0x02, 0x00)) shouldBe None
    a.getByStateKey(staged) shouldBe Some(bytes(1))
    parent.getByStateKey(key(0x02, 0x00)) shouldBe None

    parent.absorb(a)
    parent.absorb(b)
    parent.getByStateKey(key(0x02, 0x00)) shouldBe Some(bytes(2))
    parent.getByStateKey(staged) shouldBe None
  }

  it should "roll a fork back to its own savepoint without touching the parent or other forks" in {
    val parent = new ServiceStorageView(StateTrie.empty(new InMemoryTrieBackend()))
    val a = parent.fork()
    val b = parent.fork()
    b.putByStateKey(key(0x03, 0x00), bytes(3))
    a.savepoint()
    a.putByStateKey(key(0x04, 0x00), bytes(4))
    a.restore()

    parent.absorb(a)
    parent.absorb(b)
    parent.getByStateKey(key(0x04, 0x00)) shouldBe None
    parent.getByStateKey(key(0x03, 0x00)) shouldBe Some(bytes(3))
    parent.savepointDepth shouldBe 0
  }

  it should "GP: read the batch-start state through getAtBatchStart, without the fork's own writes (eq. 12.18)" in {
    val trie = StateTrie.empty(new InMemoryTrieBackend())
    trie.update(Seq((key(0x05, 0x00), Some(bytes(1)))))
    val parent = new ServiceStorageView(trie)
    parent.putByStateKey(key(0x05, 0x00), bytes(2)) // an earlier batch's write
    val a = parent.fork()
    a.putByStateKey(key(0x05, 0x00), bytes(3))

    a.getByStateKey(key(0x05, 0x00)) shouldBe Some(bytes(3))
    a.getAtBatchStart(key(0x05, 0x00)) shouldBe Some(bytes(2))
    parent.getAtBatchStart(key(0x05, 0x00)) shouldBe Some(bytes(2))
  }

  it should "GP: reject two forks of one batch writing one key differently (text after eq. 12.18)" in {
    val parent = new ServiceStorageView(StateTrie.empty(new InMemoryTrieBackend()))
    val a = parent.fork()
    val b = parent.fork()
    a.putByStateKey(key(0x06, 0x00), bytes(1))
    b.putByStateKey(key(0x06, 0x00), bytes(2))
    parent.absorb(a)
    val e = intercept[RuntimeException](parent.absorb(b))
    e.getMessage should include("block invalid")
  }

  it should "accept two forks of one batch writing one key identically" in {
    val parent = new ServiceStorageView(StateTrie.empty(new InMemoryTrieBackend()))
    parent.putByStateKey(key(0x07, 0x00), bytes(1))
    val a = parent.fork()
    val b = parent.fork()
    a.deleteByStateKey(key(0x07, 0x00))
    b.deleteByStateKey(key(0x07, 0x00))
    parent.absorb(a)
    parent.absorb(b)
    parent.getByStateKey(key(0x07, 0x00)) shouldBe None
  }
