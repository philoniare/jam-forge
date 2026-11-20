package io.forge.jam.core.trie

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.util.Random

class BufferedTrieBackendSpec extends AnyFunSuite with Matchers:

  private type Update = (JamBytes, Option[JamBytes])

  private def key(i: Int): JamBytes =
    JamBytes(Hashing.blake2b256(Array[Byte](7, (i >> 8).toByte, i.toByte)).bytes.toArray.take(31))

  private def value(v: Int): JamBytes =
    JamBytes(Array.fill[Byte](if v % 3 == 0 then 12 else 40 + v)((v * 17 + 3).toByte))

  private final class Recording(val store: InMemoryTrieBackend = new InMemoryTrieBackend) extends StateTrieBackend:
    var batches = 0
    var rawValueWrites = 0
    var nodeWrites = 0
    def readNode(hash: Hash): Option[TrieNode] = store.readNode(hash)
    def readRawValue(hash: Hash): Option[JamBytes] = store.readRawValue(hash)
    def batchUpdate(ops: Seq[BackendOp]): Unit =
      batches += 1
      ops.foreach {
        case _: BackendOp.WriteRawValue => rawValueWrites += 1
        case _: BackendOp.WriteNode     => nodeWrites += 1
        case _                          => ()
      }
      store.batchUpdate(ops)
    def gc(): Unit = store.gc()
    def clear(): Unit = store.clear()

  private def reachable(backend: StateTrieBackend, root: Hash, nodes: mutable.Set[Hash], values: mutable.Set[Hash]): Unit =
    if root != Hash.zero && nodes.add(root) then
      val node = backend.readNode(root).getOrElse(fail(s"node ${root.toHex.take(12)} is missing"))
      node.nodeType match
        case TrieNodeType.Branch =>
          reachable(backend, Hash.wrap(node.left.toArrayUnsafe), nodes, values)
          reachable(backend, Hash.wrap(node.right.toArrayUnsafe), nodes, values)
        case TrieNodeType.RegularLeaf => values += Hash.wrap(node.right.toArrayUnsafe)
        case TrieNodeType.EmbeddedLeaf => ()

  private def block(rnd: Random, keys: Int): Seq[Update] =
    Seq.fill(1 + rnd.nextInt(12)) {
      val k = key(rnd.nextInt(keys))
      k -> (if rnd.nextInt(5) == 0 then None else Some(value(rnd.nextInt(9))))
    }

  test("a batch of blocks saved through the buffer leaves the store as one save of the whole batch does") {
    val rnd = new Random(20261120L)
    val oracle = new Recording
    val buffered = new Recording
    val buffer = new BufferedTrieBackend(buffered)
    var root = Hash.zero
    val seenNodes = mutable.Set[Hash]()
    val seenValues = mutable.Set[Hash]()
    (0 until 60).foreach { round =>
      val blocks = Seq.fill(1 + rnd.nextInt(8))(block(rnd, keys = 60))
      val whole = StateTrie.at(oracle, root)
      val roots = blocks.map { updates =>
        whole.update(updates)
        whole.rootHash
      }
      whole.save()
      buffer.begin()
      var at = root
      blocks.zip(roots).foreach { (updates, expected) =>
        val t = StateTrie.at(buffer, at)
        t.update(updates)
        t.save()
        withClue(s"round $round: ")(t.rootHash shouldBe expected)
        at = t.rootHash
      }
      buffer.flush()
      root = roots.last
      withClue(s"round $round: ") {
        buffered.store.nodeCount shouldBe oracle.store.nodeCount
        buffered.store.valueCount shouldBe oracle.store.valueCount
        reachable(buffered.store, root, seenNodes, seenValues)
        seenNodes.foreach(h => buffered.store.nodeRefcount(h) shouldBe oracle.store.nodeRefcount(h))
        seenValues.foreach(h => buffered.store.valueRefcount(h) shouldBe oracle.store.valueRefcount(h))
        (0 until 60).foreach(i => StateTrie.at(buffered.store, root).read(key(i)) shouldBe StateTrie.at(oracle.store, root).read(key(i)))
      }
    }
    buffered.batches shouldBe oracle.batches
    buffered.store.gc()
    oracle.store.gc()
    buffered.store.nodeCount shouldBe oracle.store.nodeCount
    buffered.store.valueCount shouldBe oracle.store.valueCount
  }

  test("a value rewritten by every block of a batch reaches the store once") {
    val store = new Recording
    val buffer = new BufferedTrieBackend(store)
    buffer.begin()
    var root = Hash.zero
    (1 to 8).foreach { b =>
      val t = StateTrie.at(buffer, root)
      t.update(Seq(key(1) -> Some(JamBytes(Array.fill[Byte](1000)(b.toByte))), key(2) -> Some(value(1))))
      t.save()
      root = t.rootHash
    }
    store.batches shouldBe 0
    buffer.flush()
    store.batches shouldBe 1
    store.rawValueWrites shouldBe 2 // the last value of key 1, and key 2's
    StateTrie.at(store.store, root).read(key(1)) shouldBe Some(JamBytes(Array.fill[Byte](1000)(8.toByte)))
  }

  test("a block that is not saved leaves nothing, and the next block builds on the one before it") {
    val oracle = new Recording
    val store = new Recording
    val buffer = new BufferedTrieBackend(store)
    val first = Seq(key(1) -> Some(value(1)), key(2) -> Some(value(2)))
    val refused = Seq(key(1) -> Some(value(4)), key(3) -> Some(value(5)))
    val third = Seq(key(2) -> None, key(4) -> Some(value(7)))
    val whole = StateTrie.at(oracle, Hash.zero)
    whole.update(first)
    whole.update(third)
    whole.save()

    buffer.begin()
    val a = StateTrie.at(buffer, Hash.zero)
    a.update(first)
    a.save()
    StateTrie.at(buffer, a.rootHash).update(refused) // executed, found invalid, never saved
    val c = StateTrie.at(buffer, a.rootHash)
    c.update(third)
    c.save()
    buffer.flush()
    c.rootHash shouldBe whole.rootHash
    store.store.nodeCount shouldBe oracle.store.nodeCount
    store.store.valueCount shouldBe oracle.store.valueCount
  }

  test("a discarded batch leaves the store as it was") {
    val store = new Recording
    val buffer = new BufferedTrieBackend(store)
    val before = StateTrie.at(buffer, Hash.zero)
    before.update(Seq(key(1) -> Some(value(1))))
    before.save() // not buffering: written at once
    store.batches shouldBe 1
    val (nodes, values) = (store.store.nodeCount, store.store.valueCount)

    buffer.begin()
    buffer.buffering shouldBe true
    val t = StateTrie.at(buffer, before.rootHash)
    t.update(Seq(key(1) -> Some(value(2)), key(5) -> Some(value(4))))
    t.save()
    StateTrie.at(buffer, t.rootHash).read(key(5)) shouldBe Some(value(4)) // readable while buffered
    buffer.discard()
    buffer.buffering shouldBe false
    store.batches shouldBe 1
    (store.store.nodeCount, store.store.valueCount) shouldBe (nodes, values)
    buffer.readNode(t.rootHash) shouldBe None
    StateTrie.at(buffer, before.rootHash).read(key(1)) shouldBe Some(value(1))
  }
