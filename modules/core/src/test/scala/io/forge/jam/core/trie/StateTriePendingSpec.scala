package io.forge.jam.core.trie

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.util.Random

/** What a [[StateTrie]] keeps between saves, and what it leaves in the backend after save and gc. */
class StateTriePendingSpec extends AnyFlatSpec with Matchers:

  private def key(rng: Random): JamBytes = JamBytes(rng.nextBytes(31))

  // Values from 0 to 64 bytes, so both embedded (<= 32) and regular leaves occur.
  private def value(rng: Random): JamBytes = JamBytes(rng.nextBytes(rng.nextInt(65)))

  private def reachable(trie: StateTrie): Vector[TrieNode] =
    val out = Vector.newBuilder[TrieNode]
    val stack = mutable.Stack(trie.rootHash)
    while stack.nonEmpty do
      trie.getNode(stack.pop()).foreach { node =>
        out += node
        if node.nodeType == TrieNodeType.Branch then
          stack.push(Hash.wrapUnchecked(node.left.toArrayUnsafe))
          stack.push(Hash.wrapUnchecked(node.right.toArrayUnsafe))
      }
    out.result()

  private def rootOf(model: collection.Map[JamBytes, JamBytes]): Hash =
    val trie = StateTrie.empty(new InMemoryTrieBackend)
    trie.update(model.toSeq.map((k, v) => (k, Some(v))))
    trie.rootHash

  "StateTrie" should "keep only the new nodes it can still reach pending within one batch" in {
    val rng = new Random(1)
    val trie = StateTrie.empty(new InMemoryTrieBackend)
    val entries = Vector.fill(5000)((key(rng), JamBytes(rng.nextBytes(40))))
    trie.update(entries.map((k, v) => (k, Some(v))))

    val nodes = reachable(trie)
    trie.pendingNodeCount shouldBe nodes.size
    trie.pendingValueCount shouldBe entries.size
  }

  it should "drop a pending leaf and its raw value when the key is deleted before save" in {
    val rng = new Random(2)
    val trie = StateTrie.empty(new InMemoryTrieBackend)
    val k = key(rng)
    trie.update(Seq((k, Some(JamBytes(rng.nextBytes(40))))))
    trie.update(Seq((k, None)))

    trie.rootHash shouldBe Hash.zero
    trie.pendingNodeCount shouldBe 0
    trie.pendingValueCount shouldBe 0
  }

  it should "queue nothing when keys are set to the values they already hold" in {
    val rng = new Random(3)
    val backend = new InMemoryTrieBackend
    val trie = StateTrie.empty(backend)
    val entries = Vector.fill(500)((key(rng), value(rng)))
    trie.update(entries.map((k, v) => (k, Some(v))))
    trie.save()
    val root = trie.rootHash

    trie.update(entries.map((k, v) => (k, Some(v))))

    trie.rootHash shouldBe root
    trie.pendingNodeCount shouldBe 0
    trie.pendingValueCount shouldBe 0
  }

  it should "leave a backend fed BackendOps in the same state as an in-memory one fed its pending tables" in {
    // Hides the in-memory type, so save() goes through batchUpdate rather than absorb.
    final class Opaque(val inner: InMemoryTrieBackend) extends StateTrieBackend:
      def readNode(hash: Hash): Option[TrieNode] = inner.readNode(hash)
      def readRawValue(hash: Hash): Option[JamBytes] = inner.readRawValue(hash)
      def batchUpdate(ops: Seq[BackendOp]): Unit = inner.batchUpdate(ops)
      def gc(): Unit = inner.gc()
      def clear(): Unit = inner.clear()

    val rng = new Random(5)
    val direct = new InMemoryTrieBackend
    val viaOps = new Opaque(new InMemoryTrieBackend)
    var keys = Vector.empty[JamBytes]
    var rootA = Hash.zero
    var rootB = Hash.zero
    for _ <- 1 to 30 do
      val updates = Vector.fill(150) {
        if keys.nonEmpty && rng.nextInt(3) == 0 then
          (keys(rng.nextInt(keys.size)), if rng.nextInt(4) == 0 then None else Some(value(rng)))
        else
          val k = key(rng)
          keys :+= k
          (k, Some(value(rng)))
      }
      val a = StateTrie.at(direct, rootA)
      a.update(updates)
      a.save()
      val b = StateTrie.at(viaOps, rootB)
      b.update(updates)
      b.save()
      rootA = a.rootHash
      rootB = b.rootHash
      rootA shouldBe rootB
      reachable(StateTrie.at(direct, rootA)).foreach { n =>
        direct.nodeRefcount(n.hash) shouldBe viaOps.inner.nodeRefcount(n.hash)
      }
      direct.nodeCount shouldBe viaOps.inner.nodeCount
      direct.valueCount shouldBe viaOps.inner.valueCount
      direct.pendingDeadCandidates shouldBe viaOps.inner.pendingDeadCandidates
      direct.gc()
      viaOps.gc()
      direct.nodeCount shouldBe viaOps.inner.nodeCount
  }

  it should "leave the backend holding exactly the reachable nodes and values, each counted once" in {
    val rng = new Random(4)
    val backend = new InMemoryTrieBackend
    val model = mutable.LinkedHashMap[JamBytes, JamBytes]()
    var root = Hash.zero

    for block <- 1 to 40 do
      val trie = StateTrie.at(backend, root)
      val updates = mutable.ArrayBuffer[(JamBytes, Option[JamBytes])]()
      def put(k: JamBytes, v: JamBytes): Unit = { updates += ((k, Some(v))); model(k) = v }
      def del(k: JamBytes): Unit = { updates += ((k, None)); model.remove(k); () }
      val existing = model.keys.toVector
      for _ <- 1 to 200 do
        rng.nextInt(6) match
          case 0 | 1 => put(key(rng), value(rng)) // new key
          case 2 if existing.nonEmpty => put(existing(rng.nextInt(existing.size)), value(rng)) // new value
          case 3 if existing.nonEmpty => // same value again
            val k = existing(rng.nextInt(existing.size))
            model.get(k).foreach(put(k, _))
          case 4 if existing.nonEmpty => del(existing(rng.nextInt(existing.size)))
          case _ => // a key added and removed, or removed and restored, within the batch
            val k = if existing.nonEmpty && rng.nextBoolean() then existing(rng.nextInt(existing.size)) else key(rng)
            val before = model.get(k)
            put(k, value(rng))
            del(k)
            before.foreach(put(k, _))
      // One value under several keys, so a raw value held by more than one leaf is covered.
      if block % 5 == 0 then
        val shared = JamBytes(rng.nextBytes(48))
        for _ <- 1 to 3 do put(key(rng), shared)
      trie.update(updates.toSeq)
      trie.save()
      backend.gc()
      root = trie.rootHash

      withClue(s"block $block: ") {
        root shouldBe rootOf(model)
        model.foreach((k, v) => trie.read(k) shouldBe Some(v))

        val nodes = reachable(StateTrie.at(backend, root))
        backend.nodeCount shouldBe nodes.size
        nodes.foreach(n => backend.nodeRefcount(n.hash) shouldBe 1L)

        val leavesPerValue = nodes
          .filter(_.nodeType == TrieNodeType.RegularLeaf)
          .groupBy(n => Hash.wrapUnchecked(n.right.toArrayUnsafe))
        backend.valueCount shouldBe leavesPerValue.size
        leavesPerValue.foreach { (vh, leaves) =>
          backend.valueRefcount(vh) shouldBe leaves.size.toLong
          backend.readRawValue(vh).map(Hashing.blake2b256) shouldBe Some(vh)
        }
      }
}
