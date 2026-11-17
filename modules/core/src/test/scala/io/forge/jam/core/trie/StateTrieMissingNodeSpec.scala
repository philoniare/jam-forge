package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

/** A trie whose backend lost a node or value (pruned, or lost to a concurrent gc) fails on every read, walk and update
  * that reaches it, instead of taking the missing subtree or value as empty.
  */
class StateTrieMissingNodeSpec extends AnyFlatSpec with Matchers:

  /** Delegates to `inner`, except that the hidden nodes and values read as absent. */
  private final class LossyBackend(inner: StateTrieBackend) extends StateTrieBackend:
    var hidden: Set[Hash] = Set.empty
    def readNode(hash: Hash): Option[TrieNode] = if hidden(hash) then None else inner.readNode(hash)
    def readRawValue(hash: Hash): Option[JamBytes] = if hidden(hash) then None else inner.readRawValue(hash)
    def batchUpdate(ops: Seq[BackendOp]): Unit = inner.batchUpdate(ops)
    def gc(): Unit = inner.gc()
    def clear(): Unit = inner.clear()

  private def savedTrie(valueSize: Int): (StateTrie, LossyBackend, Int) =
    val rng = new Random(7)
    val backend = new LossyBackend(new InMemoryTrieBackend)
    val trie = StateTrie.empty(backend)
    val entries = Vector.fill(64)((JamBytes(rng.nextBytes(31)), JamBytes(rng.nextBytes(valueSize))))
    trie.update(entries.map((k, v) => (k, Some(v))))
    trie.save()
    (StateTrie.at(backend, trie.rootHash), backend, entries.size)

  private def firstChild(trie: StateTrie, branch: TrieNode): TrieNode =
    val left = Hash.wrapUnchecked(branch.left.toArrayUnsafe)
    trie.getNode(if left == Hash.zero then Hash.wrapUnchecked(branch.right.toArrayUnsafe) else left).get

  "StateTrie.getKeyValues" should "return every leaf when all nodes are present" in {
    val (trie, _, n) = savedTrie(valueSize = 40)
    trie.getKeyValues(JamBytes.empty, 0).size shouldBe n
  }

  it should "fail when a branch's child node is missing" in {
    val (trie, backend, _) = savedTrie(valueSize = 8)
    val root = trie.getNode(trie.rootHash).get
    root.nodeType shouldBe TrieNodeType.Branch
    backend.hidden = Set(firstChild(trie, root).hash)
    an[MissingTrieNodeException] should be thrownBy trie.getKeyValues(JamBytes.empty, 0)
  }

  it should "fail when a regular leaf's value is missing" in {
    val (trie, backend, _) = savedTrie(valueSize = 40)
    val leaf = Iterator
      .iterate(trie.getNode(trie.rootHash).get)(n => firstChild(trie, n))
      .dropWhile(_.nodeType == TrieNodeType.Branch)
      .next()
    leaf.nodeType shouldBe TrieNodeType.RegularLeaf
    backend.hidden = Set(Hash.wrapUnchecked(leaf.right.toArrayUnsafe))
    an[MissingTrieNodeException] should be thrownBy trie.getKeyValues(JamBytes.empty, 0)
  }

  private def rootChildHidden(): (StateTrie, LossyBackend, Hash, JamBytes, JamBytes) =
    val rng = new Random(11)
    val backend = new LossyBackend(new InMemoryTrieBackend)
    val trie = StateTrie.empty(backend)
    val entries = Vector.fill(64)((JamBytes(rng.nextBytes(31)), JamBytes(rng.nextBytes(40))))
    trie.update(entries.map((k, v) => (k, Some(v))))
    trie.save()
    val saved = StateTrie.at(backend, trie.rootHash)
    val root = saved.getNode(saved.rootHash).get
    val left = Hash.wrapUnchecked(root.left.toArrayUnsafe)
    left should not be Hash.zero
    backend.hidden = Set(left)
    val (under, other) = entries.map(_._1).partition(k => !saved.bitAt(k, 0))
    (saved, backend, left, under.head, other.head)

  "A StateTrie over a missing node" should "fail a read, not read the key as absent" in {
    val (trie, _, missing, under, other) = rootChildHidden()
    val e = the[MissingTrieNodeException] thrownBy trie.read(under)
    e.hash shouldBe missing
    a[MissingTrieNodeException] should be thrownBy trie.valueHash(under)
    trie.read(other) shouldBe defined // the other side is whole
  }

  it should "fail an update that passes through it, not rebuild it as empty" in {
    val (trie, _, missing, under, _) = rootChildHidden()
    val e = the[MissingTrieNodeException] thrownBy trie.update(Seq(under -> Some(JamBytes(Array[Byte](1)))))
    e.hash shouldBe missing
    a[MissingTrieNodeException] should be thrownBy trie.update(Seq(under -> None))
  }

  it should "fail a range read that reaches it, not skip it" in {
    val (trie, _, _, _, _) = rootChildHidden()
    a[MissingTrieNodeException] should be thrownBy
      trie.range(JamBytes(new Array[Byte](31)), JamBytes(Array.fill[Byte](31)(-1)), 1 << 20)
  }

  it should "fail a read of a regular leaf whose value is missing, not read it as absent" in {
    val (trie, backend, _) = savedTrie(valueSize = 40)
    val leaf = Iterator
      .iterate(trie.getNode(trie.rootHash).get)(n => firstChild(trie, n))
      .dropWhile(_.nodeType == TrieNodeType.Branch)
      .next()
    backend.hidden = Set(Hash.wrapUnchecked(leaf.right.toArrayUnsafe))
    a[MissingTrieNodeException] should be thrownBy trie.read(TrieNode.leafKey(leaf))
  }
