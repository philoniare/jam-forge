package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

/** A walk over a trie whose backend lost a node (e.g. to a concurrent gc) fails instead of returning fewer leaves. */
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
    an[IllegalStateException] should be thrownBy trie.getKeyValues(JamBytes.empty, 0)
  }

  it should "fail when a regular leaf's value is missing" in {
    val (trie, backend, _) = savedTrie(valueSize = 40)
    val leaf = Iterator
      .iterate(trie.getNode(trie.rootHash).get)(n => firstChild(trie, n))
      .dropWhile(_.nodeType == TrieNodeType.Branch)
      .next()
    leaf.nodeType shouldBe TrieNodeType.RegularLeaf
    backend.hidden = Set(Hash.wrapUnchecked(leaf.right.toArrayUnsafe))
    an[IllegalStateException] should be thrownBy trie.getKeyValues(JamBytes.empty, 0)
  }
