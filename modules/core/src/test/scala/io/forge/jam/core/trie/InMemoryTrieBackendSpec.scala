package io.forge.jam.core.trie

import io.forge.jam.core.{JamBytes, Hashing}
import io.forge.jam.core.primitives.Hash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InMemoryTrieBackendSpec extends AnyFlatSpec with Matchers:

  private def h(i: Int): Hash = Hash(Array.fill[Byte](32)(i.toByte))
  private def v(i: Int): JamBytes = JamBytes(Array.fill[Byte](40)(i.toByte))

  "InMemoryTrieBackend.clear" should "reset the pending dead-candidate sets (SER-10)" in {
    val backend = new InMemoryTrieBackend
    // A negative ref delta pushes a hash to <= 0, registering it as a dead candidate.
    backend.batchUpdate(Seq(
      BackendOp.NodeRefDelta(h(1), -1L),
      BackendOp.RawValueRefDelta(h(2), -1L)
    ))
    backend.pendingDeadCandidates shouldBe 2

    backend.clear()

    backend.pendingDeadCandidates shouldBe 0
    backend.nodeCount shouldBe 0
    backend.valueCount shouldBe 0
  }

  "InMemoryTrieBackend.gc" should "reclaim only dead nodes/values and keep live ones" in {
    val backend = new InMemoryTrieBackend
    val live = TrieNode.branch(Hash.zero, Hash.zero)
    val dead = TrieNode.branch(h(7), Hash.zero)
    val liveVal = v(1)
    val deadVal = v(2)
    val liveValHash = Hashing.blake2b256(liveVal)
    val deadValHash = Hashing.blake2b256(deadVal)

    backend.batchUpdate(Seq(
      BackendOp.WriteNode(live), BackendOp.NodeRefDelta(live.hash, 1L),
      BackendOp.WriteNode(dead), BackendOp.NodeRefDelta(dead.hash, 1L),
      BackendOp.WriteRawValue(liveVal), BackendOp.RawValueRefDelta(liveValHash, 1L),
      BackendOp.WriteRawValue(deadVal), BackendOp.RawValueRefDelta(deadValHash, 1L)
    ))
    backend.nodeCount shouldBe 2
    backend.valueCount shouldBe 2

    // Drop the "dead" refs to zero, then collect.
    backend.batchUpdate(Seq(
      BackendOp.NodeRefDelta(dead.hash, -1L),
      BackendOp.RawValueRefDelta(deadValHash, -1L)
    ))
    backend.gc()

    backend.nodeCount shouldBe 1
    backend.valueCount shouldBe 1
    backend.readNode(live.hash) shouldBe defined
    backend.readNode(dead.hash) shouldBe empty
    backend.readRawValue(liveValHash) shouldBe defined
    backend.readRawValue(deadValHash) shouldBe empty
    backend.pendingDeadCandidates shouldBe 0 // candidates consumed
  }

  it should "not reclaim a node resurrected above zero before gc" in {
    val backend = new InMemoryTrieBackend
    val node = TrieNode.branch(h(9), h(9))

    // refcount -> -1 registers a dead candidate...
    backend.batchUpdate(Seq(
      BackendOp.WriteNode(node), BackendOp.NodeRefDelta(node.hash, -1L)
    ))
    backend.pendingDeadCandidates should be > 0
    // ...but a later delta resurrects it above zero before gc runs.
    backend.batchUpdate(Seq(BackendOp.NodeRefDelta(node.hash, 2L)))

    backend.gc()

    backend.readNode(node.hash) shouldBe defined // re-validated, not reclaimed
    backend.nodeRefcount(node.hash) shouldBe 1L
    backend.pendingDeadCandidates shouldBe 0
  }
