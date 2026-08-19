package io.forge.jam.core.trie

import io.forge.jam.core.primitives.Hash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InMemoryTrieBackendSpec extends AnyFlatSpec with Matchers:

  private def h(i: Int): Hash = Hash(Array.fill[Byte](32)(i.toByte))

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
