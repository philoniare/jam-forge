package io.forge.jam.core.trie

import io.forge.jam.core.{JamBytes, Hashing}
import io.forge.jam.core.primitives.Hash
import scala.collection.mutable

final class InMemoryTrieBackend extends StateTrieBackend:
  private val nodes = mutable.Map[Hash, TrieNode]()
  private val values = mutable.Map[Hash, JamBytes]()
  private val nodeRefs = mutable.Map[Hash, Long]().withDefaultValue(0L)
  private val valueRefs = mutable.Map[Hash, Long]().withDefaultValue(0L)
  private val deadNodeCandidates = mutable.Set[Hash]()
  private val deadValueCandidates = mutable.Set[Hash]()

  def readNode(hash: Hash): Option[TrieNode] = nodes.get(hash)
  def readRawValue(hash: Hash): Option[JamBytes] = values.get(hash)

  def batchUpdate(ops: Seq[BackendOp]): Unit =
    ops.foreach {
      case BackendOp.WriteNode(node) =>
        nodes.update(node.hash, node)
      case BackendOp.WriteRawValue(v) =>
        values.update(Hashing.blake2b256(v), v)
      case BackendOp.NodeRefDelta(h, d) =>
        val c = nodeRefs(h) + d
        nodeRefs.update(h, c)
        if c <= 0 then deadNodeCandidates.add(h)
      case BackendOp.RawValueRefDelta(h, d) =>
        val c = valueRefs(h) + d
        valueRefs.update(h, c)
        if c <= 0 then deadValueCandidates.add(h)
    }

  def gc(): Unit =
    deadNodeCandidates.foreach { h =>
      if nodeRefs(h) <= 0 then
        nodes.remove(h).foreach { node =>
          if node.nodeType == TrieNodeType.RegularLeaf then
            val rawValueHash = Hash(node.right.toArray)
            val c = valueRefs(rawValueHash) - 1
            valueRefs.update(rawValueHash, c)
            if c <= 0 then deadValueCandidates.add(rawValueHash)
        }
        nodeRefs.remove(h)
    }
    deadNodeCandidates.clear()
    deadValueCandidates.foreach { h =>
      if valueRefs(h) <= 0 then
        values.remove(h)
        valueRefs.remove(h)
    }
    deadValueCandidates.clear()

  def clear(): Unit =
    nodes.clear()
    values.clear()
    nodeRefs.clear()
    valueRefs.clear()

  def nodeCount: Int = nodes.size
  def valueCount: Int = values.size
  def nodeRefcount(h: Hash): Long = nodeRefs.getOrElse(h, 0L)
  def valueRefcount(h: Hash): Long = valueRefs.getOrElse(h, 0L)
