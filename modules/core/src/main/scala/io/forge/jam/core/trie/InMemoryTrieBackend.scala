package io.forge.jam.core.trie

import io.forge.jam.core.{JamBytes, Hashing}
import io.forge.jam.core.primitives.Hash
import scala.collection.mutable

final class InMemoryTrieBackend extends StateTrieBackend:
  private val nodes = mutable.Map[Hash, TrieNode]()
  private val values = mutable.Map[Hash, JamBytes]()
  private val nodeRefs = mutable.Map[Hash, Long]().withDefaultValue(0L)
  private val valueRefs = mutable.Map[Hash, Long]().withDefaultValue(0L)

  def readNode(hash: Hash): Option[TrieNode] = nodes.get(hash)
  def readRawValue(hash: Hash): Option[JamBytes] = values.get(hash)

  def batchUpdate(ops: Seq[BackendOp]): Unit =
    ops.foreach {
      case BackendOp.WriteNode(node) =>
        nodes.update(node.hash, node)
      case BackendOp.WriteRawValue(v) =>
        values.update(Hashing.blake2b256(v), v)
      case BackendOp.NodeRefDelta(h, d) =>
        nodeRefs.update(h, nodeRefs(h) + d)
      case BackendOp.RawValueRefDelta(h, d) =>
        valueRefs.update(h, valueRefs(h) + d)
    }

  def gc(): Unit =
    val deadNodes = nodeRefs.collect { case (h, c) if c <= 0 => h }.toList
    deadNodes.foreach { h =>
      nodes.remove(h).foreach { node =>
        if node.nodeType == TrieNodeType.RegularLeaf then
          val rawValueHash = Hash(node.right.toArray)
          valueRefs.update(rawValueHash, valueRefs(rawValueHash) - 1)
      }
      nodeRefs.remove(h)
    }
    val deadValues = valueRefs.collect { case (h, c) if c <= 0 => h }.toList
    deadValues.foreach { h =>
      values.remove(h)
      valueRefs.remove(h)
    }

  def clear(): Unit =
    nodes.clear()
    values.clear()
    nodeRefs.clear()
    valueRefs.clear()

  def nodeCount: Int = nodes.size
  def valueCount: Int = values.size
  def nodeRefcount(h: Hash): Long = nodeRefs.getOrElse(h, 0L)
  def valueRefcount(h: Hash): Long = valueRefs.getOrElse(h, 0L)
