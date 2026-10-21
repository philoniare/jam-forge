package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import scala.collection.mutable

/**
 * The per-key [[StateTrie.update]] as it was before batching, kept as a reference: it inserts or
 * deletes one key at a time, rehashing the leaf-to-root path for each. Only update, read and save are kept.
 */
final class LegacyStateTrie(backend: StateTrieBackend, initialRoot: Hash):

  private var currentRoot: Hash = initialRoot

  private val pendingNodes = mutable.HashMap[Hash, TrieNode]()
  private val pendingValues = mutable.HashMap[Hash, JamBytes]()
  private val nodeRefDeltas = mutable.HashMap[Hash, Long]()
  private val valueRefDeltas = mutable.HashMap[Hash, Long]()

  def rootHash: Hash = currentRoot

  def read(key: JamBytes): Option[JamBytes] =
    require(key.length == 31, s"key must be 31 bytes, got ${key.length}")
    findLeaf(currentRoot, key, depth = 0).flatMap { node =>
      node.nodeType match
        case TrieNodeType.EmbeddedLeaf =>
          TrieNode.embeddedValue(node)
        case TrieNodeType.RegularLeaf =>
          val vh = Hash.wrapUnchecked(node.right.toArrayUnsafe)
          pendingValues.get(vh).orElse(backend.readRawValue(vh))
        case TrieNodeType.Branch =>
          None
    }

  def update(updates: Seq[(JamBytes, Option[JamBytes])]): Unit =
    updates.foreach {
      case (k, Some(v)) => currentRoot = insert(currentRoot, k, v, 0)
      case (k, None)    => currentRoot = delete(currentRoot, k, 0)
    }

  def save(): Unit =
    val ops = new mutable.ArrayBuffer[BackendOp](
      pendingNodes.size + pendingValues.size + nodeRefDeltas.size + valueRefDeltas.size
    )
    pendingNodes.valuesIterator.foreach(n => ops += BackendOp.WriteNode(n))
    pendingValues.foreach((h, v) => ops += BackendOp.WriteRawValue(h, v))
    nodeRefDeltas.foreach((h, d) => ops += BackendOp.NodeRefDelta(h, d))
    valueRefDeltas.foreach((h, d) => ops += BackendOp.RawValueRefDelta(h, d))
    backend.batchUpdate(ops.toSeq)
    pendingNodes.clear()
    pendingValues.clear()
    nodeRefDeltas.clear()
    valueRefDeltas.clear()

  private def insert(rootHash: Hash, key: JamBytes, value: JamBytes, depth: Int): Hash =
    require(key.length == 31, s"key must be 31 bytes, got ${key.length}")
    getNode(rootHash) match
      case None =>
        emitLeaf(TrieNode.leaf(key, value), value)
      case Some(node) if node.nodeType == TrieNodeType.Branch =>
        val bit = bitAt(key, depth)
        val leftHash = Hash.wrapUnchecked(node.left.toArrayUnsafe)
        val rightHash = Hash.wrapUnchecked(node.right.toArrayUnsafe)
        val oldChildHash = if bit then rightHash else leftHash
        val newChildHash = insert(oldChildHash, key, value, depth + 1)
        // Same value written again: nothing below changed, so neither does this branch.
        if newChildHash == oldChildHash then rootHash
        else
          unrefNode(rootHash)
          if bit then writeBranch(leftHash, newChildHash)
          else writeBranch(newChildHash, rightHash)
      case Some(leaf) =>
        val existingKey = TrieNode.leafKey(leaf)
        if existingKey == key then
          val newLeaf = TrieNode.leaf(key, value)
          if newLeaf.hash == rootHash then rootHash
          else
            unrefNode(rootHash)
            emitLeaf(newLeaf, value)
        else
          // The existing leaf moves down under new branches unchanged, so its refcount does not change.
          splitLeaves(existingKey, rootHash, key, value, depth)

  private def splitLeaves(
      existingKey: JamBytes,
      existingHash: Hash,
      newKey: JamBytes,
      newValue: JamBytes,
      depth: Int,
  ): Hash =
    val existingBit = bitAt(existingKey, depth)
    val newBit = bitAt(newKey, depth)
    if existingBit != newBit then
      val newLeafHash = emitLeaf(TrieNode.leaf(newKey, newValue), newValue)
      if newBit then writeBranch(existingHash, newLeafHash)
      else writeBranch(newLeafHash, existingHash)
    else
      val childHash = splitLeaves(existingKey, existingHash, newKey, newValue, depth + 1)
      if existingBit then writeBranch(Hash.zero, childHash)
      else writeBranch(childHash, Hash.zero)

  private def delete(rootHash: Hash, key: JamBytes, depth: Int): Hash =
    require(key.length == 31, s"key must be 31 bytes, got ${key.length}")
    getNode(rootHash) match
      case None => rootHash
      case Some(node) if node.nodeType == TrieNodeType.Branch =>
        val bit = bitAt(key, depth)
        val leftHash = Hash.wrapUnchecked(node.left.toArrayUnsafe)
        val rightHash = Hash.wrapUnchecked(node.right.toArrayUnsafe)
        val newLeftHash = if bit then leftHash else delete(leftHash, key, depth + 1)
        val newRightHash = if bit then delete(rightHash, key, depth + 1) else rightHash
        if newLeftHash == leftHash && newRightHash == rightHash then
          rootHash
        else
          unrefNode(rootHash)
          collapseBranch(newLeftHash, newRightHash)
      case Some(leaf) =>
        if TrieNode.leafKey(leaf) == key then
          unrefNode(rootHash)
          Hash.zero
        else
          rootHash

  private def collapseBranch(leftHash: Hash, rightHash: Hash): Hash =
    val leftEmpty = leftHash == Hash.zero
    val rightEmpty = rightHash == Hash.zero
    if leftEmpty && rightEmpty then Hash.zero
    else if leftEmpty then
      getNode(rightHash) match
        case Some(n) if n.nodeType != TrieNodeType.Branch => rightHash
        case _ => writeBranch(leftHash, rightHash)
    else if rightEmpty then
      getNode(leftHash) match
        case Some(n) if n.nodeType != TrieNodeType.Branch => leftHash
        case _ => writeBranch(leftHash, rightHash)
    else
      writeBranch(leftHash, rightHash)

  private def writeBranch(leftHash: Hash, rightHash: Hash): Hash =
    val branch = TrieNode.branch(leftHash, rightHash)
    refNode(branch)
    branch.hash

  /** `leaf` must be `TrieNode.leaf(key, value)`; `value` is kept for a regular leaf, whose node holds only its hash. */
  private def emitLeaf(leaf: TrieNode, value: JamBytes): Hash =
    if refNode(leaf) && leaf.nodeType == TrieNodeType.RegularLeaf then
      refValue(Hash.wrap(leaf.right.toArrayUnsafe), value)
    leaf.hash

  /**
   * Counts a reference to `node` and queues it for writing. Returns false, writing nothing, when `node` was replaced
   * earlier in this batch and is now back: the backend still holds it at the refcount it had, with its raw value.
   */
  private def refNode(node: TrieNode): Boolean =
    val h = node.hash
    val d = nodeRefDeltas.getOrElse(h, 0L) + 1
    if d == 0 then
      nodeRefDeltas.remove(h)
      false
    else
      nodeRefDeltas.update(h, d)
      pendingNodes.update(h, node)
      true

  /**
   * Drops a reference to `h`. A node created since the last save that loses its only reference is discarded with its
   * raw value's reference, so the backend never sees either.
   */
  private def unrefNode(h: Hash): Unit =
    val d = nodeRefDeltas.getOrElse(h, 0L) - 1
    if d == 0 then
      nodeRefDeltas.remove(h)
      pendingNodes.remove(h).foreach { node =>
        if node.nodeType == TrieNodeType.RegularLeaf then unrefValue(Hash.wrapUnchecked(node.right.toArrayUnsafe))
      }
    else nodeRefDeltas.update(h, d)

  private def refValue(h: Hash, value: JamBytes): Unit =
    valueRefDeltas.update(h, valueRefDeltas.getOrElse(h, 0L) + 1)
    pendingValues.update(h, value)

  private def unrefValue(h: Hash): Unit =
    val d = valueRefDeltas.getOrElse(h, 0L) - 1
    if d == 0 then
      valueRefDeltas.remove(h)
      pendingValues.remove(h)
      ()
    else valueRefDeltas.update(h, d)

  def pendingNodeCount: Int = pendingNodes.size
  def pendingValueCount: Int = pendingValues.size

  private[trie] def getNode(hash: Hash): Option[TrieNode] =
    if hash == Hash.zero then None
    else pendingNodes.get(hash).orElse(backend.readNode(hash))

  private[trie] def findLeaf(hash: Hash, key: JamBytes, depth: Int): Option[TrieNode] =
    getNode(hash).flatMap { node =>
      node.nodeType match
        case TrieNodeType.Branch =>
          val childHash =
            if bitAt(key, depth) then Hash.wrapUnchecked(node.right.toArrayUnsafe)
            else Hash.wrapUnchecked(node.left.toArrayUnsafe)
          findLeaf(childHash, key, depth + 1)
        case _ =>
          if TrieNode.leafKey(node) == key then Some(node)
          else None
    }

  private[trie] def bitAt(data: JamBytes, position: Int): Boolean =
    val byteIndex = position / 8
    val bitIndex = 7 - (position % 8)
    if byteIndex >= data.length then false
    else (data(byteIndex) & (1 << bitIndex)) != 0

