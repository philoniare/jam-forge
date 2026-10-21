package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import scala.collection.mutable

/**
 * Binary Patricia Merkle state trie, with writes buffered until [[save]].
 *
 * Between saves the trie holds only what the backend will need: a node created and then replaced before `save()` is
 * dropped at once, with its raw value if no other pending leaf shares it. So the pending set stays the size of the
 * nodes that are new and still reachable, however many times one batch rewrites the same paths.
 *
 * Invariants between calls: a hash is in `pendingNodes` iff its ref delta is positive, and a raw value is in
 * `pendingValues` iff its ref delta is positive (one per pending regular leaf that holds it). Zero deltas are not kept.
 */
final class StateTrie private (backend: StateTrieBackend, initialRoot: Hash):

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

  /** Nodes queued for the next [[save]]; for tests. */
  private[trie] def pendingNodeCount: Int = pendingNodes.size

  /** Raw values queued for the next [[save]]; for tests. */
  private[trie] def pendingValueCount: Int = pendingValues.size

  def getKeyValues(prefix: JamBytes, bitsCount: Int): List[(JamBytes, JamBytes)] =
    require(bitsCount >= 0 && bitsCount <= 248, "bitsCount must be in [0, 248]")
    findSubtreeRoot(currentRoot, prefix, bitsCount, depth = 0) match
      case None => Nil
      case Some(node) =>
        val buf = mutable.ArrayBuffer[(JamBytes, JamBytes)]()
        collectLeaves(node, buf)
        buf.toList

  private def findSubtreeRoot(hash: Hash, prefix: JamBytes, bitsCount: Int, depth: Int): Option[TrieNode] =
    getNode(hash).flatMap { node =>
      if depth >= bitsCount then Some(node)
      else
        node.nodeType match
          case TrieNodeType.Branch =>
            val childHash =
              if bitAt(prefix, depth) then Hash.wrapUnchecked(node.right.toArrayUnsafe)
              else Hash.wrapUnchecked(node.left.toArrayUnsafe)
            findSubtreeRoot(childHash, prefix, bitsCount, depth + 1)
          case _ =>
            val leafKey = TrieNode.leafKey(node)
            if prefixMatches(leafKey, prefix, bitsCount) then Some(node) else None
    }

  def prefixMatches(key: JamBytes, prefix: JamBytes, bitsCount: Int): Boolean =
    var i = 0
    while i < bitsCount do
      if bitAt(key, i) != bitAt(prefix, i) then return false
      i += 1
    true

  private def collectLeaves(node: TrieNode, buf: mutable.ArrayBuffer[(JamBytes, JamBytes)]): Unit =
    node.nodeType match
      case TrieNodeType.Branch =>
        getNode(Hash.wrapUnchecked(node.left.toArrayUnsafe)).foreach(collectLeaves(_, buf))
        getNode(Hash.wrapUnchecked(node.right.toArrayUnsafe)).foreach(collectLeaves(_, buf))
      case TrieNodeType.EmbeddedLeaf =>
        TrieNode.embeddedValue(node).foreach { v =>
          buf += ((TrieNode.leafKey(node), v))
        }
      case TrieNodeType.RegularLeaf =>
        val vh = Hash.wrapUnchecked(node.right.toArrayUnsafe)
        pendingValues.get(vh).orElse(backend.readRawValue(vh)).foreach { v =>
          buf += ((TrieNode.leafKey(node), v))
        }

  def range(start: JamBytes, end: JamBytes, maxSize: Int): (List[TrieNode], List[(JamBytes, JamBytes)]) =
    require(start.length == 31, s"start must be 31 bytes, got ${start.length}")
    require(end.length == 31, s"end must be 31 bytes, got ${end.length}")

    val pairs = mutable.ArrayBuffer[(JamBytes, JamBytes)]()
    var accumulated = 0L
    var stop = false
    var lastIncludedKey: Option[JamBytes] = None

    def leafValue(node: TrieNode): Option[JamBytes] =
      node.nodeType match
        case TrieNodeType.EmbeddedLeaf => TrieNode.embeddedValue(node)
        case TrieNodeType.RegularLeaf =>
          val vh = Hash.wrapUnchecked(node.right.toArrayUnsafe)
          pendingValues.get(vh).orElse(backend.readRawValue(vh))
        case TrieNodeType.Branch => None

    def visit(hash: Hash, depth: Int, boundedByStart: Boolean): Unit =
      if !stop then
        getNode(hash) match
          case None => ()
          case Some(node) if node.nodeType == TrieNodeType.Branch =>
            val leftHash = Hash.wrapUnchecked(node.left.toArrayUnsafe)
            val rightHash = Hash.wrapUnchecked(node.right.toArrayUnsafe)
            if boundedByStart && bitAt(start, depth) then
              // start's bit here is 1: the whole left (bit=0) subtree is < start.
              visit(rightHash, depth + 1, boundedByStart = true)
            else
              visit(leftHash, depth + 1, boundedByStart)
              if !stop then visit(rightHash, depth + 1, boundedByStart = false)
          case Some(leaf) =>
            val key = TrieNode.leafKey(leaf)
            if boundedByStart && JamBytes.compareUnsigned(key.toArray, start.toArray) < 0 then
              () // before the range; skip
            else if JamBytes.compareUnsigned(key.toArray, end.toArray) > 0 then
              stop = true
            else
              leafValue(leaf).foreach { v =>
                if pairs.isEmpty || accumulated + v.length <= maxSize then
                  pairs += ((key, v))
                  accumulated += v.length
                  lastIncludedKey = Some(key)
                else
                  stop = true
              }

    visit(currentRoot, depth = 0, boundedByStart = true)

    def descentPath(key: JamBytes): List[TrieNode] =
      val path = mutable.ArrayBuffer[TrieNode]()
      var cur = currentRoot
      var depth = 0
      var continue = true
      while continue do
        getNode(cur) match
          case Some(node) if node.nodeType == TrieNodeType.Branch =>
            path += node
            cur =
              if bitAt(key, depth) then Hash.wrapUnchecked(node.right.toArrayUnsafe)
              else Hash.wrapUnchecked(node.left.toArrayUnsafe)
            depth += 1
          case _ => continue = false
      path.toList

    val boundaryNodes = mutable.LinkedHashSet[TrieNode]()
    descentPath(start).foreach(boundaryNodes += _)
    lastIncludedKey.foreach(k => descentPath(k).foreach(boundaryNodes += _))

    (boundaryNodes.toList, pairs.toList)

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

object StateTrie:
  def empty(backend: StateTrieBackend): StateTrie = new StateTrie(backend, Hash.zero)
  def at(backend: StateTrieBackend, root: Hash): StateTrie = new StateTrie(backend, root)
