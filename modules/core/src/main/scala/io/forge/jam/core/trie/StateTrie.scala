package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import scala.collection.mutable

/**
 * Binary Patricia Merkle state trie, with writes buffered until [[save]].
 *
 * Between saves the trie holds only what the backend will need: each [[update]] rebuilds a touched node once, and a
 * node created and then replaced by a later update before `save()` is dropped at once, with its raw value if no other
 * pending leaf shares it. So the pending set stays the size of the nodes that are new and still reachable.
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

  /**
   * Applies `updates` (a later update to a key overrides an earlier one) as one batch: the keys are sorted, the trie is
   * descended once with the batch split by key bit at each branch, and every node on a touched path is rebuilt, hashed
   * and counted once, however many keys pass through it. The result is the canonical trie of the final key set
   * the same root and ref deltas as applying the updates one key at a time.
   */
  def update(updates: Seq[(JamBytes, Option[JamBytes])]): Unit =
    if updates.nonEmpty then
      val batch = StateTrie.sortedBatch(updates)
      currentRoot = updateSubtree(currentRoot, 0, batch, 0, batch.length)

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

  /** Rewrites the subtree at `hash` (at `depth`) with `batch(lo until hi)`, whose keys all lead to it. */
  private def updateSubtree(hash: Hash, depth: Int, batch: Array[StateTrie.Update], lo: Int, hi: Int): Hash =
    if lo == hi then hash
    else
      getNode(hash) match
        case None =>
          buildFromBatch(depth, batch, lo, hi, existing = null, existingIndex = lo, existingHash = Hash.zero)
        case Some(node) if node.nodeType == TrieNodeType.Branch =>
          val mid = StateTrie.firstWithBit(batch, lo, hi, depth)
          val leftHash = Hash.wrapUnchecked(node.left.toArrayUnsafe)
          val rightHash = Hash.wrapUnchecked(node.right.toArrayUnsafe)
          val newLeft = updateSubtree(leftHash, depth + 1, batch, lo, mid)
          val newRight = updateSubtree(rightHash, depth + 1, batch, mid, hi)
          // Same values written again: nothing below changed, so neither does this branch.
          if newLeft == leftHash && newRight == rightHash then hash
          else
            unrefNode(hash)
            collapseBranch(newLeft, newRight)
        case Some(leaf) =>
          // Where the leaf's key sits in the batch: its update if it has one, else its sorted position.
          val leafArr = leaf.left.toArrayUnsafe
          var a = lo
          var b = hi
          while a < b do
            val m = (a + b) >>> 1
            if java.util.Arrays.compareUnsigned(batch(m).key, 0, 31, leafArr, 1, 32) < 0 then a = m + 1 else b = m
          val updated = a < hi && java.util.Arrays.equals(batch(a).key, 0, 31, leafArr, 1, 32)
          if !updated then buildFromBatch(depth, batch, lo, hi, existing = leaf, existingIndex = a, existingHash = hash)
          else
            val kept = batch(a).value match
              case Some(v) => TrieNode.leaf(batch(a).keyBytes, v).hash == hash
              case None => false
            if kept then buildFromBatch(depth, batch, lo, hi, existing = leaf, existingIndex = a, existingHash = hash)
            else
              unrefNode(hash)
              buildFromBatch(depth, batch, lo, hi, existing = null, existingIndex = lo, existingHash = Hash.zero)

  /**
   * Builds the subtree at `depth` holding the inserted keys of `batch(lo until hi)` plus, when non-null, the unchanged
   * leaf `existing` (hash `existingHash`, sorting at `existingIndex`; an update to its own key there is skipped). A
   * lone remaining leaf is the subtree; two or more make a branch, even with one side empty (GP 0.8.0 §D.2, M).
   */
  private def buildFromBatch(
      depth: Int,
      batch: Array[StateTrie.Update],
      lo: Int,
      hi: Int,
      existing: TrieNode | Null,
      existingIndex: Int,
      existingHash: Hash,
  ): Hash =
    val items = new mutable.ArrayBuffer[StateTrie.Update](hi - lo + 1)
    var i = lo
    while i <= hi do
      existing match
        case leaf: TrieNode if i == existingIndex =>
          items += StateTrie.Update(TrieNode.leafKeyArray(leaf), null, None, existingHash)
          if i < hi && java.util.Arrays.equals(batch(i).key, 0, 31, leaf.left.toArrayUnsafe, 1, 32) then i += 1
        case _ => ()
      if i < hi && batch(i).value.isDefined then items += batch(i)
      i += 1
    val arr = items.toArray
    buildItems(depth, arr, 0, arr.length)

  private def buildItems(depth: Int, items: Array[StateTrie.Update], lo: Int, hi: Int): Hash =
    if lo == hi then Hash.zero
    else if hi - lo == 1 then
      val item = items(lo)
      if item.existing != null then item.existing
      else
        val v = item.value.get
        emitLeaf(TrieNode.leaf(item.keyBytes, v), v)
    else
      val mid = StateTrie.firstWithBit(items, lo, hi, depth)
      writeBranch(buildItems(depth + 1, items, lo, mid), buildItems(depth + 1, items, mid, hi))

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

  /** Fails on a node or value the trie refers to but the backend no longer holds (e.g. removed by a concurrent gc),
    * so a walk never returns a silently truncated state.
    */
  private def collectLeaves(node: TrieNode, buf: mutable.ArrayBuffer[(JamBytes, JamBytes)]): Unit =
    node.nodeType match
      case TrieNodeType.Branch =>
        collectChild(Hash.wrapUnchecked(node.left.toArrayUnsafe), buf)
        collectChild(Hash.wrapUnchecked(node.right.toArrayUnsafe), buf)
      case TrieNodeType.EmbeddedLeaf =>
        TrieNode.embeddedValue(node).foreach { v =>
          buf += ((TrieNode.leafKey(node), v))
        }
      case TrieNodeType.RegularLeaf =>
        val vh = Hash.wrapUnchecked(node.right.toArrayUnsafe)
        val v = pendingValues.get(vh).orElse(backend.readRawValue(vh)).getOrElse(
          throw new IllegalStateException(s"State trie value ${vh.toHex} is missing")
        )
        buf += ((TrieNode.leafKey(node), v))

  private def collectChild(hash: Hash, buf: mutable.ArrayBuffer[(JamBytes, JamBytes)]): Unit =
    if hash != Hash.zero then
      val child = getNode(hash).getOrElse(throw new IllegalStateException(s"State trie node ${hash.toHex} is missing"))
      collectLeaves(child, buf)

  /** JAMNP-S CE 129 range read: the key/value pairs with keys in
    * `[start, end]`, in key order, plus the boundary nodes on the paths from
    * the root to `start` and to the last pair returned (parents first, no
    * duplicates). A path ends at the leaf it reaches, whatever its key, or
    * at the branch whose child on the key's side is empty.
    *
    * The response's encoded size (64 bytes per boundary node, 31-byte key,
    * compact length and value per pair) stays within `maxSize` unless it
    * holds a single pair. The bound is checked against each path's depth,
    * an upper bound on what the deduplicated node list adds.
    */
  def range(start: JamBytes, end: JamBytes, maxSize: Int): (List[TrieNode], List[(JamBytes, JamBytes)]) =
    require(start.length == 31, s"start must be 31 bytes, got ${start.length}")
    require(end.length == 31, s"end must be 31 bytes, got ${end.length}")

    val pairs = mutable.ArrayBuffer[(JamBytes, JamBytes)]()
    var pairBytes = 0L
    var stop = false
    var lastIncludedKey: Option[JamBytes] = None

    def leafValue(node: TrieNode): Option[JamBytes] =
      node.nodeType match
        case TrieNodeType.EmbeddedLeaf => TrieNode.embeddedValue(node)
        case TrieNodeType.RegularLeaf =>
          val vh = Hash.wrapUnchecked(node.right.toArrayUnsafe)
          pendingValues.get(vh).orElse(backend.readRawValue(vh))
        case TrieNodeType.Branch => None

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
          case Some(leaf) =>
            path += leaf
            continue = false
          case None => continue = false
      path.toList

    val startPath = descentPath(start)
    val startPathBytes = startPath.size.toLong * TrieNode.GpEncodingBytes

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
            if boundedByStart && JamBytes.compareUnsigned(key.toArrayUnsafe, start.toArrayUnsafe) < 0 then
              () // before the range; skip
            else if JamBytes.compareUnsigned(key.toArrayUnsafe, end.toArrayUnsafe) > 0 then
              stop = true
            else
              leafValue(leaf).foreach { v =>
                val entry = 31L + compactLength(v.length.toLong) + v.length
                val lastPathBytes = (depth + 1).toLong * TrieNode.GpEncodingBytes
                if pairs.isEmpty || startPathBytes + lastPathBytes + pairBytes + entry <= maxSize then
                  pairs += ((key, v))
                  pairBytes += entry
                  lastIncludedKey = Some(key)
                else
                  stop = true
              }

    visit(currentRoot, depth = 0, boundedByStart = true)

    val boundaryNodes = mutable.LinkedHashSet[TrieNode]()
    startPath.foreach(boundaryNodes += _)
    lastIncludedKey.foreach(k => descentPath(k).foreach(boundaryNodes += _))

    (boundaryNodes.toList, pairs.toList)

  private def compactLength(v: Long): Int =
    io.forge.jam.core.codec.JamCodecs.encodeCompactInteger(v).length

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
          if TrieNode.leafKeyEquals(node, key) then Some(node)
          else None
    }

  private[trie] def bitAt(data: JamBytes, position: Int): Boolean =
    val byteIndex = position / 8
    val bitIndex = 7 - (position % 8)
    if byteIndex >= data.length then false
    else (data(byteIndex) & (1 << bitIndex)) != 0

object StateTrie:

  /** One key of a batch: an update (`existing` null; `value` None deletes) or a kept leaf being rebuilt around. */
  private[trie] final class Update(
      val key: Array[Byte],
      val keyBytes: JamBytes,
      val value: Option[JamBytes],
      val existing: Hash,
  )

  private val updateOrder: java.util.Comparator[Update] =
    (a, b) => java.util.Arrays.compareUnsigned(a.key, b.key)

  /** The updates sorted by key, keeping only the last update to each key. */
  private def sortedBatch(updates: Seq[(JamBytes, Option[JamBytes])]): Array[Update] =
    val arr = new Array[Update](updates.size)
    var n = 0
    updates.foreach { (k, v) =>
      require(k.length == 31, s"key must be 31 bytes, got ${k.length}")
      arr(n) = Update(k.toArrayUnsafe, k, v, null)
      n += 1
    }
    java.util.Arrays.sort(arr, updateOrder) // stable, so equal keys stay in update order
    var kept = 0
    var i = 0
    while i < arr.length do
      if i + 1 == arr.length || !java.util.Arrays.equals(arr(i).key, arr(i + 1).key) then
        arr(kept) = arr(i)
        kept += 1
      i += 1
    if kept == arr.length then arr else java.util.Arrays.copyOf(arr, kept)

  /** First index in `items(lo until hi)`, sorted and sharing their first `depth` bits, whose bit `depth` is set. */
  private def firstWithBit(items: Array[Update], lo: Int, hi: Int, depth: Int): Int =
    val byteIndex = depth >>> 3
    val mask = 0x80 >>> (depth & 7)
    var a = lo
    var b = hi
    while a < b do
      val m = (a + b) >>> 1
      if (items(m).key(byteIndex) & mask) != 0 then b = m else a = m + 1
    a

  def empty(backend: StateTrieBackend): StateTrie = new StateTrie(backend, Hash.zero)
  def at(backend: StateTrieBackend, root: Hash): StateTrie = new StateTrie(backend, root)
