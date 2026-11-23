package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.HashTables.{NodeTable, ValueTable}
import scala.collection.mutable

final class MissingTrieNodeException(val hash: Hash, val what: String)
    extends IllegalStateException(s"State trie $what ${hash.toHex} is missing")

/**
 * Binary Patricia Merkle state trie, with writes buffered until [[save]].
 *
 * Between saves the trie holds only what the backend will need: each [[update]] rebuilds a touched node once, and a
 * node created and then replaced by a later update before `save()` is dropped at once, with its raw value if no other
 * pending leaf shares it. So the pending set stays the size of the nodes that are new and still reachable.
 *
 * Pending changes live in flat hash tables ([[HashTables]]): a slot's count is the hash's ref delta since the last
 * save, and its payload, when present, is the node or raw value queued for writing.
 *
 * Invariants between calls: a node is queued iff its ref delta is positive, and a raw value is queued iff its ref
 * delta is positive (one per queued regular leaf that holds it). Zero deltas are not kept.
 */
final class StateTrie private (backend: StateTrieBackend, initialRoot: Hash):

  private var currentRoot: Hash = initialRoot

  private val pendingNodes = new NodeTable
  private val pendingValues = new ValueTable

  private var children: Array[StateTrie] | Null = null
  private var doneByBatchIndex: java.util.HashMap[Integer, Hash] | Null = null
  private var doneByItemIndex: java.util.HashMap[Integer, Hash] | Null = null

  def rootHash: Hash = currentRoot

  def read(key: JamBytes): Option[JamBytes] =
    require(key.length == 31, s"key must be 31 bytes, got ${key.length}")
    findLeaf(currentRoot, key, depth = 0).flatMap { node =>
      node.nodeType match
        case TrieNodeType.EmbeddedLeaf =>
          TrieNode.embeddedValue(node)
        case TrieNodeType.RegularLeaf =>
          Some(pendingOrStoredValue(Hash.wrapUnchecked(node.right.toArrayUnsafe)))
        case TrieNodeType.Branch =>
          None
    }

  def valueHash(key: JamBytes): Option[Hash] =
    require(key.length == 31, s"key must be 31 bytes, got ${key.length}")
    findLeaf(currentRoot, key, depth = 0).flatMap { node =>
      if node.nodeType == TrieNodeType.RegularLeaf then Some(Hash.wrapUnchecked(node.right.toArrayUnsafe)) else None
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
      if backend.readsInBulk && batch.length >= StateTrie.PrefetchFrom then prefetch(batch)
      if splits(batch.length) && currentRoot != Hash.zero then
        val tasks = new mutable.ArrayBuffer[(Hash, Int, Int)]
        def prepass(hash: Hash, depth: Int, lo: Int, hi: Int): Unit =
          if lo < hi && hash != Hash.zero then
            if depth == StateTrie.ParDepth then tasks += ((hash, lo, hi))
            else
              val node = getNode(hash).get
              if node.nodeType == TrieNodeType.Branch then
                val mid = StateTrie.firstWithBit(batch, lo, hi, depth)
                prepass(Hash.wrapUnchecked(node.left.toArrayUnsafe), depth + 1, lo, mid)
                prepass(Hash.wrapUnchecked(node.right.toArrayUnsafe), depth + 1, mid, hi)
        prepass(currentRoot, 0, 0, batch.length)
        // None when the trie is shallower than ParDepth along the batch: the rebuild below may then split instead.
        if tasks.nonEmpty then
          doneByBatchIndex = runSubtrees(tasks.length, i => tasks(i)._2)((child, i) =>
            child.updateSubtree(tasks(i)._1, StateTrie.ParDepth, batch, tasks(i)._2, tasks(i)._3)
          )
      try currentRoot = updateSubtree(currentRoot, 0, batch, 0, batch.length)
      finally doneByBatchIndex = null

  /**
   * Whether an update of `size` keys is rebuilt as parallel subtrees. Only on the in-memory backend, whose reads are
   * safe from several threads while nothing writes and whose save takes pending tables as they are; only for a batch
   * large enough to repay starting the tasks; and only as the first update since a save, so that every pending node
   * a subtree could meet is in the backend or its own table.
   */
  private def splits(size: Int): Boolean =
    size >= StateTrie.ParFrom && children == null && pendingNodes.size == 0 &&
      backend.isInstanceOf[InMemoryTrieBackend]

  private def runSubtrees(n: Int, key: Int => Int)(job: (StateTrie, Int) => Hash): java.util.HashMap[Integer, Hash] =
    val kids = Array.fill(n)(new StateTrie(backend, Hash.zero))
    val hashes = new Array[Hash](n)
    java.util.stream.IntStream.range(0, n).parallel().forEach(i => hashes(i) = job(kids(i), i))
    val done = new java.util.HashMap[Integer, Hash]
    var i = 0
    while i < n do
      done.put(key(i), hashes(i))
      i += 1
    children = kids
    done

  private def doneAtBatchIndex(depth: Int, lo: Int): Hash | Null =
    val d = doneByBatchIndex
    if d != null && depth == StateTrie.ParDepth then d.get(lo) else null

  private def doneAtItemIndex(depth: Int, lo: Int): Hash | Null =
    val d = doneByItemIndex
    if d != null && depth == StateTrie.ParDepth then d.get(lo) else null

  private def prefetch(batch: Array[StateTrie.Update]): Unit =
    var hashes = Array(currentRoot)
    var ranges = Array(0, batch.length)
    var depth = 0
    while hashes.nonEmpty do
      backend.prefetchNodes(scala.collection.immutable.ArraySeq.unsafeWrapArray(hashes))
      val nextHashes = new scala.collection.mutable.ArrayBuilder.ofRef[Hash]
      val nextRanges = new scala.collection.mutable.ArrayBuilder.ofInt
      var i = 0
      while i < hashes.length do
        val (lo, hi) = (ranges(2 * i), ranges(2 * i + 1))
        getNode(hashes(i)) match
          case Some(node) if node.nodeType == TrieNodeType.Branch =>
            val mid = StateTrie.firstWithBit(batch, lo, hi, depth)
            def descend(child: Hash, from: Int, to: Int): Unit =
              if from < to && child != Hash.zero then
                nextHashes += child
                nextRanges += from
                nextRanges += to
            descend(Hash.wrapUnchecked(node.left.toArrayUnsafe), lo, mid)
            descend(Hash.wrapUnchecked(node.right.toArrayUnsafe), mid, hi)
          case _ => ()
        i += 1
      hashes = nextHashes.result()
      ranges = nextRanges.result()
      depth += 1

  /** Writes the queued nodes and values with all ref deltas in one backend batch. */
  def save(): Unit =
    backend match
      case mem: InMemoryTrieBackend =>
        val kids = children
        if kids == null then mem.absorb(pendingNodes, pendingValues)
        else
          mem.absorbAll(kids.map(_.pendingNodes) :+ pendingNodes, kids.map(_.pendingValues) :+ pendingValues)
          children = null
      case _ =>
        val ops = new Array[BackendOp](2 * (pendingNodes.size + pendingValues.size))
        var n = 0
        var s = 0
        while s < pendingNodes.slotCount do
          if pendingNodes.occupied(s) then
            val h = pendingNodes.keyHash(s)
            if pendingNodes.hasPayload(s) then
              ops(n) = BackendOp.WriteNode(pendingNodes.node(s, h).nn)
              n += 1
            val d = pendingNodes.ref(s)
            ops(n) =
              if d < 0 then BackendOp.NodeRefDelta(h, d, pendingNodes.givenUpValue(s, BackendOp.NotRegularLeaf))
              else BackendOp.NodeRefDelta(h, d)
            n += 1
          s += 1
        s = 0
        while s < pendingValues.slotCount do
          if pendingValues.occupied(s) then
            val h = pendingValues.keyHash(s)
            val v = pendingValues.value(s)
            if v != null then
              ops(n) = BackendOp.WriteRawValue(h, JamBytes.wrap(v))
              n += 1
            ops(n) = BackendOp.RawValueRefDelta(h, pendingValues.ref(s))
            n += 1
          s += 1
        backend.batchUpdate(scala.collection.immutable.ArraySeq.unsafeWrapArray(ops).take(n))
    pendingNodes.clear()
    pendingValues.clear()

  /** Rewrites the subtree at `hash` (at `depth`) with `batch(lo until hi)`, whose keys all lead to it. */
  private def updateSubtree(hash: Hash, depth: Int, batch: Array[StateTrie.Update], lo: Int, hi: Int): Hash =
    val pre = doneAtBatchIndex(depth, lo)
    if lo == hi then hash
    else if pre != null then pre
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
            unrefNode(hash, node)
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
            val kept = batch(a).value.isDefined && batch(a).leaf.hash == hash
            if kept then buildFromBatch(depth, batch, lo, hi, existing = leaf, existingIndex = a, existingHash = hash)
            else
              unrefNode(hash, leaf)
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
    if depth == 0 && splits(arr.length) then
      val tasks = new mutable.ArrayBuffer[(Int, Int)]
      def prepass(d: Int, from: Int, to: Int): Unit =
        if to - from >= 2 then
          if d == StateTrie.ParDepth then tasks += ((from, to))
          else
            val mid = StateTrie.firstWithBit(arr, from, to, d)
            prepass(d + 1, from, mid)
            prepass(d + 1, mid, to)
      prepass(0, 0, arr.length)
      if tasks.nonEmpty then
        doneByItemIndex = runSubtrees(tasks.length, i => tasks(i)._1)((child, i) =>
          child.buildItems(StateTrie.ParDepth, arr, tasks(i)._1, tasks(i)._2)
        )
      try buildItems(depth, arr, 0, arr.length)
      finally doneByItemIndex = null
    else buildItems(depth, arr, 0, arr.length)

  private def buildItems(depth: Int, items: Array[StateTrie.Update], lo: Int, hi: Int): Hash =
    val pre = if hi - lo >= 2 then doneAtItemIndex(depth, lo) else null
    if lo == hi then Hash.zero
    else if pre != null then pre
    else if hi - lo == 1 then
      val item = items(lo)
      if item.existing != null then item.existing
      else emitLeaf(item.leaf, item.value.get)
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
    val l = leftHash.toArrayUnsafe
    val r = rightHash.toArrayUnsafe
    val h = TrieNode.branchHash(l, r)
    val slot = refSlot(h)
    if slot >= 0 then pendingNodes.setBranch(slot, l, r)
    Hash.wrapUnchecked(h)

  /** `leaf` must be `TrieNode.leaf(key, value)`; `value` is kept for a regular leaf, whose node holds only its hash. */
  private def emitLeaf(leaf: TrieNode, value: JamBytes): Hash =
    val h = leaf.hash
    val slot = refSlot(h.toArrayUnsafe)
    if slot >= 0 then
      pendingNodes.setNode(slot, leaf)
      if leaf.nodeType == TrieNodeType.RegularLeaf then refValue(leaf.right.toArrayUnsafe, value)
    h

  /**
   * Counts a reference to node `h` and returns its pending slot, for the caller to queue the node in. Returns -1,
   * queueing nothing, when the node was replaced earlier in this batch and is now back: the backend still holds it at
   * the refcount it had, with its raw value.
   */
  private def refSlot(h: Array[Byte]): Int =
    val slot = pendingNodes.findOrInsert(h)
    val d = pendingNodes.ref(slot) + 1
    if d == 0 then
      pendingNodes.remove(slot)
      -1
    else
      pendingNodes.setRef(slot, d)
      slot

  /**
   * Drops a reference to `h`. A node created since the last save that loses its only reference is discarded with its
   * raw value's reference, so the backend never sees either.
   */
  private def unrefNode(h: Hash, node: TrieNode): Unit =
    val slot = pendingNodes.findOrInsert(h.toArrayUnsafe)
    val d = pendingNodes.ref(slot) - 1
    if d == 0 then
      if pendingNodes.isRegularLeaf(slot) then
        val vh = new Array[Byte](32)
        pendingNodes.copyRight(slot, vh)
        pendingNodes.remove(slot)
        unrefValue(vh)
      else pendingNodes.remove(slot)
    else
      pendingNodes.setRef(slot, d)
      if d < 0 then pendingNodes.setGivenUp(slot, node)

  private def refValue(h: Array[Byte], value: JamBytes): Unit =
    val slot = pendingValues.findOrInsert(h)
    pendingValues.addRef(slot, 1)
    pendingValues.setValue(slot, value.toArrayUnsafe)

  private def unrefValue(h: Array[Byte]): Unit =
    val slot = pendingValues.findOrInsert(h)
    if pendingValues.ref(slot) == 1 then pendingValues.remove(slot)
    else pendingValues.addRef(slot, -1)
    ()

  private def pendingOrStoredValue(h: Hash): JamBytes =
    val slot = pendingValues.find(h.toArrayUnsafe)
    var v = if slot >= 0 then pendingValues.value(slot) else null
    val kids = children
    if v == null && kids != null then
      var k = 0
      while v == null && k < kids.length do
        val ks = kids(k).pendingValues.find(h.toArrayUnsafe)
        if ks >= 0 then v = kids(k).pendingValues.value(ks)
        k += 1
    if v != null then JamBytes.wrap(v)
    else backend.readRawValue(h).getOrElse(throw new MissingTrieNodeException(h, "value"))

  /** Nodes queued for the next [[save]], here and in the subtrees' tables; for tests. */
  private[trie] def pendingNodeCount: Int =
    val kids = children
    pendingNodes.payloadCount + (if kids == null then 0 else kids.iterator.map(_.pendingNodes.payloadCount).sum)

  /** Raw values queued for the next [[save]]; for tests. A value queued by two subtrees counts twice. */
  private[trie] def pendingValueCount: Int =
    val kids = children
    pendingValues.payloadCount + (if kids == null then 0 else kids.iterator.map(_.pendingValues.payloadCount).sum)

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

  /** Fails ([[MissingTrieNodeException]]) on a node or value the trie refers to but the backend no longer holds, so a
    * walk never returns a silently truncated state.
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
        buf += ((TrieNode.leafKey(node), pendingOrStoredValue(Hash.wrapUnchecked(node.right.toArrayUnsafe))))

  private def collectChild(hash: Hash, buf: mutable.ArrayBuffer[(JamBytes, JamBytes)]): Unit =
    getNode(hash).foreach(collectLeaves(_, buf))

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
          Some(pendingOrStoredValue(Hash.wrapUnchecked(node.right.toArrayUnsafe)))
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
    else
      val slot = pendingNodes.find(hash.toArrayUnsafe)
      var pending = if slot >= 0 then pendingNodes.node(slot, hash) else null
      val kids = children
      if pending == null && kids != null then
        var k = 0
        while pending == null && k < kids.length do
          val ks = kids(k).pendingNodes.find(hash.toArrayUnsafe)
          if ks >= 0 then pending = kids(k).pendingNodes.node(ks, hash)
          k += 1
      if pending != null then Some(pending)
      else
        val stored = backend.readNode(hash)
        if stored.isEmpty then throw new MissingTrieNodeException(hash, "node")
        stored

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
  private val PrefetchFrom = 16

  /** Batches of at least this many keys are rebuilt as parallel subtrees (in-memory backend only). */
  private[trie] val ParFrom = 4096

  /** The depth the subtrees hang at: up to 32 of them, enough to keep every core busy when their sizes differ. */
  private val ParDepth = 5


  /** One key of a batch: an update (`existing` null; `value` None deletes) or a kept leaf being rebuilt around. */
  private[trie] final class Update(
      val key: Array[Byte],
      val keyBytes: JamBytes,
      val value: Option[JamBytes],
      val existing: Hash,
  ):
    /** The key's first 8 bytes, big-endian: comparing these unsigned orders most pairs without touching `key`. */
    val prefix: Long =
      var p = 0L
      var i = 0
      while i < 8 do
        p = (p << 8) | (key(i) & 0xffL)
        i += 1
      p

    private var built: TrieNode | Null = null

    /** The leaf this update writes, built (and later hashed) once however often it is asked for. */
    def leaf: TrieNode =
      if built == null then built = TrieNode.leaf(keyBytes, value.get)
      built.nn

  private val updateOrder: java.util.Comparator[Update] =
    (a, b) =>
      val c = java.lang.Long.compareUnsigned(a.prefix, b.prefix)
      if c != 0 then c else java.util.Arrays.compareUnsigned(a.key, b.key)

  /** The updates sorted by key, keeping only the last update to each key. */
  private def sortedBatch(updates: Seq[(JamBytes, Option[JamBytes])]): Array[Update] =
    val arr = new Array[Update](updates.size)
    var n = 0
    updates.foreach { (k, v) =>
      require(k.length == 31, s"key must be 31 bytes, got ${k.length}")
      arr(n) = Update(k.toArrayUnsafe, k, v, null)
      n += 1
    }
    java.util.Arrays.parallelSort(arr, updateOrder) // stable, so equal keys stay in update order
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
