package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.HashTables.{HashList, NodeTable, ValueTable}

/**
 * A [[StateTrieBackend]] in flat hash tables ([[HashTables]]): nodes and counts live in primitive arrays rather than
 * as objects in maps. Not thread-safe: callers serialise writes and reads against them.
 */
final class InMemoryTrieBackend extends StateTrieBackend:
  private val nodes = new NodeTable(HashTables.LargeCapacity)
  private val values = new ValueTable(HashTables.LargeCapacity)
  // Hashes whose count a delta took to zero or below; gc() re-checks each. May hold duplicates.
  private val deadNodeCandidates = new HashList
  private val deadValueCandidates = new HashList

  def readNode(hash: Hash): Option[TrieNode] =
    val slot = nodes.find(hash.toArrayUnsafe)
    if slot < 0 then None else Option(nodes.node(slot, hash))

  def readRawValue(hash: Hash): Option[JamBytes] =
    val slot = values.find(hash.toArrayUnsafe)
    if slot < 0 then None else Option(values.value(slot)).map(JamBytes.wrap)

  def batchUpdate(ops: Seq[BackendOp]): Unit =
    ops.foreach {
      case BackendOp.WriteNode(node) =>
        nodes.setNode(nodes.findOrInsert(node.hash.toArrayUnsafe), node)
      case BackendOp.WriteRawValue(h, v) =>
        values.setValue(values.findOrInsert(h.toArrayUnsafe), v.toArrayUnsafe)
      case BackendOp.NodeRefDelta(h, d) =>
        if nodes.addRef(nodes.findOrInsert(h.toArrayUnsafe), d) <= 0 then deadNodeCandidates.add(h.toArrayUnsafe)
      case BackendOp.RawValueRefDelta(h, d) =>
        if values.addRef(values.findOrInsert(h.toArrayUnsafe), d) <= 0 then deadValueCandidates.add(h.toArrayUnsafe)
    }

  /**
   * The same as [[batchUpdate]] with a [[StateTrie]]'s pending tables (a slot's count is its delta, and a payload is a
   * write), copied table to table without building an op or node object per entry.
   */
  private[trie] def absorb(pendingNodes: NodeTable, pendingValues: ValueTable): Unit =
    val key = new Array[Byte](32)
    nodes.reserve(pendingNodes.size)
    values.reserve(pendingValues.size)
    var s = 0
    while s < pendingNodes.slotCount do
      if pendingNodes.occupied(s) then
        val slot = nodes.findOrInsertWords(
          pendingNodes.keyWord(s, 0),
          pendingNodes.keyWord(s, 1),
          pendingNodes.keyWord(s, 2),
          pendingNodes.keyWord(s, 3)
        )
        if pendingNodes.hasPayload(s) then nodes.setNodeFrom(slot, pendingNodes, s)
        if nodes.addRef(slot, pendingNodes.ref(s)) <= 0 then
          deadNodeCandidates.add(pendingNodes.keyHash(s).toArrayUnsafe)
      s += 1
    s = 0
    while s < pendingValues.slotCount do
      if pendingValues.occupied(s) then
        var j = 0
        while j < 4 do
          HashTables.putWord(key, j * 8, pendingValues.keyWord(s, j))
          j += 1
        val slot = values.findOrInsert(key)
        val v = pendingValues.value(s)
        if v != null then values.setValue(slot, v)
        if values.addRef(slot, pendingValues.ref(s)) <= 0 then deadValueCandidates.add(key)
      s += 1

  def gc(): Unit =
    val key = new Array[Byte](32)
    val valueHash = new Array[Byte](32)
    var i = 0
    while i < deadNodeCandidates.size do
      deadNodeCandidates.copyTo(i, key)
      val slot = nodes.find(key)
      if slot >= 0 && nodes.ref(slot) <= 0 then
        if nodes.isRegularLeaf(slot) then
          nodes.copyRight(slot, valueHash)
          val vs = values.find(valueHash)
          if vs >= 0 && values.addRef(vs, -1) <= 0 then deadValueCandidates.add(valueHash)
        nodes.remove(slot)
      i += 1
    deadNodeCandidates.clear()
    i = 0
    while i < deadValueCandidates.size do
      deadValueCandidates.copyTo(i, key)
      val slot = values.find(key)
      if slot >= 0 && values.ref(slot) <= 0 then values.remove(slot)
      i += 1
    deadValueCandidates.clear()

  def clear(): Unit =
    nodes.clear()
    values.clear()
    deadNodeCandidates.clear()
    deadValueCandidates.clear()

  def nodeCount: Int = nodes.payloadCount
  def valueCount: Int = values.payloadCount
  def nodeRefcount(h: Hash): Long =
    val slot = nodes.find(h.toArrayUnsafe)
    if slot < 0 then 0L else nodes.ref(slot)
  def valueRefcount(h: Hash): Long =
    val slot = values.find(h.toArrayUnsafe)
    if slot < 0 then 0L else values.ref(slot)

  /** Number of pending dead-candidate hashes awaiting the next gc(). */
  def pendingDeadCandidates: Int = deadNodeCandidates.size + deadValueCandidates.size
