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
      case BackendOp.NodeRefDelta(h, d, _) =>
        if nodes.addRef(nodes.findOrInsert(h.toArrayUnsafe), d) <= 0 then deadNodeCandidates.add(h.toArrayUnsafe)
      case BackendOp.RawValueRefDelta(h, d) =>
        if values.addRef(values.findOrInsert(h.toArrayUnsafe), d) <= 0 then deadValueCandidates.add(h.toArrayUnsafe)
    }

  /**
   * The same as [[batchUpdate]] with a [[StateTrie]]'s pending tables (a slot's count is its delta, and a payload is a
   * write), copied table to table without building an op or node object per entry.
   */
  private[trie] def absorb(pendingNodes: NodeTable, pendingValues: ValueTable): Unit =
    nodes.reserve(pendingNodes.size)
    values.reserve(pendingValues.size)
    absorbNodes(pendingNodes)
    absorbValues(pendingValues, new Array[Byte](32))

  private[trie] def absorbAll(pendingNodes: Array[NodeTable], pendingValues: Array[ValueTable]): Unit =
    import InMemoryTrieBackend.{ParallelFrom, RegionBits, Regions}
    val nodeTotal = pendingNodes.iterator.map(_.size.toLong).sum
    val valueTotal = pendingValues.iterator.map(_.size.toLong).sum
    if nodeTotal + valueTotal < ParallelFrom || nodeTotal + valueTotal > Int.MaxValue then
      var t = 0
      while t < pendingNodes.length do
        absorb(pendingNodes(t), pendingValues(t))
        t += 1
    else
      nodes.reserve(nodeTotal.toInt)
      values.reserve(valueTotal.toInt)
      val (bigNodes, smallNodes) = pendingNodes.partition(_.slotCount >= Regions * 4)
      val (bigValues, smallValues) = pendingValues.partition(_.slotCount >= Regions * 4)

      final class Done(val inserted: Int, val payloads: Int, val dead: HashList, val unresolved: Array[Long])

      def run[T <: HashTables.Table](store: T, tables: Array[T])(each: (T, Int, Int, HashList) => Int): Array[Done] =
        val done = new Array[Done](Regions)
        java.util.stream.IntStream.range(0, Regions).parallel().forEach { region =>
          val end = store.regionStart(region, Regions) + store.slotCount / Regions
          val dead = new HashList
          val unresolved = new scala.collection.mutable.ArrayBuilder.ofLong
          var inserted = 0
          var payloads = 0
          var t = 0
          while t < tables.length do
            val table = tables(t)
            val cap = table.slotCount
            val sliceEnd = table.regionStart(region, Regions) + cap / Regions
            var s = table.regionStart(region, Regions)
            var steps = 0
            while (s < sliceEnd || table.occupied(s & (cap - 1))) && steps < cap do
              val slot = s & (cap - 1)
              if table.occupied(slot) && table.regionOf(table.keyWord(slot, 0), RegionBits) == region then
                val r = each(table, slot, end, dead)
                if r == HashTables.Unresolved then unresolved += ((t.toLong << 32) | slot)
                else
                  inserted += r & 1
                  payloads += r >> 1
              s += 1
              steps += 1
            t += 1
          done(region) = new Done(inserted, payloads, dead, unresolved.result())
        }
        done

      val key = new Array[Byte](32)
      val nodesDone = run(nodes, bigNodes) { (pending, s, end, dead) =>
        val k0 = pending.keyWord(s, 0)
        val k1 = pending.keyWord(s, 1)
        val k2 = pending.keyWord(s, 2)
        val k3 = pending.keyWord(s, 3)
        val found = nodes.findOrInsertBefore(k0, k1, k2, k3, end)
        if found == HashTables.Unresolved then found
        else
          val slot = if found >= 0 then found else -(found + 1)
          val payload = pending.hasPayload(s) && nodes.setNodeFromUncounted(slot, pending, s)
          if nodes.addRef(slot, pending.ref(s)) <= 0 then dead.addWords(k0, k1, k2, k3)
          (if found < 0 then 1 else 0) | (if payload then 2 else 0)
      }
      nodes.addCounts(nodesDone.iterator.map(_.inserted).sum, nodesDone.iterator.map(_.payloads).sum)
      nodesDone.foreach { d =>
        deadNodeCandidates.addAll(d.dead)
        d.unresolved.foreach(u => absorbNode(bigNodes((u >>> 32).toInt), u.toInt))
      }

      val valuesDone = run(values, bigValues) { (pending, s, end, dead) =>
        val k0 = pending.keyWord(s, 0)
        val k1 = pending.keyWord(s, 1)
        val k2 = pending.keyWord(s, 2)
        val k3 = pending.keyWord(s, 3)
        val found = values.findOrInsertBefore(k0, k1, k2, k3, end)
        if found == HashTables.Unresolved then found
        else
          val slot = if found >= 0 then found else -(found + 1)
          val v = pending.value(s)
          val payload = v != null && values.setValueUncounted(slot, v)
          if values.addRef(slot, pending.ref(s)) <= 0 then dead.addWords(k0, k1, k2, k3)
          (if found < 0 then 1 else 0) | (if payload then 2 else 0)
      }
      values.addCounts(valuesDone.iterator.map(_.inserted).sum, valuesDone.iterator.map(_.payloads).sum)
      valuesDone.foreach { d =>
        deadValueCandidates.addAll(d.dead)
        d.unresolved.foreach(u => absorbValue(bigValues((u >>> 32).toInt), u.toInt, key))
      }

      smallNodes.foreach(t => absorbNodes(t))
      smallValues.foreach(t => absorbValues(t, key))

  private def absorbNode(pendingNodes: NodeTable, s: Int): Unit =
    val slot = nodes.findOrInsertWords(
      pendingNodes.keyWord(s, 0),
      pendingNodes.keyWord(s, 1),
      pendingNodes.keyWord(s, 2),
      pendingNodes.keyWord(s, 3)
    )
    if pendingNodes.hasPayload(s) then nodes.setNodeFrom(slot, pendingNodes, s)
    if nodes.addRef(slot, pendingNodes.ref(s)) <= 0 then
      deadNodeCandidates.add(pendingNodes.keyHash(s).toArrayUnsafe)

  private def absorbValue(pendingValues: ValueTable, s: Int, key: Array[Byte]): Unit =
    var j = 0
    while j < 4 do
      HashTables.putWord(key, j * 8, pendingValues.keyWord(s, j))
      j += 1
    val slot = values.findOrInsert(key)
    val v = pendingValues.value(s)
    if v != null then values.setValue(slot, v)
    if values.addRef(slot, pendingValues.ref(s)) <= 0 then deadValueCandidates.add(key)

  private def absorbNodes(pendingNodes: NodeTable): Unit =
    var s = 0
    while s < pendingNodes.slotCount do
      if pendingNodes.occupied(s) then absorbNode(pendingNodes, s)
      s += 1

  private def absorbValues(pendingValues: ValueTable, key: Array[Byte]): Unit =
    var s = 0
    while s < pendingValues.slotCount do
      if pendingValues.occupied(s) then absorbValue(pendingValues, s, key)
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

object InMemoryTrieBackend:
  private[trie] val ParallelFrom = 4096

  private val RegionBits = 6
  private val Regions = 1 << RegionBits
