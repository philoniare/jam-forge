package io.forge.jam.db

import java.nio.file.Path

import io.forge.jam.core.{JamBytes, ProcessSettings}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{BackendOp, MissingTrieNodeException, StateTrieBackend, TrieNode, TrieNodeType}
import org.rocksdb.{
  BloomFilter,
  ColumnFamilyDescriptor,
  DataBlockIndexType,
  IndexType,
  ColumnFamilyHandle,
  ColumnFamilyOptions,
  CompressionType,
  DBOptions,
  ReadOptions,
  RocksDB,
  Snapshot,
  UInt64AddOperator,
  WriteBatch,
  WriteOptions
}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Disk-backed [[StateTrieBackend]] over RocksDB.
  *
  * Column families: trie nodes (hash → 65-byte encoding), raw values
  * (blake2b(value) → value), and their reference counts (8-byte LE). Refcount
  * deltas are written with RocksDB's `uint64add` merge operator, so applying
  * one needs no read; the count is resolved when gc reads it. Like the
  * in-memory backend, all mutation goes through single-threaded consensus code;
  * reads may come from other threads.
  */
final class RocksDbTrieBackend private (
    db: RocksDB,
    nodesCf: ColumnFamilyHandle,
    valuesCf: ColumnFamilyHandle,
    nodeRefsCf: ColumnFamilyHandle,
    valueRefsCf: ColumnFamilyHandle,
    journalCf: ColumnFamilyHandle,
    nodeBornCf: ColumnFamilyHandle,
    ownedHandles: Seq[ColumnFamilyHandle],
    resources: Seq[AutoCloseable],
    watched: AutoCloseable,
    val tracksGarbage: Boolean,
    val settings: RocksDbTrieBackend.Settings,
    memory: RocksDbMemory
) extends StateTrieBackend
    with AutoCloseable:

  // Hashes whose count a delta may have taken to zero or below; gc() checks each. Always empty unless tracksGarbage.
  private val deadNodeCandidates = mutable.Set[Hash]()
  private val deadValueCandidates = mutable.Set[Hash]()
  private val writeOptions = new WriteOptions()


  private val unflushed = new java.util.concurrent.atomic.AtomicLong()

  private def wrote(bytes: Long): Unit =
    if settings.manualWalFlush && unflushed.addAndGet(bytes) >= settings.manualWalFlushBytes then
      unflushed.set(0L)
      db.flushWal(false)

  private def flushNow(): Unit =
    if settings.manualWalFlush then
      unflushed.set(0L)
      db.flushWal(false)
  private val nodeCache = new RocksDbTrieBackend.NodeCache(RocksDbTrieBackend.NodeCacheSlots)

  // RocksDB copies keys and values into its own buffers, so the backing arrays can be passed as they are.
  private inline def key(h: Hash): Array[Byte] = h.toArrayUnsafe

  // ---------------------------------------------------------------------------------------------------------
  // Pruning by journal (the node's way of reclaiming replaced states; independent of garbage tracking and gc).
  //
  // A save made inside [[withJournal]] belongs to one block at one height. Two things are recorded with it, in the
  // same atomic write:
  //   - journal(block): the nodes the save replaced, i.e. those in the parent's trie and not in the block's;
  //   - born(node), for each node the save added that was already stored: the greatest height of any save that
  //     added it again (a "max" merge). A node written for the first time gets no record, and that is nearly all
  //     of them, so a block's save and the pruning of its journal each touch `born` for a handful of nodes.
  // When the block's parent state is no longer wanted, [[pruneJournal]] deletes each journalled node that no save
  // above the block's height has added again. Nodes are content-addressed, so a later block (on any branch) can
  // bring a replaced node back; `born` is what tells. A node with no record was added once, by the save that
  // created it. That save is below any block that replaced the node, unless it was on a branch the pruned block
  // is not on: a branch whose states are, by [[pruneJournal]]'s contract, no longer wanted. A save made outside a
  // journal marks every node it adds as never deletable. A deleted regular leaf releases one reference to its raw value, which goes when none is left: the
  // value count is one per leaf node ever written and is only ever too high, never too low.
  // ---------------------------------------------------------------------------------------------------------

  @volatile private var journalLabel: Hash = null
  @volatile private var journalHeight: Long = 0L
  @volatile private var journalReplaced: Boolean = true

  def withJournal[A](label: Hash, height: Long, replaced: Boolean = true)(body: => A): A =
    journalLabel = label
    journalHeight = height
    journalReplaced = replaced
    try body
    finally
      journalLabel = null
      journalReplaced = true

  private def encodeBE8(v: Long): Array[Byte] = java.nio.ByteBuffer.allocate(8).putLong(v).array()

  /** The greatest height a save added node `h` at; 0 when unknown (written before journals existed). */
  private[db] def nodeBornAt(h: Hash): Long =
    val b = db.get(nodeBornCf, key(h))
    if b == null || b.length != 8 then 0L else java.nio.ByteBuffer.wrap(b).getLong

  /** Nodes journalled for block `label`, if any. */
  private[jam] def journalOf(label: Hash): List[Hash] = journalEntriesOf(label).map(_.node)

  private[db] def journalEntriesOf(label: Hash): List[RocksDbTrieBackend.Journal.Entry] =
    val b = db.get(journalCf, key(label))
    if b == null then Nil else RocksDbTrieBackend.Journal.decode(b).toList

  @volatile private[db] var exactStoredCheckForTesting = false

  private val pruneNodeReads = new java.util.concurrent.atomic.AtomicLong()
  private[db] def pruneNodeReadsForTesting: Long = pruneNodeReads.get

  /** Trie nodes and raw values on disk, by a full scan: for tests and diagnostics. */
  def storedCounts: (Long, Long) =
    def count(cf: ColumnFamilyHandle): Long =
      val it = db.newIterator(cf)
      try
        var n = 0L
        it.seekToFirst()
        while it.isValid do
          n += 1
          it.next()
        n
      finally it.close()
    (count(nodesCf), count(valuesCf))

  private[db] def contentsForTesting: Seq[(String, Seq[(String, String)])] =
    def hex(b: Array[Byte]) = b.map(x => f"$x%02x").mkString
    Seq("nodes" -> nodesCf, "values" -> valuesCf, "node-refs" -> nodeRefsCf, "value-refs" -> valueRefsCf,
      "journal" -> journalCf, "born" -> nodeBornCf).map { (name, cf) =>
      val it = db.newIterator(cf)
      try
        val out = mutable.ArrayBuffer[(String, String)]()
        it.seekToFirst()
        while it.isValid do
          val v =
            if cf == journalCf && it.key().length == 32 then
              RocksDbTrieBackend.Journal.decode(it.value()).map(_.node.toHex).mkString(",")
            else hex(it.value())
          out += hex(it.key()) -> v
          it.next()
        name -> out.toSeq
      finally it.close()
    }

  private[jam] def loseNodesForTesting(hashes: Hash*): Unit =
    hashes.foreach { h =>
      db.delete(nodesCf, key(h))
      nodeCache.remove(h)
    }

  /** Forget block `label`'s journal without acting on it (a block that was never on the finalized chain). */
  def dropJournal(label: Hash): Unit =
    db.delete(journalCf, key(label))
    wrote(64)

  private val saveLock = new Object
  private var saves = 0L // guarded by saveLock

  private def pruneMark(label: Hash): Array[Byte] = RocksDbTrieBackend.PruneMarkPrefix ++ key(label)

  def schedulePrune(label: Hash, height: Long): Unit =
    db.put(journalCf, pruneMark(label), encodeBE8(height))
    flushNow() // the only record of the request: it must outlive a process crash

  def scheduledPrunes: List[(Hash, Long)] =
    val prefix = RocksDbTrieBackend.PruneMarkPrefix
    val it = db.newIterator(journalCf)
    try
      val found = mutable.ListBuffer[(Hash, Long)]()
      it.seek(prefix)
      while it.isValid && it.key().length >= prefix.length && it.key().take(prefix.length).sameElements(prefix) do
        if it.key().length == prefix.length + 32 && it.value().length == 8 then
          found += Hash.wrap(it.key().drop(prefix.length)) -> java.nio.ByteBuffer.wrap(it.value()).getLong
        it.next()
      found.toList.sortBy(_._2)
    finally it.close()

  /** The state before block `label` (at `height`) is no longer wanted: delete the nodes its save replaced that
    * nothing above `height` added again, with the raw values that leaves no leaf for. Returns (nodes, values)
    * deleted. Only for a block every retained state descends from. May run beside saves, on another thread: the
    * reading is done unlocked, and only the write waits for a save in progress (and is worked out again, under the
    * lock, when a save came in between).
    */
  def pruneJournal(label: Hash, height: Long): (Int, Int) =
    val journalled = RocksDbTrieBackend.Journal.distinct(journalEntriesOf(label))
    if journalled.isEmpty then
      val batch = new WriteBatch()
      try
        batch.delete(journalCf, key(label))
        batch.delete(journalCf, pruneMark(label))
        db.write(writeOptions, batch)
        wrote(batch.getDataSize)
      finally batch.close()
      (0, 0)
    else
      val seen = saveLock.synchronized(saves)
      var plan = planPrune(label, height, journalled)
      try
        saveLock.synchronized {
          if saves != seen then
            plan.batch.close()
            plan = planPrune(label, height, journalled)
          db.write(writeOptions, plan.batch)
          wrote(plan.batch.getDataSize)
          plan.nodes.foreach(nodeCache.remove)
        }
        (plan.nodes.size, plan.values)
      finally plan.batch.close()

  private final class PrunePlan(val batch: WriteBatch, val nodes: IndexedSeq[Hash], val values: Int)

  private def planPrune(label: Hash, height: Long, journalled: IndexedSeq[RocksDbTrieBackend.Journal.Entry]): PrunePlan =
    val batch = new WriteBatch()
    try
      val deletedNodes = mutable.ArrayBuffer[Hash]()
      val valueDeltas = mutable.LinkedHashMap[Hash, Long]()
      val born = readMany(nodeBornCf, journalled.map(_.node))
      val prunable = journalled.indices.filter { i =>
        (if born(i) == null || born(i).length != 8 then 0L else java.nio.ByteBuffer.wrap(born(i)).getLong) <= height
      }
      val unknown = prunable.filter(i => !journalled(i).known)
      pruneNodeReads.addAndGet(unknown.size)
      val cached = unknown.map(i => nodeCache.get(journalled(i).node))
      val missed = unknown.indices.filter(cached(_) == null)
      val stored = readMany(nodesCf, missed.map(m => journalled(unknown(m)).node))
      val read = mutable.HashMap[Int, TrieNode]()
      unknown.indices.foreach(u => if cached(u) != null then read(unknown(u)) = cached(u))
      missed.indices.foreach(m => if stored(m) != null then read(unknown(missed(m))) = TrieNode.decodeArray(stored(m)))
      prunable.foreach { i =>
        val entry = journalled(i)
        val h = entry.node
        val leafValue: Option[Hash | Null] =
          if entry.known then Some(entry.leafValue)
          else
            read.get(i).map(node =>
              if node.nodeType == TrieNodeType.RegularLeaf then Hash.wrap(node.right.toArrayUnsafe) else null
            )
        leafValue.foreach { v =>
          if v != null then valueDeltas.update(v, valueDeltas.getOrElse(v, 0L) - 1)
          batch.delete(nodesCf, key(h))
          if tracksGarbage then batch.delete(nodeRefsCf, key(h))
          if born(i) != null then batch.delete(nodeBornCf, key(h))
          deletedNodes += h
        }
      }
      var deletedValues = 0
      val values = valueDeltas.keys.toIndexedSeq
      readCounts(valueRefsCf, values).lazyZip(values).foreach { (stored, v) =>
        if stored + valueDeltas(v) <= 0 then
          batch.delete(valuesCf, key(v))
          batch.delete(valueRefsCf, key(v))
          deletedValues += 1
        else batch.merge(valueRefsCf, key(v), encodeLE8(valueDeltas(v)))
      }
      batch.delete(journalCf, key(label))
      batch.delete(journalCf, pruneMark(label))
      PrunePlan(batch, deletedNodes.toIndexedSeq, deletedValues)
    catch
      case e: Throwable =>
        batch.close()
        throw e

  private def readCount(cf: ColumnFamilyHandle, h: Hash): Long =
    val bytes = db.get(cf, key(h))
    if bytes == null then 0L else decodeLE8(bytes)

  private def readMany(cf: ColumnFamilyHandle, hashes: IndexedSeq[Hash]): IndexedSeq[Array[Byte]] =
    hashes.grouped(RocksDbTrieBackend.MultiGetChunk).flatMap { chunk =>
      db.multiGetAsList(java.util.Collections.nCopies(chunk.size, cf), chunk.map(key).asJava).asScala
    }.toIndexedSeq

  /** Counts of `hashes` in `cf`, read in one multi-get. */
  private def readCounts(cf: ColumnFamilyHandle, hashes: IndexedSeq[Hash]): IndexedSeq[Long] =
    if hashes.isEmpty then IndexedSeq.empty
    else
      val cfs = java.util.Collections.nCopies(hashes.size, cf)
      val got = db.multiGetAsList(cfs, hashes.map(key).asJava)
      hashes.indices.map(i => Option(got.get(i)).fold(0L)(decodeLE8))

  def readNode(hash: Hash): Option[TrieNode] =
    val cached = nodeCache.get(hash)
    if cached != null then Some(cached)
    else
      val bytes = db.get(nodesCf, key(hash))
      if bytes == null then None
      else
        val node = TrieNode.decodeArray(bytes)
        nodeCache.put(hash, node)
        Some(node)

  override def readsInBulk: Boolean = true

  override def prefetchNodes(hashes: IndexedSeq[Hash]): Unit =
    val missing = hashes.filter(h => nodeCache.get(h) == null)
    def load(chunk: IndexedSeq[Hash]): Unit =
      val got = db.multiGetAsList(java.util.Collections.nCopies(chunk.size, nodesCf), chunk.map(key).asJava)
      chunk.indices.foreach { i =>
        val bytes = got.get(i)
        if bytes != null then nodeCache.put(chunk(i), TrieNode.decodeArray(bytes))
      }
    if missing.size < 2 * RocksDbTrieBackend.PrefetchChunk then
      if missing.nonEmpty then load(missing)
    else
      val chunks = missing.grouped(RocksDbTrieBackend.PrefetchChunk).toIndexedSeq
      RocksDbTrieBackend.inParallel(settings.storageThreads, chunks.size)(i => load(chunks(i)))

  def readRawValue(hash: Hash): Option[JamBytes] =
    Option(db.get(valuesCf, key(hash))).map(JamBytes.wrap)

  def batchUpdate(ops: Seq[BackendOp]): Unit =
    val batch = new WriteBatch()
    val written = mutable.ArrayBuffer[TrieNode]()
    val label = journalLabel
    val journalling = label != null && journalReplaced
    // Outside a journal the height is unknown: such nodes are marked as added "at the end of time" and never pruned.
    val born = encodeBE8(if label == null then Long.MaxValue else journalHeight)
    val replaced = mutable.LinkedHashMap[Hash, RocksDbTrieBackend.Journal.Entry]()
    val added = mutable.ArrayBuffer[Hash]()
    val touched = mutable.ArrayBuffer[Hash]()
    try
      ops.foreach {
        case BackendOp.WriteNode(node) =>
          batch.put(nodesCf, key(node.hash), node.encodeArray)
          written += node
          touched += node.hash
        case BackendOp.WriteRawValue(h, v) =>
          batch.put(valuesCf, key(h), v.toArrayUnsafe)
          touched += h
        case BackendOp.NodeRefDelta(h, d, described) =>
          if tracksGarbage then
            batch.merge(nodeRefsCf, key(h), encodeLE8(d))
            if d < 0 then deadNodeCandidates.add(h)
          if d > 0 then
            touched += h
            if label == null then batch.merge(nodeBornCf, key(h), born) else added += h
          else if d < 0 && journalling then replaced(h) = RocksDbTrieBackend.Journal.Entry.of(h, described)
        case BackendOp.RawValueRefDelta(h, d) =>
          batch.merge(valueRefsCf, key(h), encodeLE8(d))
          if d > 0 then touched += h
          if d < 0 && tracksGarbage then deadValueCandidates.add(h)
      }
      saveLock.synchronized {
        val stored = new Array[Boolean](added.size)
        def ask(i: Int): Unit =
          val h = added(i)
          stored(i) =
            nodeCache.get(h) != null ||
              (if exactStoredCheckForTesting then db.get(nodesCf, key(h)) != null
               else db.keyMayExist(nodesCf, key(h), null))
        if added.size < 2 * RocksDbTrieBackend.ExistsChunk then added.indices.foreach(ask)
        else
          val chunk = RocksDbTrieBackend.ExistsChunk
          RocksDbTrieBackend.inParallel(settings.storageThreads, (added.size + chunk - 1) / chunk)(c =>
            (c * chunk until math.min(added.size, (c + 1) * chunk)).foreach(ask)
          )
        added.indices.foreach(i => if stored(i) then batch.merge(nodeBornCf, key(added(i)), born))
        if label != null && replaced.nonEmpty then
          val earlier = db.get(journalCf, key(label))
          val had = if earlier == null then IndexedSeq.empty else RocksDbTrieBackend.Journal.decode(earlier)
          val have = had.iterator.map(_.node).toSet
          val fresh = replaced.valuesIterator.filterNot(e => have.contains(e.node)).toIndexedSeq
          if fresh.nonEmpty then
            batch.put(journalCf, key(label), RocksDbTrieBackend.Journal.encode(had ++ fresh, settings.journalValueHashes))
        db.write(writeOptions, batch)
        wrote(batch.getDataSize)
        written.foreach(n => nodeCache.put(n.hash, n))
        saves += 1
        if sweeping != null then touched.foreach(h => sweeping.add(h.toArrayUnsafe))
      }
    finally batch.close()


  private var sweeping: RocksDbTrieBackend.KeySet = null

  def beginSweep(): StateSweep = saveLock.synchronized {
    if sweeping != null then throw new IllegalStateException("a state sweep is already in progress")
    val touched = new RocksDbTrieBackend.KeySet(1 << 10)
    sweeping = touched
    new StateSweep(db.getSnapshot, touched)
  }

  final class StateSweep private[RocksDbTrieBackend] (snapshot: Snapshot, touched: RocksDbTrieBackend.KeySet)
      extends AutoCloseable:
    import RocksDbTrieBackend.{SweepOutcome, SweepStats}

    private val readOptions = new ReadOptions().setSnapshot(snapshot).setFillCache(false)
    private val stats = new SweepStats
    @volatile private var closed = false

    def run(
        roots: Seq[Hash],
        floorHeight: Long,
        journalWanted: Hash => Boolean,
        stop: () => Boolean,
        afterMark: () => Unit = () => ()
    ): SweepOutcome =
      val began = System.nanoTime()
      def done(outcome: SweepStats => SweepOutcome): SweepOutcome =
        stats.millis = (System.nanoTime() - began) / 1_000_000
        outcome(stats)
      val marked = new RocksDbTrieBackend.KeySet(1 << 16)
      try mark(roots, marked, stop)
      catch case e: MissingTrieNodeException => return done(SweepOutcome.Incomplete(e.hash, e.what, _))
      stats.marked = marked.size
      stats.markBytes = marked.bytes
      if stop() then return done(SweepOutcome.Stopped(_))
      afterMark()
      if !sweepNodes(marked, floorHeight, stop) then return done(SweepOutcome.Stopped(_))
      if !dropJournals(journalWanted, stop) then return done(SweepOutcome.Stopped(_))
      done(SweepOutcome.Completed(_))

    private def mark(roots: Seq[Hash], marked: RocksDbTrieBackend.KeySet, stop: () => Boolean): Unit =
      var level = mutable.ArrayBuffer[Array[Byte]]()
      roots.foreach(r => if r != Hash.zero && marked.add(r.toArrayUnsafe) then level += r.toArrayUnsafe)
      while level.nonEmpty && !stop() do
        val next = mutable.ArrayBuffer[Array[Byte]]()
        level.grouped(RocksDbTrieBackend.MultiGetChunk).foreach { chunk =>
          val got = db.multiGetAsList(readOptions, java.util.Collections.nCopies(chunk.size, nodesCf), chunk.asJava)
          var i = 0
          while i < chunk.size do
            val bytes = got.get(i)
            if bytes == null then throw new MissingTrieNodeException(Hash.wrap(chunk(i)), "node")
            bytes(0) match
              case 0 => // branch
                val l = java.util.Arrays.copyOfRange(bytes, 1, 33)
                val r = java.util.Arrays.copyOfRange(bytes, 33, 65)
                if !RocksDbTrieBackend.isZero(l) && marked.add(l) then next += l
                if !RocksDbTrieBackend.isZero(r) && marked.add(r) then next += r
              case 2 => // regular leaf: its raw value
                if marked.add(java.util.Arrays.copyOfRange(bytes, 33, 65)) then stats.valuesMarked += 1
              case _ => ()
            i += 1
        }
        level = next

    private def sweepNodes(marked: RocksDbTrieBackend.KeySet, floor: Long, stop: () => Boolean): Boolean =
      val it = db.newIterator(nodesCf, readOptions)
      try
        val hashes = mutable.ArrayBuffer[Hash]()
        val nodes = mutable.ArrayBuffer[Array[Byte]]()
        it.seekToFirst()
        var stopped = false
        while it.isValid && !stopped do
          val k = it.key()
          if k.length == 32 && !marked.contains(k) then
            hashes += Hash.wrap(k)
            nodes += it.value()
          it.next()
          if hashes.size >= RocksDbTrieBackend.SweepBatch then
            deleteNodes(hashes, nodes, marked, floor)
            hashes.clear()
            nodes.clear()
            stopped = pause(stop)
        if !stopped && hashes.nonEmpty then deleteNodes(hashes, nodes, marked, floor)
        !stopped
      finally it.close()

    private def deleteNodes(
        hashes: mutable.ArrayBuffer[Hash],
        nodes: mutable.ArrayBuffer[Array[Byte]],
        marked: RocksDbTrieBackend.KeySet,
        floor: Long
    ): Unit =
      val born = readMany(nodeBornCf, hashes.toIndexedSeq)
      val eligible = hashes.indices.filter { i =>
        val b = born(i)
        b == null || b.length != 8 || { val h = java.nio.ByteBuffer.wrap(b).getLong; h <= floor || h == Long.MaxValue }
      }
      saveLock.synchronized {
        if sweeping ne touched then throw new IllegalStateException("the sweep was closed")
        val batch = new WriteBatch()
        try
          val deleted = mutable.ArrayBuffer[Hash]()
          val valueDeltas = mutable.LinkedHashMap[Hash, Long]()
          eligible.foreach { i =>
            val h = hashes(i)
            if !touched.contains(h.toArrayUnsafe) then
              val bytes = nodes(i)
              if bytes.length == TrieNode.StorageBytes && bytes(0) == TrieNodeType.RegularLeaf.tag then
                val v = Hash.wrap(java.util.Arrays.copyOfRange(bytes, 33, 65))
                valueDeltas.update(v, valueDeltas.getOrElse(v, 0L) - 1)
              batch.delete(nodesCf, key(h))
              if tracksGarbage then batch.delete(nodeRefsCf, key(h))
              if born(i) != null then batch.delete(nodeBornCf, key(h))
              deleted += h
          }
          val values = valueDeltas.keys.toIndexedSeq
          readCounts(valueRefsCf, values).lazyZip(values).foreach { (stored, v) =>
            val delta = valueDeltas(v)
            val keep = marked.contains(v.toArrayUnsafe) || touched.contains(v.toArrayUnsafe)
            if stored + delta <= 0 then
              if !keep then
                batch.delete(valuesCf, key(v))
                batch.delete(valueRefsCf, key(v))
                stats.valuesDeleted += 1
            else batch.merge(valueRefsCf, key(v), encodeLE8(delta))
          }
          db.write(writeOptions, batch)
          wrote(batch.getDataSize)
          deleted.foreach(nodeCache.remove)
          stats.nodesDeleted += deleted.size
          stats.nodesLeft += hashes.size - deleted.size
        finally batch.close()
      }

    private def dropJournals(wanted: Hash => Boolean, stop: () => Boolean): Boolean =
      val prefix = RocksDbTrieBackend.PruneMarkPrefix
      val it = db.newIterator(journalCf, readOptions)
      try
        val drop = mutable.ArrayBuffer[Array[Byte]]()
        it.seekToFirst()
        while it.isValid do
          val k = it.key()
          val label =
            if k.length == 32 then Some(Hash.wrap(k))
            else if k.length == prefix.length + 32 && java.util.Arrays.equals(k, 0, prefix.length, prefix, 0, prefix.length) then
              Some(Hash.wrap(java.util.Arrays.copyOfRange(k, prefix.length, k.length)))
            else None
          label.foreach { l =>
            if !wanted(l) then
              drop += k
              if k.length == 32 then stats.journalsDropped += 1
          }
          it.next()
        var stopped = false
        drop.grouped(RocksDbTrieBackend.SweepBatch).foreach { chunk =>
          if !stopped then
            saveLock.synchronized {
              val batch = new WriteBatch()
              try
                chunk.foreach(k => batch.delete(journalCf, k))
                db.write(writeOptions, batch)
                wrote(batch.getDataSize)
              finally batch.close()
            }
            stopped = pause(stop)
        }
        !stopped
      finally it.close()

    private def pause(stop: () => Boolean): Boolean =
      Thread.`yield`()
      stop()

    def close(): Unit =
      if !closed then
        closed = true
        saveLock.synchronized { if sweeping eq touched then sweeping = null }
        readOptions.close()
        db.releaseSnapshot(snapshot)

  /**
   * Deletes every candidate node and value whose count is at or below zero, in one atomic write. Throws when garbage
   * tracking is off: there are no candidates to check, and such a store's counts are not safe to collect by.
   */
  def gc(): Unit =
    if !tracksGarbage then
      throw new UnsupportedOperationException(
        "RocksDbTrieBackend.gc: garbage tracking is off for this store (opened with trackGarbage = false)"
      )
    val batch = new WriteBatch()
    try
      val deletedNodes = mutable.ArrayBuffer[Hash]()
      val valueDeltas = mutable.HashMap[Hash, Long]()
      val nodes = deadNodeCandidates.toIndexedSeq
      readCounts(nodeRefsCf, nodes).lazyZip(nodes).foreach { (count, h) =>
        if count <= 0 then
          readNode(h).foreach { node =>
            if node.nodeType == TrieNodeType.RegularLeaf then
              val rawValueHash = Hash.wrap(node.right.toArrayUnsafe)
              valueDeltas.update(rawValueHash, valueDeltas.getOrElse(rawValueHash, 0L) - 1)
              deadValueCandidates.add(rawValueHash)
          }
          batch.delete(nodesCf, key(h))
          batch.delete(nodeRefsCf, key(h))
          deletedNodes += h
      }
      val values = deadValueCandidates.toIndexedSeq
      readCounts(valueRefsCf, values).lazyZip(values).foreach { (stored, h) =>
        val delta = valueDeltas.getOrElse(h, 0L)
        if stored + delta <= 0 then
          batch.delete(valuesCf, key(h))
          batch.delete(valueRefsCf, key(h))
        else if delta != 0 then batch.merge(valueRefsCf, key(h), encodeLE8(delta))
      }
      db.write(writeOptions, batch)
      wrote(batch.getDataSize)
      deletedNodes.foreach(nodeCache.remove)
      deadNodeCandidates.clear()
      deadValueCandidates.clear()
    finally batch.close()

  def clear(): Unit =
    val begin = new Array[Byte](32)
    val end = Array.fill[Byte](33)(0xff.toByte)
    Seq(nodesCf, valuesCf, nodeRefsCf, valueRefsCf, journalCf, nodeBornCf).foreach { cf =>
      db.deleteRange(cf, begin, end)
    }
    nodeCache.clear()
    deadNodeCandidates.clear()
    deadValueCandidates.clear()

  /** Number of dead-candidate hashes awaiting the next gc(); always 0 with tracking off. */
  def pendingDeadCandidates: Int = deadNodeCandidates.size + deadValueCandidates.size

  def nodeRefcount(h: Hash): Long = readCount(nodeRefsCf, h)
  def valueRefcount(h: Hash): Long = readCount(valueRefsCf, h)

  def nativeMemory: Seq[(String, Long)] =
    def sum(property: String): Long = ownedHandles.map(h => db.getLongProperty(h, property)).sum
    Seq(
      "block_cache" -> memory.blockCache.getUsage,
      "block_cache_pinned" -> memory.blockCache.getPinnedUsage,
      "table_readers" -> sum("rocksdb.estimate-table-readers-mem"),
      "memtables" -> sum("rocksdb.cur-size-all-mem-tables")
    )

  /** Make every write so far durable (flush and sync the write-ahead log). */
  def syncWal(): Unit =
    unflushed.set(0L)
    db.flushWal(true)

  def unflushedWalBytes: Long = unflushed.get

  override def close(): Unit =
    watched.close() // before the database: the budget must not reach for it again
    db.flushWal(true)
    writeOptions.close()
    ownedHandles.foreach(_.close())
    db.close()
    resources.foreach(_.close())

  private def encodeLE8(v: Long): Array[Byte] =
    val out = new Array[Byte](8)
    var i = 0
    while i < 8 do
      out(i) = ((v >> (8 * i)) & 0xff).toByte
      i += 1
    out

  private def decodeLE8(b: Array[Byte]): Long =
    var v = 0L
    var i = 0
    while i < 8 do
      v |= (b(i).toLong & 0xff) << (8 * i)
      i += 1
    v

object RocksDbTrieBackend:
  final case class Settings(
      journalValueHashes: Boolean = true,
      manualWalFlush: Boolean = false,
      manualWalFlushBytes: Long = 16L << 20,
      partitionedIndexFilters: Boolean = false,
      nodeFiltersOutsideCache: Boolean = true,
      hashIndex: Boolean = false,
      blobValues: Boolean = false,
      backgroundJobs: Int = 0,
      subcompactions: Int = 0,
      storageThreads: Int = 0
  )

  object Settings:
    val JournalValueHashesProperty = "jam.db.journalValueHashes"
    val ManualWalFlushProperty = "jam.db.manualWalFlush"
    val ManualWalFlushBytesProperty = "jam.db.manualWalFlushBytes"
    val PartitionedIndexFiltersProperty = "jam.db.partitionedIndexFilters"
    val NodeFiltersOutsideCacheProperty = "jam.db.nodeFiltersOutsideCache"
    val HashIndexProperty = "jam.db.hashIndex"
    val BlobValuesProperty = "jam.db.blobValues"
    val BackgroundJobsProperty = "jam.db.backgroundJobs"
    val SubcompactionsProperty = "jam.db.subcompactions"
    val StorageThreadsProperty = "jam.db.storageThreads"

    val process: ProcessSettings[Settings] = new ProcessSettings(lookup =>
      val defaults = Settings()
      def flag(property: String, default: Boolean): Boolean =
        lookup(property).map(_.trim.toLowerCase).fold(default) {
          case "true" | "on" | "1"   => true
          case "false" | "off" | "0" => false
          case _                     => default
        }
      def count(property: String, default: Int): Int =
        lookup(property).flatMap(_.trim.toIntOption).filter(_ >= 0).getOrElse(default)
      def bytes(property: String, default: Long): Long =
        lookup(property).flatMap(_.trim.toLongOption).filter(_ > 0).getOrElse(default)
      Settings(
        journalValueHashes = flag(JournalValueHashesProperty, defaults.journalValueHashes),
        manualWalFlush = flag(ManualWalFlushProperty, defaults.manualWalFlush),
        manualWalFlushBytes = bytes(ManualWalFlushBytesProperty, defaults.manualWalFlushBytes),
        partitionedIndexFilters = flag(PartitionedIndexFiltersProperty, defaults.partitionedIndexFilters),
        nodeFiltersOutsideCache = flag(NodeFiltersOutsideCacheProperty, defaults.nodeFiltersOutsideCache),
        hashIndex = flag(HashIndexProperty, defaults.hashIndex),
        blobValues = flag(BlobValuesProperty, defaults.blobValues),
        backgroundJobs = count(BackgroundJobsProperty, defaults.backgroundJobs),
        subcompactions = count(SubcompactionsProperty, defaults.subcompactions),
        storageThreads = count(StorageThreadsProperty, defaults.storageThreads)
      )
    )

  private[db] object Journal:
    final case class Entry(node: Hash, known: Boolean, leafValue: Hash | Null)

    object Entry:
      def of(node: Hash, described: Hash | Null): Entry =
        if described == null then Entry(node, known = false, null)
        else if described == BackendOp.NotRegularLeaf then Entry(node, known = true, null)
        else Entry(node, known = true, described)

    val Tag: Byte = 0x4a
    private val NotLeaf: Byte = 0
    private val Leaf: Byte = 1
    private val Unknown: Byte = 2

    def decode(b: Array[Byte]): IndexedSeq[Entry] =
      if b.length % 32 == 0 then
        (0 until b.length / 32).map(i => Entry(Hash.wrap(java.util.Arrays.copyOfRange(b, i * 32, i * 32 + 32)), false, null))
      else
        if b(0) != Tag then throw new IllegalStateException(s"trie journal of ${b.length} bytes has an unknown tag ${b(0)}")
        val out = mutable.ArrayBuffer[Entry]()
        var pos = 1
        while pos + 33 <= b.length do
          val h = Hash.wrap(java.util.Arrays.copyOfRange(b, pos, pos + 32))
          b(pos + 32) match
            case NotLeaf =>
              out += Entry(h, known = true, null)
              pos += 33
            case Leaf =>
              if pos + 65 > b.length then throw new IllegalStateException("trie journal ends inside an entry")
              out += Entry(h, known = true, Hash.wrap(java.util.Arrays.copyOfRange(b, pos + 33, pos + 65)))
              pos += 65
            case Unknown =>
              out += Entry(h, known = false, null)
              pos += 33
            case k => throw new IllegalStateException(s"trie journal entry of unknown kind $k")
        if b.length - pos > 1 then throw new IllegalStateException(s"trie journal has ${b.length - pos} stray byte(s)")
        out.toIndexedSeq

    def encode(entries: IndexedSeq[Entry], described: Boolean): Array[Byte] =
      if !described then
        val out = new Array[Byte](32 * entries.size)
        entries.indices.foreach(i => System.arraycopy(entries(i).node.toArrayUnsafe, 0, out, 32 * i, 32))
        out
      else
        val size = 1 + entries.iterator.map(e => if e.leafValue != null then 65 else 33).sum
        val out = new Array[Byte](if size % 32 == 0 then size + 1 else size)
        out(0) = Tag
        var pos = 1
        entries.foreach { e =>
          System.arraycopy(e.node.toArrayUnsafe, 0, out, pos, 32)
          if e.leafValue != null then
            out(pos + 32) = Leaf
            System.arraycopy(e.leafValue.nn.toArrayUnsafe, 0, out, pos + 33, 32)
            pos += 65
          else
            out(pos + 32) = if e.known then NotLeaf else Unknown
            pos += 33
        }
        out

    def distinct(entries: Seq[Entry]): IndexedSeq[Entry] =
      val byNode = mutable.LinkedHashMap[Hash, Entry]()
      entries.foreach(e =>
        byNode.get(e.node) match
          case Some(had) if had.known || !e.known => ()
          case _                                  => byNode(e.node) = e
      )
      byNode.values.toIndexedSeq
  private val PruneMarkPrefix: Array[Byte] = "toprune:".getBytes("US-ASCII")

  private val CfNames =
    Seq("trie-nodes", "trie-values", "trie-node-refs", "trie-value-refs", "trie-journal", "trie-node-born")

  /**
   * Slots in the decoded-node cache: enough for most upper levels of a 1M-key trie, so a read or a block's update
   * goes to RocksDB for the last few nodes of a path only. Up to roughly 250 MB of heap when full (estimated).
   */
  private val NodeCacheSlots = 1 << 20

  private val pools = new java.util.concurrent.ConcurrentHashMap[Int, java.util.concurrent.ForkJoinPool]()

  private def inParallel(threads: Int, n: Int)(f: Int => Unit): Unit =
    val all: Runnable = () => java.util.stream.IntStream.range(0, n).parallel().forEach(i => f(i))
    if threads <= 0 then all.run()
    else
      val pool = pools.computeIfAbsent(
        threads,
        t =>
          new java.util.concurrent.ForkJoinPool(
            t,
            p => {
              val w = java.util.concurrent.ForkJoinPool.defaultForkJoinWorkerThreadFactory.newThread(p)
              w.setName(s"jam-storage-${w.getPoolIndex}")
              w.setDaemon(true)
              w
            },
            null,
            false
          )
      )
      try pool.submit(all).get()
      catch case e: java.util.concurrent.ExecutionException if e.getCause != null => throw e.getCause

  private val MultiGetChunk = 4096

  private val PrefetchChunk = 64

  private val ExistsChunk = 256

  private val UntrackedMarker = "trie-garbage-untracked".getBytes("UTF-8")

  private val SweepBatch = 10_000

  private def isZero(a: Array[Byte]): Boolean =
    var i = 0
    while i < a.length && a(i) == 0 do i += 1
    i == a.length

  final class SweepStats:
    var marked: Long = 0L
    var valuesMarked: Long = 0L
    var markBytes: Long = 0L
    var nodesDeleted: Long = 0L
    var nodesLeft: Long = 0L
    var valuesDeleted: Long = 0L
    var journalsDropped: Long = 0L
    var millis: Long = 0L

    override def toString: String =
      s"$nodesDeleted node(s) and $valuesDeleted value(s) deleted, $nodesLeft unreachable node(s) left, " +
        s"$journalsDropped journal(s) dropped; $marked node and value hash(es) marked " +
        s"(${markBytes >> 20} MiB) in $millis ms"

  enum SweepOutcome:
    case Completed(stats: SweepStats)
    case Stopped(stats: SweepStats)
    case Incomplete(hash: Hash, what: String, stats: SweepStats)

  private[db] final class KeySet(initialSlots: Int):
    private var slots = Integer.highestOneBit(math.max(16, initialSlots) - 1) << 1
    private var table = new Array[Long](2 * slots)
    private var count = 0
    private var hasZero = false

    def size: Long = count + (if hasZero then 1 else 0)
    def bytes: Long = table.length.toLong * 8

    private inline def word(h: Array[Byte], off: Int): Long =
      var v = 0L
      var i = 0
      while i < 8 do
        v = (v << 8) | (h(off + i) & 0xffL)
        i += 1
      v

    private inline def slotOf(a: Long, b: Long, mask: Int): Int =
      ((a ^ (a >>> 29) ^ b) * 0x9e3779b97f4a7c15L >>> 32).toInt & mask

    def add(h: Array[Byte]): Boolean =
      val a = word(h, 0)
      val b = word(h, 8)
      if a == 0L && b == 0L then
        val added = !hasZero
        hasZero = true
        added
      else
        if 2 * (count + 1) > slots then grow()
        insert(a, b)

    private def insert(a: Long, b: Long): Boolean =
      val mask = slots - 1
      var i = slotOf(a, b, mask)
      var result = true
      var done = false
      while !done do
        val x = table(2 * i)
        val y = table(2 * i + 1)
        if x == 0L && y == 0L then
          table(2 * i) = a
          table(2 * i + 1) = b
          count += 1
          done = true
        else if x == a && y == b then
          result = false
          done = true
        else i = (i + 1) & mask
      result

    def contains(h: Array[Byte]): Boolean =
      val a = word(h, 0)
      val b = word(h, 8)
      if a == 0L && b == 0L then hasZero
      else
        val mask = slots - 1
        var i = slotOf(a, b, mask)
        var found = false
        var done = false
        while !done do
          val x = table(2 * i)
          val y = table(2 * i + 1)
          if x == 0L && y == 0L then done = true
          else if x == a && y == b then
            found = true
            done = true
          else i = (i + 1) & mask
        found

    private def grow(): Unit =
      val old = table
      slots *= 2
      table = new Array[Long](2 * slots)
      count = 0
      var i = 0
      while i < old.length do
        if old(i) != 0L || old(i + 1) != 0L then insert(old(i), old(i + 1))
        i += 2


  /**
   * A direct-mapped cache of decoded nodes. Nodes are content-addressed and immutable, so an entry can only go stale
   * by its node being deleted, which gc and clear handle by evicting it.
   */
  private final class NodeCache(slots: Int):
    private final class Entry(val hash: Hash, val node: TrieNode)
    private val entries = new java.util.concurrent.atomic.AtomicReferenceArray[Entry](slots)
    private val mask = slots - 1

    /** The cached node, or null. */
    def get(hash: Hash): TrieNode =
      val e = entries.get(hash.hashCode & mask)
      if e != null && e.hash == hash then e.node else null

    def put(hash: Hash, node: TrieNode): Unit = entries.set(hash.hashCode & mask, Entry(hash, node))

    def remove(hash: Hash): Unit =
      val i = hash.hashCode & mask
      val e = entries.get(i)
      if e != null && e.hash == hash then entries.compareAndSet(i, e, null)
      ()

    def clear(): Unit =
      var i = 0
      while i < slots do
        entries.set(i, null)
        i += 1

  /**
   * Open (creating if absent) a trie backend at `path`. Pass `trackGarbage = false` when nothing will call `gc()`, as
   * in the node, which keeps replaced roots readable for reorgs; the class comment says what that gives up.
   */
  def open(
      path: Path,
      trackGarbage: Boolean = true,
      memory: RocksDbMemory = RocksDbMemory.shared,
      settings: Settings = Settings.process.current
  ): RocksDbTrieBackend =
    RocksDB.loadLibrary()
    val resources = mutable.ArrayBuffer[AutoCloseable]()
    def own[A <: AutoCloseable](a: A): A =
      resources += a
      a
    try
      // Keys are uniformly random hashes: a bloom filter lets most misses skip the read, and neither keys nor nodes
      // compress, so only raw values (code, preimages) keep the default compression.
      // The block cache and the write-buffer limit are the process's, shared with the block and shard stores.
      def tableConfig(pointLookups: Boolean = false, outsideCache: Boolean = false) =
        val t = memory.tableConfig().setFilterPolicy(own(new BloomFilter(10, false)))
        if settings.partitionedIndexFilters then
          t.setIndexType(IndexType.kTwoLevelIndexSearch)
            .setPartitionFilters(true)
            .setMetadataBlockSize(4096)
            .setPinTopLevelIndexAndFilter(true)
            .setCacheIndexAndFilterBlocksWithHighPriority(true)
        if pointLookups && settings.hashIndex then
          t.setDataBlockIndexType(DataBlockIndexType.kDataBlockBinaryAndHash).setDataBlockHashTableUtilRatio(0.75)
        if outsideCache then t.setCacheIndexAndFilterBlocks(false).setPinL0FilterAndIndexBlocksInCache(false)
        t
      val defaultOptions = own(new ColumnFamilyOptions()).setTableFormatConfig(memory.tableConfig())
      val nodeOptions = own(new ColumnFamilyOptions())
        .setTableFormatConfig(tableConfig(pointLookups = true, outsideCache = settings.nodeFiltersOutsideCache))
        .setCompressionType(CompressionType.NO_COMPRESSION)
      val valueOptions = own(new ColumnFamilyOptions()).setTableFormatConfig(tableConfig(pointLookups = true))
      if settings.blobValues then
        valueOptions
          .setEnableBlobFiles(true)
          .setMinBlobSize(4096)
          .setBlobFileSize(256L << 20)
          .setEnableBlobGarbageCollection(true)
          .setBlobGarbageCollectionAgeCutoff(0.25)
      val refOptions = own(new ColumnFamilyOptions())
        .setTableFormatConfig(tableConfig())
        .setCompressionType(CompressionType.NO_COMPRESSION)
        .setMergeOperator(own(new UInt64AddOperator()))
      // Greatest height a node was added at: merged with "max" over 8-byte big-endian heights, which compare as
      // bytes the way they compare as numbers.
      val bornOptions = own(new ColumnFamilyOptions())
        .setTableFormatConfig(tableConfig())
        .setCompressionType(CompressionType.NO_COMPRESSION)
        .setMergeOperatorName("max")
      val journalOptions = own(new ColumnFamilyOptions())
        .setTableFormatConfig(tableConfig())
        .setCompressionType(CompressionType.NO_COMPRESSION)
      val descriptors = Seq(
        "default" -> defaultOptions,
        CfNames(0) -> nodeOptions,
        CfNames(1) -> valueOptions,
        CfNames(2) -> refOptions,
        CfNames(3) -> refOptions,
        CfNames(4) -> journalOptions,
        CfNames(5) -> bornOptions
      ).map((n, o) => new ColumnFamilyDescriptor(n.getBytes("UTF-8"), o))
      val handles = new java.util.ArrayList[ColumnFamilyHandle]()
      val options = own(
        memory(
          new DBOptions()
            .setCreateIfMissing(true)
            .setCreateMissingColumnFamilies(true)
            .setManualWalFlush(settings.manualWalFlush)
        )
      )
      if settings.backgroundJobs > 0 then options.setMaxBackgroundJobs(settings.backgroundJobs)
      if settings.subcompactions > 0 then options.setMaxSubcompactions(settings.subcompactions)
      val db = RocksDB.open(options, path.toString, descriptors.asJava, handles)
      val hs = handles.asScala.toSeq
      val wasUntracked = db.get(UntrackedMarker) != null
      if trackGarbage && wasUntracked then
        hs.foreach(_.close())
        db.close()
        throw new IllegalStateException(
          s"RocksDbTrieBackend.open: $path was opened with trackGarbage = false before and holds no node refcounts; " +
            "gc on it would delete live nodes"
        )
      if !trackGarbage && !wasUntracked then
        db.deleteRange(hs(3), new Array[Byte](32), Array.fill[Byte](33)(0xff.toByte))
        db.put(UntrackedMarker, Array[Byte](1))
        db.flushWal(true)
      new RocksDbTrieBackend(
        db,
        nodesCf = hs(1),
        valuesCf = hs(2),
        nodeRefsCf = hs(3),
        valueRefsCf = hs(4),
        journalCf = hs(5),
        nodeBornCf = hs(6),
        ownedHandles = hs,
        resources = resources.reverse.toSeq,
        watched = memory.watch(db, hs),
        tracksGarbage = trackGarbage,
        settings = settings,
        memory = memory
      )
    catch
      case e: Throwable =>
        resources.reverse.foreach(r => scala.util.Try(r.close()))
        throw e
