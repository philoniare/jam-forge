package io.forge.jam.db

import java.nio.file.Path

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{BackendOp, StateTrieBackend, TrieNode, TrieNodeType}
import org.rocksdb.{
  BloomFilter,
  ColumnFamilyDescriptor,
  ColumnFamilyHandle,
  ColumnFamilyOptions,
  CompressionType,
  DBOptions,
  RocksDB,
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
    val tracksGarbage: Boolean
) extends StateTrieBackend
    with AutoCloseable:

  // Hashes whose count a delta may have taken to zero or below; gc() checks each. Always empty unless tracksGarbage.
  private val deadNodeCandidates = mutable.Set[Hash]()
  private val deadValueCandidates = mutable.Set[Hash]()
  private val writeOptions = new WriteOptions()
  private val nodeCache = new RocksDbTrieBackend.NodeCache(RocksDbTrieBackend.NodeCacheSlots)

  // RocksDB copies keys and values into its own buffers, so the backing arrays can be passed as they are.
  private inline def key(h: Hash): Array[Byte] = h.toArrayUnsafe

  // ---------------------------------------------------------------------------------------------------------
  // Pruning by journal (the node's way of reclaiming replaced states; independent of garbage tracking and gc).
  //
  // A save made inside [[withJournal]] belongs to one block at one height. Two things are recorded with it, in the
  // same atomic write:
  //   - journal(block): the nodes the save replaced, i.e. those in the parent's trie and not in the block's;
  //   - born(node): the greatest height of any save that added the node (a "max" merge, so no read is needed).
  // When the block's parent state is no longer wanted, [[pruneJournal]] deletes each journalled node that no save
  // above the block's height has added again. Nodes are content-addressed, so a later block (on any branch) can
  // bring a replaced node back; `born` is what tells. A save made outside a journal marks its nodes as never
  // deletable. A deleted regular leaf releases one reference to its raw value, which goes when none is left: the
  // value count is one per leaf node ever written and is only ever too high, never too low.
  // ---------------------------------------------------------------------------------------------------------

  @volatile private var journalLabel: Hash = null
  @volatile private var journalHeight: Long = 0L

  /** Run `body` (one block's import) with its trie save attributed to block `label` at `height`. */
  def withJournal[A](label: Hash, height: Long)(body: => A): A =
    journalLabel = label
    journalHeight = height
    try body
    finally journalLabel = null

  private def encodeBE8(v: Long): Array[Byte] = java.nio.ByteBuffer.allocate(8).putLong(v).array()

  /** The greatest height a save added node `h` at; 0 when unknown (written before journals existed). */
  private[db] def nodeBornAt(h: Hash): Long =
    val b = db.get(nodeBornCf, key(h))
    if b == null || b.length != 8 then 0L else java.nio.ByteBuffer.wrap(b).getLong

  /** Nodes journalled for block `label`, if any. */
  private[db] def journalOf(label: Hash): List[Hash] =
    val b = db.get(journalCf, key(label))
    if b == null then Nil
    else (0 until b.length / 32).map(i => Hash.wrap(java.util.Arrays.copyOfRange(b, i * 32, i * 32 + 32))).toList

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

  /** Forget block `label`'s journal without acting on it (a block that was never on the finalized chain). */
  def dropJournal(label: Hash): Unit = db.delete(journalCf, key(label))

  /** The state before block `label` (at `height`) is no longer wanted: delete the nodes its save replaced that
    * nothing above `height` added again, with the raw values that leaves no leaf for. Returns (nodes, values)
    * deleted. Must run on the thread that saves, and only for a block every retained state descends from.
    */
  def pruneJournal(label: Hash, height: Long): (Int, Int) =
    val journalled = journalOf(label)
    if journalled.isEmpty then
      dropJournal(label)
      (0, 0)
    else
      val batch = new WriteBatch()
      try
        val deletedNodes = mutable.ArrayBuffer[Hash]()
        val valueDeltas = mutable.LinkedHashMap[Hash, Long]()
        journalled.foreach { h =>
          if nodeBornAt(h) <= height then
            val bytes = db.get(nodesCf, key(h))
            if bytes != null then
              val node = TrieNode.decodeArray(bytes)
              if node.nodeType == TrieNodeType.RegularLeaf then
                val v = Hash.wrap(node.right.toArrayUnsafe)
                valueDeltas.update(v, valueDeltas.getOrElse(v, 0L) - 1)
              batch.delete(nodesCf, key(h))
              batch.delete(nodeRefsCf, key(h))
              batch.delete(nodeBornCf, key(h))
              deletedNodes += h
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
        db.write(writeOptions, batch)
        deletedNodes.foreach(nodeCache.remove)
        (deletedNodes.size, deletedValues)
      finally batch.close()

  private def readCount(cf: ColumnFamilyHandle, h: Hash): Long =
    val bytes = db.get(cf, key(h))
    if bytes == null then 0L else decodeLE8(bytes)

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

  def readRawValue(hash: Hash): Option[JamBytes] =
    Option(db.get(valuesCf, key(hash))).map(JamBytes.wrap)

  def batchUpdate(ops: Seq[BackendOp]): Unit =
    val batch = new WriteBatch()
    val written = mutable.ArrayBuffer[TrieNode]()
    val label = journalLabel
    // Outside a journal the height is unknown: such nodes are marked as added "at the end of time" and never pruned.
    val born = encodeBE8(if label == null then Long.MaxValue else journalHeight)
    val replaced = new java.io.ByteArrayOutputStream()
    try
      ops.foreach {
        case BackendOp.WriteNode(node) =>
          batch.put(nodesCf, key(node.hash), node.encodeArray)
          written += node
        case BackendOp.WriteRawValue(h, v) =>
          batch.put(valuesCf, key(h), v.toArrayUnsafe)
        case BackendOp.NodeRefDelta(h, d) =>
          batch.merge(nodeRefsCf, key(h), encodeLE8(d))
          if d < 0 && tracksGarbage then deadNodeCandidates.add(h)
          if d > 0 then batch.merge(nodeBornCf, key(h), born)
          else if d < 0 && label != null then replaced.write(key(h))
        case BackendOp.RawValueRefDelta(h, d) =>
          batch.merge(valueRefsCf, key(h), encodeLE8(d))
          if d < 0 && tracksGarbage then deadValueCandidates.add(h)
      }
      if label != null && replaced.size > 0 then
        // A block saves once; if it ever saved twice, the second save's replaced nodes are added to the first's.
        val earlier = db.get(journalCf, key(label))
        batch.put(journalCf, key(label), if earlier == null then replaced.toByteArray else earlier ++ replaced.toByteArray)
      db.write(writeOptions, batch)
      written.foreach(n => nodeCache.put(n.hash, n))
    finally batch.close()

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

  override def close(): Unit =
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
  private val CfNames =
    Seq("trie-nodes", "trie-values", "trie-node-refs", "trie-value-refs", "trie-journal", "trie-node-born")

  /**
   * Slots in the decoded-node cache: enough for most upper levels of a 1M-key trie, so a read or a block's update
   * goes to RocksDB for the last few nodes of a path only. Up to roughly 250 MB of heap when full (estimated).
   */
  private val NodeCacheSlots = 1 << 20


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
      memory: RocksDbMemory = RocksDbMemory.shared
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
      def tableConfig() = memory.tableConfig().setFilterPolicy(own(new BloomFilter(10, false)))
      val defaultOptions = own(new ColumnFamilyOptions()).setTableFormatConfig(memory.tableConfig())
      val nodeOptions = own(new ColumnFamilyOptions())
        .setTableFormatConfig(tableConfig())
        .setCompressionType(CompressionType.NO_COMPRESSION)
      val valueOptions = own(new ColumnFamilyOptions()).setTableFormatConfig(tableConfig())
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
        )
      )
      val db = RocksDB.open(options, path.toString, descriptors.asJava, handles)
      val hs = handles.asScala.toSeq
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
        tracksGarbage = trackGarbage
      )
    catch
      case e: Throwable =>
        resources.reverse.foreach(r => scala.util.Try(r.close()))
        throw e
