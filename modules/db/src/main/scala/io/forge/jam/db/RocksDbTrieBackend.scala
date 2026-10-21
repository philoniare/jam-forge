package io.forge.jam.db

import java.nio.file.Path

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{BackendOp, StateTrieBackend, TrieNode, TrieNodeType}
import org.rocksdb.{
  BlockBasedTableConfig,
  BloomFilter,
  ColumnFamilyDescriptor,
  ColumnFamilyHandle,
  ColumnFamilyOptions,
  CompressionType,
  DBOptions,
  LRUCache,
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
    ownedHandles: Seq[ColumnFamilyHandle],
    resources: Seq[AutoCloseable]
) extends StateTrieBackend
    with AutoCloseable:

  // Hashes whose count a delta may have taken to zero or below; gc() checks each.
  private val deadNodeCandidates = mutable.Set[Hash]()
  private val deadValueCandidates = mutable.Set[Hash]()
  private val writeOptions = new WriteOptions()
  private val nodeCache = new RocksDbTrieBackend.NodeCache(RocksDbTrieBackend.NodeCacheSlots)

  // RocksDB copies keys and values into its own buffers, so the backing arrays can be passed as they are.
  private inline def key(h: Hash): Array[Byte] = h.toArrayUnsafe

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
    try
      ops.foreach {
        case BackendOp.WriteNode(node) =>
          batch.put(nodesCf, key(node.hash), node.encodeArray)
          written += node
        case BackendOp.WriteRawValue(h, v) =>
          batch.put(valuesCf, key(h), v.toArrayUnsafe)
        case BackendOp.NodeRefDelta(h, d) =>
          batch.merge(nodeRefsCf, key(h), encodeLE8(d))
          if d < 0 then deadNodeCandidates.add(h)
        case BackendOp.RawValueRefDelta(h, d) =>
          batch.merge(valueRefsCf, key(h), encodeLE8(d))
          if d < 0 then deadValueCandidates.add(h)
      }
      db.write(writeOptions, batch)
      written.foreach(n => nodeCache.put(n.hash, n))
    finally batch.close()

  /** Deletes every candidate node and value whose count is at or below zero, in one atomic write. */
  def gc(): Unit =
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
    Seq(nodesCf, valuesCf, nodeRefsCf, valueRefsCf).foreach { cf =>
      db.deleteRange(cf, begin, end)
    }
    nodeCache.clear()
    deadNodeCandidates.clear()
    deadValueCandidates.clear()

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
    Seq("trie-nodes", "trie-values", "trie-node-refs", "trie-value-refs")

  /** Slots in the decoded-node cache: enough for the upper levels of the trie that every block rewrites. */
  private val NodeCacheSlots = 1 << 16

  private val BlockCacheBytes = 64L * 1024 * 1024

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

  /** Open (creating if absent) a trie backend at `path`. */
  def open(path: Path): RocksDbTrieBackend =
    RocksDB.loadLibrary()
    val resources = mutable.ArrayBuffer[AutoCloseable]()
    def own[A <: AutoCloseable](a: A): A =
      resources += a
      a
    try
      // Keys are uniformly random hashes: a bloom filter lets most misses skip the read, and neither keys nor nodes
      // compress, so only raw values (code, preimages) keep the default compression.
      val blockCache = own(new LRUCache(BlockCacheBytes))
      def tableConfig() =
        new BlockBasedTableConfig()
          .setBlockCache(blockCache)
          .setFilterPolicy(own(new BloomFilter(10, false)))
      val defaultOptions = own(new ColumnFamilyOptions())
      val nodeOptions = own(new ColumnFamilyOptions())
        .setTableFormatConfig(tableConfig())
        .setCompressionType(CompressionType.NO_COMPRESSION)
      val valueOptions = own(new ColumnFamilyOptions()).setTableFormatConfig(tableConfig())
      val refOptions = own(new ColumnFamilyOptions())
        .setTableFormatConfig(tableConfig())
        .setCompressionType(CompressionType.NO_COMPRESSION)
        .setMergeOperator(own(new UInt64AddOperator()))
      val descriptors = Seq(
        "default" -> defaultOptions,
        CfNames(0) -> nodeOptions,
        CfNames(1) -> valueOptions,
        CfNames(2) -> refOptions,
        CfNames(3) -> refOptions
      ).map((n, o) => new ColumnFamilyDescriptor(n.getBytes("UTF-8"), o))
      val handles = new java.util.ArrayList[ColumnFamilyHandle]()
      val options = own(
        new DBOptions()
          .setCreateIfMissing(true)
          .setCreateMissingColumnFamilies(true)
      )
      val db = RocksDB.open(options, path.toString, descriptors.asJava, handles)
      val hs = handles.asScala.toSeq
      new RocksDbTrieBackend(
        db,
        nodesCf = hs(1),
        valuesCf = hs(2),
        nodeRefsCf = hs(3),
        valueRefsCf = hs(4),
        ownedHandles = hs,
        resources = resources.reverse.toSeq
      )
    catch
      case e: Throwable =>
        resources.reverse.foreach(r => scala.util.Try(r.close()))
        throw e
