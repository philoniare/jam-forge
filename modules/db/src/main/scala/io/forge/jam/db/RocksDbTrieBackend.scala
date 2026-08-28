package io.forge.jam.db

import java.nio.file.Path

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{BackendOp, StateTrieBackend, TrieNode, TrieNodeType}
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, ColumnFamilyOptions, DBOptions, RocksDB, WriteBatch, WriteOptions}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Disk-backed [[StateTrieBackend]] over RocksDB.
  *
  * Column families: trie nodes (hash → 65-byte encoding), raw values
  * (blake2b(value) → value), and their reference counts (8-byte LE). Like the
  * in-memory backend, all mutation goes through single-threaded consensus
  * code, so refcount read-modify-write inside batchUpdate needs no locking.
  */
final class RocksDbTrieBackend private (
    db: RocksDB,
    nodesCf: ColumnFamilyHandle,
    valuesCf: ColumnFamilyHandle,
    nodeRefsCf: ColumnFamilyHandle,
    valueRefsCf: ColumnFamilyHandle,
    ownedHandles: Seq[ColumnFamilyHandle],
    ownsDb: Boolean
) extends StateTrieBackend
    with AutoCloseable:

  private val deadNodeCandidates = mutable.Set[Hash]()
  private val deadValueCandidates = mutable.Set[Hash]()
  private val writeOptions = new WriteOptions()

  private inline def key(h: Hash): Array[Byte] = h.bytes.toArray

  private def readCount(cf: ColumnFamilyHandle, h: Hash): Long =
    val bytes = db.get(cf, key(h))
    if bytes == null then 0L else decodeLE8(bytes)

  def readNode(hash: Hash): Option[TrieNode] =
    Option(db.get(nodesCf, key(hash))).map(b => TrieNode.decode(JamBytes(b)))

  def readRawValue(hash: Hash): Option[JamBytes] =
    Option(db.get(valuesCf, key(hash))).map(JamBytes(_))

  def batchUpdate(ops: Seq[BackendOp]): Unit =
    val batch = new WriteBatch()
    // Refcounts touched by this batch, read once and updated in memory so
    // repeated deltas to the same hash compose before the single write.
    val nodeCounts = mutable.Map[Hash, Long]()
    val valueCounts = mutable.Map[Hash, Long]()
    try
      ops.foreach {
        case BackendOp.WriteNode(node) =>
          batch.put(nodesCf, key(node.hash), node.encode.toArray)
        case BackendOp.WriteRawValue(v) =>
          batch.put(valuesCf, key(Hashing.blake2b256(v)), v.toArray)
        case BackendOp.NodeRefDelta(h, d) =>
          val c = nodeCounts.getOrElseUpdate(h, readCount(nodeRefsCf, h)) + d
          nodeCounts.update(h, c)
          if c <= 0 then deadNodeCandidates.add(h)
        case BackendOp.RawValueRefDelta(h, d) =>
          val c = valueCounts.getOrElseUpdate(h, readCount(valueRefsCf, h)) + d
          valueCounts.update(h, c)
          if c <= 0 then deadValueCandidates.add(h)
      }
      nodeCounts.foreach { case (h, c) => batch.put(nodeRefsCf, key(h), encodeLE8(c)) }
      valueCounts.foreach { case (h, c) => batch.put(valueRefsCf, key(h), encodeLE8(c)) }
      db.write(writeOptions, batch)
    finally batch.close()

  def gc(): Unit =
    deadNodeCandidates.foreach { h =>
      if readCount(nodeRefsCf, h) <= 0 then
        readNode(h).foreach { node =>
          if node.nodeType == TrieNodeType.RegularLeaf then
            val rawValueHash = Hash.fromByteVectorUnsafe(node.right.toByteVector)
            val c = readCount(valueRefsCf, rawValueHash) - 1
            db.put(valueRefsCf, key(rawValueHash), encodeLE8(c))
            if c <= 0 then deadValueCandidates.add(rawValueHash)
        }
        db.delete(nodesCf, key(h))
        db.delete(nodeRefsCf, key(h))
    }
    deadNodeCandidates.clear()
    deadValueCandidates.foreach { h =>
      if readCount(valueRefsCf, h) <= 0 then
        db.delete(valuesCf, key(h))
        db.delete(valueRefsCf, key(h))
    }
    deadValueCandidates.clear()

  def clear(): Unit =
    Seq(nodesCf, valuesCf, nodeRefsCf, valueRefsCf).foreach { cf =>
      val it = db.newIterator(cf)
      try
        it.seekToFirst()
        while it.isValid do
          db.delete(cf, it.key())
          it.next()
      finally it.close()
    }
    deadNodeCandidates.clear()
    deadValueCandidates.clear()

  def nodeRefcount(h: Hash): Long = readCount(nodeRefsCf, h)
  def valueRefcount(h: Hash): Long = readCount(valueRefsCf, h)

  override def close(): Unit =
    writeOptions.close()
    ownedHandles.foreach(_.close())
    if ownsDb then db.close()

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

  /** Open (creating if absent) a trie backend at `path`. */
  def open(path: Path): RocksDbTrieBackend =
    RocksDB.loadLibrary()
    val cfOptions = new ColumnFamilyOptions()
    val descriptors =
      (Seq("default") ++ CfNames).map(n =>
        new ColumnFamilyDescriptor(n.getBytes("UTF-8"), cfOptions)
      )
    val handles = new java.util.ArrayList[ColumnFamilyHandle]()
    val options = new DBOptions()
      .setCreateIfMissing(true)
      .setCreateMissingColumnFamilies(true)
    val db = RocksDB.open(options, path.toString, descriptors.asJava, handles)
    val hs = handles.asScala.toSeq
    new RocksDbTrieBackend(
      db,
      nodesCf = hs(1),
      valuesCf = hs(2),
      nodeRefsCf = hs(3),
      valueRefsCf = hs(4),
      ownedHandles = hs,
      ownsDb = true
    )
