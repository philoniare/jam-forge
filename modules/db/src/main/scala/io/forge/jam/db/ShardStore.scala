package io.forge.jam.db

import java.nio.file.Path

import io.forge.jam.core.primitives.Hash
import scala.collection.mutable
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, ColumnFamilyOptions, DBOptions, RocksDB}

import scala.jdk.CollectionConverters.*

/** Persistent DA shard custody: encoded validator shard sets keyed by
  * (erasure-root, validator index). Stores raw bytes — the shard codec lives
  * with the callers (protocol's DaShards).
  */
final class ShardStore private (
    db: RocksDB,
    shardsCf: ColumnFamilyHandle,
    storedAtCf: ColumnFamilyHandle,
    clock: () => Long,
    ownedHandles: Seq[ColumnFamilyHandle],
    cfOptions: ColumnFamilyOptions,
    dbOptions: DBOptions
) extends AutoCloseable:
  private def key(erasureRoot: Hash, validatorIndex: Int): Array[Byte] =
    val out = new Array[Byte](34)
    System.arraycopy(erasureRoot.toArrayUnsafe, 0, out, 0, 32)
    out(32) = (validatorIndex & 0xff).toByte
    out(33) = ((validatorIndex >> 8) & 0xff).toByte
    out

  /** Store a custody unit and note when its erasure-root was last stored: [[deleteStoredBefore]] expires roots by
    * that time. The index holds, per root, `seconds ++ root` (sorted by time) and `root -> seconds` (to move the
    * first when the root is stored again).
    */
  def put(erasureRoot: Hash, validatorIndex: Int, encodedShards: Array[Byte]): Unit =
    val now = clock()
    val batch = new org.rocksdb.WriteBatch()
    val options = new org.rocksdb.WriteOptions()
    try
      batch.put(shardsCf, key(erasureRoot, validatorIndex), encodedShards)
      val rootKey = erasureRoot.toArrayUnsafe
      val previous = Option(db.get(storedAtCf, rootKey)).filter(_.length == 8).map(java.nio.ByteBuffer.wrap(_).getLong)
      if !previous.contains(now) then
        previous.foreach(t => batch.delete(storedAtCf, storedAtKey(t, erasureRoot)))
        batch.put(storedAtCf, storedAtKey(now, erasureRoot), Array.emptyByteArray)
        batch.put(storedAtCf, rootKey, java.nio.ByteBuffer.allocate(8).putLong(now).array())
      db.write(options, batch)
    finally
      batch.close()
      options.close()

  // Seconds (big-endian, so keys sort by time) then the erasure-root.
  private def storedAtKey(seconds: Long, erasureRoot: Hash): Array[Byte] =
    val out = new Array[Byte](40)
    java.nio.ByteBuffer.wrap(out).putLong(seconds)
    System.arraycopy(erasureRoot.toArrayUnsafe, 0, out, 8, 32)
    out

  /** Delete every custody unit of each erasure-root last stored before `seconds` (as the store's clock counts
    * them); returns how many units went. Units stored before this index existed have no entry and are not touched.
    */
  def deleteStoredBefore(seconds: Long): Int =
    val expired = mutable.ListBuffer.empty[Hash]
    val it = db.newIterator(storedAtCf)
    try
      // Time entries are 40 bytes; the 32-byte per-root entries are skipped. Seconds are non-negative, so the
      // time entries sort by time from the first key.
      it.seekToFirst()
      var more = true
      while more && it.isValid && expired.size < ShardStore.MaxExpiredPerCall do
        val k = it.key()
        if k.length == 40 then
          if java.nio.ByteBuffer.wrap(k).getLong < seconds then expired += Hash(java.util.Arrays.copyOfRange(k, 8, 40))
          else more = false
        it.next()
    finally it.close()
    var deleted = 0
    expired.foreach { root =>
      heldIndexes(root).foreach { idx =>
        db.delete(shardsCf, key(root, idx))
        deleted += 1
      }
      Option(db.get(storedAtCf, root.toArrayUnsafe)).filter(_.length == 8).foreach { t =>
        db.delete(storedAtCf, storedAtKey(java.nio.ByteBuffer.wrap(t).getLong, root))
      }
      db.delete(storedAtCf, root.toArrayUnsafe)
    }
    deleted

  def get(erasureRoot: Hash, validatorIndex: Int): Option[Array[Byte]] =
    Option(db.get(shardsCf, key(erasureRoot, validatorIndex)))

  def has(erasureRoot: Hash, validatorIndex: Int): Boolean =
    db.get(shardsCf, key(erasureRoot, validatorIndex)) != null

  /** All held validator indexes for a report's erasure-root. */
  def heldIndexes(erasureRoot: Hash): List[Int] =
    val prefix = erasureRoot.toArrayUnsafe
    val it = db.newIterator(shardsCf)
    try
      val out = List.newBuilder[Int]
      it.seek(prefix)
      while it.isValid && it.key().length == 34 &&
        java.util.Arrays.equals(it.key(), 0, 32, prefix, 0, 32)
      do
        val k = it.key()
        out += ((k(32) & 0xff) | ((k(33) & 0xff) << 8))
        it.next()
      out.result()
    finally it.close()

  /** Bytes held in write buffers not yet flushed to disk. */
  def memTableBytes: Long = db.getLongProperty(shardsCf, "rocksdb.cur-size-all-mem-tables")

  def delete(erasureRoot: Hash, validatorIndex: Int): Unit =
    db.delete(shardsCf, key(erasureRoot, validatorIndex))

  override def close(): Unit =
    ownedHandles.foreach(_.close())
    db.close()
    cfOptions.close()
    dbOptions.close()

object ShardStore:

  /** Erasure-roots expired by one [[ShardStore.deleteStoredBefore]] call; the rest go at the next. */
  val MaxExpiredPerCall: Int = 4096

  /** @param clock seconds, for the time each unit is stored at; wall-clock time by default */
  def open(
      path: Path,
      memory: RocksDbMemory = RocksDbMemory.shared,
      clock: () => Long = () => System.currentTimeMillis() / 1000
  ): ShardStore =
    RocksDB.loadLibrary()
    // Block cache and write-buffer limit shared with the process's other stores.
    val cfOptions = new ColumnFamilyOptions().setTableFormatConfig(memory.tableConfig())
    val descriptors = Seq("default", "shards", "stored_at").map(n =>
      new ColumnFamilyDescriptor(n.getBytes("UTF-8"), cfOptions)
    )
    val handles = new java.util.ArrayList[ColumnFamilyHandle]()
    val options = memory(
      new DBOptions()
        .setCreateIfMissing(true)
        .setCreateMissingColumnFamilies(true)
    )
    val db =
      try RocksDB.open(options, path.toString, descriptors.asJava, handles)
      catch
        case e: Throwable =>
          options.close()
          cfOptions.close()
          throw e
    val hs = handles.asScala.toSeq
    new ShardStore(
      db,
      shardsCf = hs(1),
      storedAtCf = hs(2),
      clock = clock,
      ownedHandles = hs,
      cfOptions = cfOptions,
      dbOptions = options
    )
