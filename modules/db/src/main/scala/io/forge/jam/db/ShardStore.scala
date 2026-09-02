package io.forge.jam.db

import java.nio.file.Path

import io.forge.jam.core.primitives.Hash
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, ColumnFamilyOptions, DBOptions, RocksDB}

import scala.jdk.CollectionConverters.*

/** Persistent DA shard custody: encoded validator shard sets keyed by
  * (erasure-root, validator index). Stores raw bytes — the shard codec lives
  * with the callers (protocol's DaShards).
  */
final class ShardStore private (
    db: RocksDB,
    shardsCf: ColumnFamilyHandle,
    ownedHandles: Seq[ColumnFamilyHandle]
) extends AutoCloseable:
  private def key(erasureRoot: Hash, validatorIndex: Int): Array[Byte] =
    val out = new Array[Byte](34)
    System.arraycopy(erasureRoot.bytes.toArray, 0, out, 0, 32)
    out(32) = (validatorIndex & 0xff).toByte
    out(33) = ((validatorIndex >> 8) & 0xff).toByte
    out

  def put(erasureRoot: Hash, validatorIndex: Int, encodedShards: Array[Byte]): Unit =
    db.put(shardsCf, key(erasureRoot, validatorIndex), encodedShards)

  def get(erasureRoot: Hash, validatorIndex: Int): Option[Array[Byte]] =
    Option(db.get(shardsCf, key(erasureRoot, validatorIndex)))

  def has(erasureRoot: Hash, validatorIndex: Int): Boolean =
    db.get(shardsCf, key(erasureRoot, validatorIndex)) != null

  /** All held validator indexes for a report's erasure-root. */
  def heldIndexes(erasureRoot: Hash): List[Int] =
    val prefix = erasureRoot.bytes.toArray
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

  def delete(erasureRoot: Hash, validatorIndex: Int): Unit =
    db.delete(shardsCf, key(erasureRoot, validatorIndex))

  override def close(): Unit =
    ownedHandles.foreach(_.close())
    db.close()

object ShardStore:
  def open(path: Path): ShardStore =
    RocksDB.loadLibrary()
    val cfOptions = new ColumnFamilyOptions()
    val descriptors = Seq("default", "shards").map(n =>
      new ColumnFamilyDescriptor(n.getBytes("UTF-8"), cfOptions)
    )
    val handles = new java.util.ArrayList[ColumnFamilyHandle]()
    val options = new DBOptions()
      .setCreateIfMissing(true)
      .setCreateMissingColumnFamilies(true)
    val db = RocksDB.open(options, path.toString, descriptors.asJava, handles)
    val hs = handles.asScala.toSeq
    new ShardStore(db, shardsCf = hs(1), ownedHandles = hs)
