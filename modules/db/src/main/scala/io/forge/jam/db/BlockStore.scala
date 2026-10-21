package io.forge.jam.db

import java.nio.file.Path

import io.forge.jam.core.primitives.Hash
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, ColumnFamilyOptions, DBOptions, RocksDB, WriteBatch, WriteOptions}

import scala.jdk.CollectionConverters.*

/** Persistent chain storage: encoded blocks and headers by header hash, a
  * parent → children index for the fork tree, and named head pointers.
  *
  * Stores raw encoded bytes — the codec lives with the callers, keeping this
  * module protocol-version agnostic.
  */
final class BlockStore private (
    db: RocksDB,
    blocksCf: ColumnFamilyHandle,
    headersCf: ColumnFamilyHandle,
    childrenCf: ColumnFamilyHandle,
    metaCf: ColumnFamilyHandle,
    ownedHandles: Seq[ColumnFamilyHandle],
    cfOptions: ColumnFamilyOptions,
    dbOptions: DBOptions
) extends AutoCloseable:

  // RocksDB copies keys, so the hash's own array can be passed.
  private val writeOptions = new WriteOptions()

  private inline def key(h: Hash): Array[Byte] = h.toArrayUnsafe

  // Serialises the read-modify-write of a parent's children list.
  private val childrenLock = new Object

  /** Store a block (and its header) under its header hash, indexing it as a
    * child of `parent`, in one atomic write.
    */
  def putBlock(
      headerHash: Hash,
      parent: Hash,
      headerBytes: Array[Byte],
      blockBytes: Array[Byte]
  ): Unit =
    childrenLock.synchronized {
      val batch = new WriteBatch()
      try
        batch.put(blocksCf, key(headerHash), blockBytes)
        batch.put(headersCf, key(headerHash), headerBytes)
        val existing = children(parent)
        if !existing.contains(headerHash) then
          batch.put(childrenCf, key(parent), encodeChildren(existing :+ headerHash))
        db.write(writeOptions, batch)
      finally batch.close()
    }

  def getBlock(headerHash: Hash): Option[Array[Byte]] =
    Option(db.get(blocksCf, key(headerHash)))

  def getHeader(headerHash: Hash): Option[Array[Byte]] =
    Option(db.get(headersCf, key(headerHash)))

  def hasBlock(headerHash: Hash): Boolean =
    db.get(blocksCf, key(headerHash)) != null

  /** Children of a block, for fork-tree traversal. A truncated trailing entry is ignored. */
  def children(parent: Hash): List[Hash] =
    Option(db.get(childrenCf, key(parent))) match
      case None => Nil
      case Some(bytes) =>
        (0 until bytes.length / 32).map(i => Hash.wrap(java.util.Arrays.copyOfRange(bytes, i * 32, i * 32 + 32))).toList

  private def encodeChildren(hs: List[Hash]): Array[Byte] =
    val out = new Array[Byte](hs.length * 32)
    hs.zipWithIndex.foreach((h, i) => System.arraycopy(h.toArrayUnsafe, 0, out, i * 32, 32))
    out

  /** Delete a block, its header and its own children list, and unlink it from `parent`'s children, atomically. */
  def deleteBlock(headerHash: Hash, parent: Hash): Unit =
    childrenLock.synchronized {
      val batch = new WriteBatch()
      try
        batch.delete(blocksCf, key(headerHash))
        batch.delete(headersCf, key(headerHash))
        batch.delete(childrenCf, key(headerHash))
        val siblings = children(parent)
        if siblings.contains(headerHash) then
          val rest = siblings.filterNot(_ == headerHash)
          if rest.isEmpty then batch.delete(childrenCf, key(parent))
          else batch.put(childrenCf, key(parent), encodeChildren(rest))
        db.write(writeOptions, batch)
      finally batch.close()
    }

  // --- named pointers / metadata ---

  def setMeta(name: String, value: Array[Byte]): Unit =
    db.put(metaCf, name.getBytes("UTF-8"), value)

  def getMeta(name: String): Option[Array[Byte]] =
    Option(db.get(metaCf, name.getBytes("UTF-8")))

  def deleteMeta(name: String): Unit =
    db.delete(metaCf, name.getBytes("UTF-8"))

  def setHead(name: String, hash: Hash): Unit = setMeta(name, hash.bytes)

  def getHead(name: String): Option[Hash] = getMeta(name).map(Hash(_))

  override def close(): Unit =
    writeOptions.close()
    ownedHandles.foreach(_.close())
    db.close()
    cfOptions.close()
    dbOptions.close()

object BlockStore:
  val BestHead = "best"
  val FinalizedHead = "finalized"
  val GenesisHead = "genesis"

  private val CfNames = Seq("blocks", "headers", "children", "meta")

  def open(path: Path): BlockStore =
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
    val db =
      try RocksDB.open(options, path.toString, descriptors.asJava, handles)
      catch
        case e: Throwable =>
          options.close()
          cfOptions.close()
          throw e
    val hs = handles.asScala.toSeq
    new BlockStore(
      db,
      blocksCf = hs(1),
      headersCf = hs(2),
      childrenCf = hs(3),
      metaCf = hs(4),
      ownedHandles = hs,
      cfOptions = cfOptions,
      dbOptions = options
    )
