package io.forge.jam.db

import java.nio.file.Path

import io.forge.jam.core.primitives.Hash
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, ColumnFamilyOptions, DBOptions, RocksDB}

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
    ownedHandles: Seq[ColumnFamilyHandle]
) extends AutoCloseable:

  private inline def key(h: Hash): Array[Byte] = h.bytes.toArray

  /** Store a block (and its header) under its header hash, indexing it as a
    * child of `parent`.
    */
  def putBlock(
      headerHash: Hash,
      parent: Hash,
      headerBytes: Array[Byte],
      blockBytes: Array[Byte]
  ): Unit =
    db.put(blocksCf, key(headerHash), blockBytes)
    db.put(headersCf, key(headerHash), headerBytes)
    addChild(parent, headerHash)

  def getBlock(headerHash: Hash): Option[Array[Byte]] =
    Option(db.get(blocksCf, key(headerHash)))

  def getHeader(headerHash: Hash): Option[Array[Byte]] =
    Option(db.get(headersCf, key(headerHash)))

  def hasBlock(headerHash: Hash): Boolean =
    db.get(blocksCf, key(headerHash)) != null

  /** Children of a block, for fork-tree traversal. */
  def children(parent: Hash): List[Hash] =
    Option(db.get(childrenCf, key(parent))) match
      case None => Nil
      case Some(bytes) =>
        bytes
          .grouped(32)
          .map(b => Hash(b))
          .toList

  private def addChild(parent: Hash, child: Hash): Unit =
    val existing = children(parent)
    if !existing.contains(child) then
      val out = new Array[Byte]((existing.length + 1) * 32)
      existing.zipWithIndex.foreach { case (h, i) =>
        System.arraycopy(h.bytes.toArray, 0, out, i * 32, 32)
      }
      System.arraycopy(child.bytes.toArray, 0, out, existing.length * 32, 32)
      db.put(childrenCf, key(parent), out)

  def deleteBlock(headerHash: Hash): Unit =
    db.delete(blocksCf, key(headerHash))
    db.delete(headersCf, key(headerHash))
    db.delete(childrenCf, key(headerHash))

  // --- named pointers / metadata ---

  def setMeta(name: String, value: Array[Byte]): Unit =
    db.put(metaCf, name.getBytes("UTF-8"), value)

  def getMeta(name: String): Option[Array[Byte]] =
    Option(db.get(metaCf, name.getBytes("UTF-8")))

  def setHead(name: String, hash: Hash): Unit = setMeta(name, hash.bytes.toArray)

  def getHead(name: String): Option[Hash] = getMeta(name).map(Hash(_))

  override def close(): Unit =
    ownedHandles.foreach(_.close())
    db.close()

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
    val db = RocksDB.open(options, path.toString, descriptors.asJava, handles)
    val hs = handles.asScala.toSeq
    new BlockStore(
      db,
      blocksCf = hs(1),
      headersCf = hs(2),
      childrenCf = hs(3),
      metaCf = hs(4),
      ownedHandles = hs
    )
