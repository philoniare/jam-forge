package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.trie.StateTrieStore
import io.forge.jam.core.types.block.Block
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend}
import io.forge.jam.protocol.traces.{BlockImporter, ImportResult, RawState}
import scodec.bits.ByteVector

import scala.collection.mutable

/** Owns the canonical chain: the persistent state trie, block storage and the
  * best/finalized heads, and drives [[BlockImporter]] for every new block.
  *
  * Fork handling is currently linear (a new block must extend the best head);
  * fork-tree selection arrives with the GRANDPA work.
  */
final class ChainManager(
    val config: ChainConfig,
    trieBackend: RocksDbTrieBackend,
    val blockStore: BlockStore
) extends LazyLogging:

  private val trieStore = new StateTrieStore(trieBackend)
  private val importer =
    new BlockImporter(config, skipAncestryValidation = false, externalTrieStore = Some(trieStore))
  private val blockCodec = Block.blockCodec(config)

  /** Recent ancestor headers (newest first) for lookup-anchor validation. */
  private val recentHeaders = mutable.ArrayDeque.empty[(Long, Hash)]

  final case class Head(hash: Hash, slot: Long, stateRoot: Hash)

  @volatile private var bestHead: Head = Head(Hash.zero, 0L, Hash.zero)

  def best: Head = bestHead

  /** For now finality is the genesis block (GRANDPA lands later). */
  def finalized: Head =
    val hash = blockStore.getHead(BlockStore.FinalizedHead).getOrElse(Hash.zero)
    Head(hash, metaLong("finalized_slot").getOrElse(0L), Hash.zero)

  /** Leaves of the (currently linear) chain: the best head. */
  def leaves: List[Head] = List(bestHead)

  private def metaLong(name: String): Option[Long] =
    blockStore.getMeta(name).map(b => java.nio.ByteBuffer.wrap(b).getLong)

  private def putMetaLong(name: String, v: Long): Unit =
    blockStore.setMeta(name, java.nio.ByteBuffer.allocate(8).putLong(v).array())

  /** Initialize a fresh database from the chain spec, or restore heads from a
    * previous run. Returns true when genesis was (re-)initialized.
    */
  def initializeOrRestore(spec: ChainSpec): Boolean =
    blockStore.getHead(BlockStore.GenesisHead) match
      case Some(_) =>
        // Restore: re-pin the persisted state root.
        val root = blockStore
          .getMeta("state_root")
          .map(Hash(_))
          .getOrElse(throw new IllegalStateException("missing state_root meta"))
        trieStore.markCommitted(root)
        val bestHash = blockStore.getHead(BlockStore.BestHead).get
        bestHead = Head(bestHash, metaLong("best_slot").getOrElse(0L), root)
        logger.info(
          s"restored chain: best=${bestHash.toHex.take(18)} slot=${bestHead.slot} root=${root.toHex.take(18)}"
        )
        false
      case None =>
        val root =
          trieStore.bootstrap(spec.genesisState.map(kv => (kv.key, kv.value)))
        val genesisHash = spec.genesisHeaderHash
        spec.genesisHeaderBytes.foreach { hb =>
          blockStore.putBlock(genesisHash, Hash.zero, hb, Array.emptyByteArray)
        }
        blockStore.setHead(BlockStore.GenesisHead, genesisHash)
        blockStore.setHead(BlockStore.BestHead, genesisHash)
        blockStore.setHead(BlockStore.FinalizedHead, genesisHash)
        blockStore.setMeta("state_root", root.bytes.toArray)
        putMetaLong("best_slot", 0L)
        bestHead = Head(genesisHash, 0L, root)
        logger.info(
          s"initialized genesis ${genesisHash.toHex.take(18)} root=${root.toHex.take(18)}"
        )
        true

  def decodeBlock(bytes: Array[Byte]): Either[String, Block] =
    blockCodec.decode(ByteVector(bytes).bits) match
      case scodec.Attempt.Successful(res) if res.remainder.isEmpty =>
        Right(res.value)
      case scodec.Attempt.Successful(_) => Left("trailing bytes after block")
      case scodec.Attempt.Failure(err)  => Left(err.message)

  def encodeBlock(block: Block): Array[Byte] =
    blockCodec.encode(block) match
      case scodec.Attempt.Successful(bits) => bits.toByteArray
      case scodec.Attempt.Failure(err) =>
        throw new IllegalStateException(s"block encode failed: ${err.message}")

  def headerHashOf(block: Block): Hash =
    Hashing.blake2b256(block.header.encode.toArray)

  /** Import a block extending the best head, persist it and advance the head.
    */
  def importBlock(blockBytes: Array[Byte]): Either[String, Head] =
    synchronized {
      decodeBlock(blockBytes).flatMap { block =>
        val parent = block.header.parent
        if parent != bestHead.hash then
          Left(
            s"block parent ${parent.toHex.take(18)} does not extend best head ${bestHead.hash.toHex.take(18)}"
          )
        else
          val preState = RawState(bestHead.stateRoot, Nil)
          importer.importBlock(block, preState) match
            case ImportResult.Failure(error, message) =>
              Left(s"$error: $message")
            case ImportResult.Success(postRoot, _) =>
              val hash = headerHashOf(block)
              val slot = block.header.slot.value.toLong
              blockStore.putBlock(hash, parent, block.header.encode.toArray, blockBytes)
              blockStore.setHead(BlockStore.BestHead, hash)
              blockStore.setMeta("state_root", postRoot.bytes.toArray)
              putMetaLong("best_slot", slot)
              recentHeaders.prepend((slot, hash))
              while recentHeaders.size > config.maxLookupAnchorAge.toInt + 1 do
                recentHeaders.removeLast()
              bestHead = Head(hash, slot, postRoot)
              logger.info(
                s"imported block ${hash.toHex.take(18)} slot=$slot root=${postRoot.toHex.take(18)}"
              )
              Right(bestHead)
      }
    }

  def hasBlock(hash: Hash): Boolean = blockStore.hasBlock(hash)

  /** Walk `max` blocks from `from` following parents (descending, inclusive).
    */
  def blocksDescending(from: Hash, max: Int): List[Array[Byte]] =
    val out = mutable.ListBuffer.empty[Array[Byte]]
    var cursor = from
    var n = 0
    while n < max && cursor != Hash.zero do
      blockStore.getBlock(cursor) match
        case Some(bytes) if bytes.nonEmpty =>
          out += bytes
          decodeBlock(bytes) match
            case Right(block) => cursor = block.header.parent
            case Left(_)      => cursor = Hash.zero
          n += 1
        case _ =>
          cursor = Hash.zero
    out.toList

  /** Walk up to `max` blocks strictly after `from` (ascending, exclusive)
    * along the linear chain via the children index.
    */
  def blocksAscending(from: Hash, max: Int): List[Array[Byte]] =
    val out = mutable.ListBuffer.empty[Array[Byte]]
    var cursor = from
    var continue = true
    while out.size < max && continue do
      blockStore.children(cursor) match
        case child :: _ =>
          blockStore.getBlock(child) match
            case Some(bytes) if bytes.nonEmpty =>
              out += bytes
              cursor = child
            case _ => continue = false
        case Nil => continue = false
    out.toList
