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
  * Forks: blocks with a known non-best parent are stored unvalidated on a
  * side branch; when a branch becomes strictly longer than the best chain it
  * is adopted by rewinding to the common ancestor and replaying.
  */
final class ChainManager(
    val config: ChainConfig,
    trieBackend: RocksDbTrieBackend,
    val blockStore: BlockStore
) extends LazyLogging:

  private val trieStore = new StateTrieStore(trieBackend)
  private val importer =
    new BlockImporter(
      config,
      skipAncestryValidation = false,
      externalTrieStore = Some(trieStore),
      gcAfterImport = false
    )
  private val blockCodec = Block.blockCodec(config)

  /** Recent ancestor headers (newest first) for lookup-anchor validation. */
  private val recentHeaders = mutable.ArrayDeque.empty[(Long, Hash)]

  final case class Head(hash: Hash, slot: Long, stateRoot: Hash)

  @volatile private var bestHead: Head = Head(Hash.zero, 0L, Hash.zero)

  /** Listeners invoked after every successful import (assurers, watchers). */
  private val importListeners =
    new java.util.concurrent.CopyOnWriteArrayList[(Head, Block) => Unit]()

  def onImported(listener: (Head, Block) => Unit): Unit =
    importListeners.add(listener)
    ()

  def best: Head = bestHead

  def finalized: Head =
    val hash = blockStore.getHead(BlockStore.FinalizedHead).getOrElse(Hash.zero)
    Head(hash, metaLong("finalized_slot").getOrElse(0L), Hash.zero)

  /** Mark a main-chain block as finalized: the finalized head advances and
    * side branches forking at or below it are no longer adoptable
    */
  def finalize(hash: Hash): Either[String, Head] =
    synchronized {
      blockRoot(hash) match
        case None => Left("cannot finalize a non-main-chain block")
        case Some(_) =>
          val slot = blockStore
            .getBlock(hash)
            .filter(_.nonEmpty)
            .flatMap(b => decodeBlock(b).toOption)
            .map(_.header.slot.value.toLong)
            .getOrElse(0L)
          val current = finalized
          if metaLong(s"height:${hash.toHex}").getOrElse(0L) <
            metaLong(s"height:${current.hash.toHex}").getOrElse(0L)
          then Left("cannot finalize below the finalized head")
          else
            blockStore.setHead(BlockStore.FinalizedHead, hash)
            putMetaLong("finalized_slot", slot)
            logger.info(s"finalized ${hash.toHex.take(18)} slot=$slot")
            Right(Head(hash, slot, Hash.zero))
    }

  /** Devnet finality rule: finalize the ancestor `depth` blocks behind the
    * best head (a stand-in for GRANDPA supermajority votes; the finalized
    * pointer feeds the UP 0 handshake). Returns the newly finalized head when
    * it advanced.
    */
  def finalizeAtDepth(depth: Int): Option[Head] =
    val bestHeight = blockHeight(bestHead.hash).getOrElse(0L)
    val targetHeight = bestHeight - depth
    if targetHeight <= metaLong(s"height:${finalized.hash.toHex}").getOrElse(0L) then None
    else
      // Walk back from best to the target height.
      var cursor = bestHead.hash
      var h = bestHeight
      while h > targetHeight do
        decodeBlock(blockStore.getBlock(cursor).get) match
          case Right(b) => cursor = b.header.parent
          case Left(_)  => return None
        h -= 1
      finalize(cursor).toOption

  /** Leaves of the (currently linear) chain: the best head. */
  def leaves: List[Head] = List(bestHead)

  private def metaLong(name: String): Option[Long] =
    blockStore.getMeta(name).map(b => java.nio.ByteBuffer.wrap(b).getLong)

  private def putMetaLong(name: String, v: Long): Unit =
    blockStore.setMeta(name, java.nio.ByteBuffer.allocate(8).putLong(v).array())

  // Per-block chain metadata. A stored post-state root marks a block as part
  // of the (validated) main chain — side-branch blocks are stored unvalidated
  // and only gain a root when a reorg replays them.
  private def blockRoot(h: Hash): Option[Hash] =
    blockStore.getMeta(s"blockroot:${h.toHex}").filter(_.length == 32).map(Hash(_))
  private def putBlockRoot(h: Hash, root: Hash): Unit =
    blockStore.setMeta(s"blockroot:${h.toHex}", root.bytes.toArray)
  private def dropBlockRoot(h: Hash): Unit =
    blockStore.setMeta(s"blockroot:${h.toHex}", Array.emptyByteArray)
  private def blockHeight(h: Hash): Option[Long] =
    metaLong(s"height:${h.toHex}")
  private def putBlockHeight(h: Hash, height: Long): Unit =
    putMetaLong(s"height:${h.toHex}", height)

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
        putBlockRoot(genesisHash, root)
        putBlockHeight(genesisHash, 0L)
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

  /** Import a block: extends the best head directly, or is stored as a side
    * branch and adopted via reorg when its branch becomes strictly longer
    */
  def importBlock(blockBytes: Array[Byte]): Either[String, Head] =
    synchronized {
      decodeBlock(blockBytes).flatMap { block =>
        val hash = headerHashOf(block)
        val parent = block.header.parent
        if blockRoot(hash).isDefined then Left("block already imported")
        else if parent == bestHead.hash then importOnBest(block, blockBytes)
        else
          (blockHeight(parent), blockStore.hasBlock(parent)) match
            case (Some(parentHeight), true) =>
              // Known parent on some branch: store and evaluate.
              blockStore.putBlock(hash, parent, block.header.encode.toArray, blockBytes)
              val height = parentHeight + 1
              putBlockHeight(hash, height)
              val bestHeight = blockHeight(bestHead.hash).getOrElse(0L)
              if height > bestHeight then reorgTo(hash)
              else
                Left(
                  s"stored on side branch (height $height <= best $bestHeight)"
                )
            case _ =>
              Left(s"unknown parent ${parent.toHex.take(18)}")
      }
    }

  /** Validate and apply a block that extends the best head. */
  private def importOnBest(block: Block, blockBytes: Array[Byte]): Either[String, Head] =
    val parent = block.header.parent
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
        putBlockRoot(hash, postRoot)
        putBlockHeight(hash, blockHeight(parent).getOrElse(0L) + 1)
        recentHeaders.prepend((slot, hash))
        while recentHeaders.size > config.maxLookupAnchorAge.toInt + 1 do
          recentHeaders.removeLast()
        bestHead = Head(hash, slot, postRoot)
        logger.info(
          s"imported block ${hash.toHex.take(18)} slot=$slot root=${postRoot.toHex.take(18)}"
        )
        importListeners.forEach { l =>
          try l(bestHead, block)
          catch case e: Exception => logger.error("import listener failed", e)
        }
        Right(bestHead)

  /** Adopt the branch ending at `tip`: rewind to the common ancestor (the
    * nearest tip-ancestor with a validated post-state root) and replay the
    * branch
    */
  private def reorgTo(tip: Hash): Either[String, Head] =
    // Collect the unvalidated branch (tip backwards until a validated block).
    val branch = mutable.ListBuffer.empty[Array[Byte]]
    var cursor = tip
    while blockRoot(cursor).isEmpty do
      blockStore.getBlock(cursor) match
        case Some(bytes) if bytes.nonEmpty =>
          branch.prepend(bytes)
          decodeBlock(bytes) match
            case Right(b) => cursor = b.header.parent
            case Left(e)  => return Left(s"reorg: undecodable branch block: $e")
        case _ => return Left("reorg: branch block missing")
    val ancestor = cursor
    val ancestorRoot = blockRoot(ancestor).get

    // Never rewind at or below finality.
    val finalizedHeight =
      metaLong(s"height:${finalized.hash.toHex}").getOrElse(0L)
    if blockHeight(ancestor).getOrElse(0L) < finalizedHeight then
      return Left("reorg would revert finalized blocks")

    val previousBest = bestHead
    // Abandoned main-chain blocks (best back to ancestor) lose main status.
    val abandoned = mutable.ListBuffer.empty[Hash]
    var back = previousBest.hash
    while back != ancestor do
      abandoned += back
      decodeBlock(blockStore.getBlock(back).get) match
        case Right(b) => back = b.header.parent
        case Left(e)  => return Left(s"reorg: undecodable main block: $e")

    logger.info(
      s"reorg: rewinding ${abandoned.size} block(s) to ${ancestor.toHex.take(18)}, " +
        s"replaying ${branch.size}"
    )
    trieStore.markCommitted(ancestorRoot)
    val ancestorBlock = blockStore.getBlock(ancestor)
    val ancestorSlot =
      ancestorBlock.filter(_.nonEmpty).flatMap(b => decodeBlock(b).toOption)
        .map(_.header.slot.value.toLong)
        .getOrElse(0L)
    bestHead = Head(ancestor, ancestorSlot, ancestorRoot)

    val replayed = mutable.ListBuffer.empty[Hash]
    branch.foreach { bytes =>
      decodeBlock(bytes).flatMap(b => importOnBest(b, bytes)) match
        case Right(head) =>
          replayed += head.hash
        case Left(err) =>
          // Roll back: restore the previous chain and clear the roots the
          // partial replay recorded.
          replayed.foreach(dropBlockRoot)
          trieStore.markCommitted(previousBest.stateRoot)
          bestHead = previousBest
          blockStore.setHead(BlockStore.BestHead, previousBest.hash)
          blockStore.setMeta("state_root", previousBest.stateRoot.bytes.toArray)
          putMetaLong("best_slot", previousBest.slot)
          return Left(s"reorg replay failed: $err")
    }
    abandoned.foreach(dropBlockRoot)
    Right(bestHead)

  def hasBlock(hash: Hash): Boolean = blockStore.hasBlock(hash)

  /** Raw read of a 31-byte state key from the current best state. */
  def readRawState(stateKey: JamBytes): Option[JamBytes] =
    trieStore.at(bestHead.stateRoot).read(stateKey)

  /** A read view over the current best state (mutations are staged in the
    * view and discarded; imports go through importBlock).
    */
  def stateView(): io.forge.jam.protocol.state.TrieBackedJamState =
    val trie = trieStore.at(bestHead.stateRoot)
    new io.forge.jam.protocol.state.TrieBackedJamState(
      trie,
      config,
      new io.forge.jam.protocol.state.ServiceStorageView(trie),
      Some(trieStore)
    )

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
