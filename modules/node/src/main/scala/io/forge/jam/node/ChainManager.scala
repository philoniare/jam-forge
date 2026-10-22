package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.trie.{StateTrie, StateTrieStore}
import io.forge.jam.core.types.block.Block
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend}
import io.forge.jam.protocol.state.{ServiceStorageView, TrieBackedJamState}
import io.forge.jam.protocol.traces.{BlockImporter, ImportResult, RawState}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Owns the canonical chain: the persistent state trie, block storage and the
  * best/finalized heads, and drives [[BlockImporter]] for every new block.
  *
  * Forks: blocks with a known non-best parent are stored unvalidated on a
  * side branch; when a branch becomes strictly heavier than the best chain
  * and contains no equivocation, it is adopted by rewinding to the common
  * ancestor and replaying.
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

  final case class Head(hash: Hash, slot: Long, stateRoot: Hash)

  @volatile private var bestHead: Head = Head(Hash.zero, 0L, Hash.zero)

  private val leafMap = new java.util.concurrent.ConcurrentHashMap[Hash, Head]()

  /** Listeners invoked after every successful import (assurers, watchers). */
  private val importListeners =
    new java.util.concurrent.CopyOnWriteArrayList[(Head, Block) => Unit]()

  def onImported(listener: (Head, Block) => Unit): Unit =
    importListeners.add(listener)
    ()

  private val postImportBus = new PostImportBus[Head, Block]((head, block) =>
    importListeners.forEach { l =>
      try l(head, block)
      catch case e: Exception => logger.error("import listener failed", e)
    }
  )

  def shutdown(): Unit = postImportBus.close()

  def best: Head = bestHead

  def finalized: Head =
    val hash = blockStore.getHead(BlockStore.FinalizedHead).getOrElse(Hash.zero)
    val root = blockStore.getMeta("finalized_root").filter(_.length == 32).map(Hash(_)).getOrElse(Hash.zero)
    Head(hash, metaLong("finalized_slot").getOrElse(0L), root)

  /** Mark a main-chain block as finalized: the finalized head advances and
    * side branches forking at or below it are no longer adoptable
    */
  def finalize(hash: Hash): Either[String, Head] =
    synchronized {
      blockRoot(hash) match
        case None => Left("cannot finalize a non-main-chain block")
        case Some(root) =>
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
            blockStore.setMeta("finalized_root", root.bytes.toArray)
            val finalizedHeight = metaLong(s"height:${hash.toHex}").getOrElse(0L)
            val it = leafMap.entrySet().iterator()
            while it.hasNext do
              val e = it.next()
              if blockHeight(e.getKey).getOrElse(0L) <= finalizedHeight then it.remove()
            logger.info(s"finalized ${hash.toHex.take(18)} slot=$slot")
            Right(Head(hash, slot, root))
    }

  /** Devnet finality rule: finalize the ancestor `depth` blocks behind the
    * best head once at least `quorum` nodes hold it on their best chain: this
    * node plus every peer whose announced best (`peerBests`) descends from it.
    * A stand-in for GRANDPA's supermajority; with a quorum
    * of 2V/3+1 neither side of a network split can finalize alone, so a healed
    * split can still reorg. The finalized pointer feeds the UP 0 handshake.
    * Returns the newly finalized head when it advanced.
    */
  def finalizeAtDepth(depth: Int, peerBests: Iterable[Hash] = Nil, quorum: Int = 1): Option[Head] =
    val bestHeight = blockHeight(bestHead.hash).getOrElse(0L)
    val targetHeight = bestHeight - depth
    if targetHeight <= metaLong(s"height:${finalized.hash.toHex}").getOrElse(0L) then None
    else
      ancestorAtHeight(bestHead.hash, targetHeight) match
        case None => None
        case Some(target) =>
          // One entry per peer: peers at the same head each count.
          val bests = peerBests.toList
          val supporters = 1 + bests.count(b => ancestorAtHeight(b, targetHeight).contains(target))
          if supporters < quorum then
            logger.debug(
              s"finality at height $targetHeight waits: $supporters of $quorum (peer bests ${bests.size})"
            )
            None
          else finalize(target).toOption

  /** Ancestor of stored block `hash` (inclusive) at `height`, following
    * parent links from the stored headers; None when the walk leaves the
    * store or `hash` is below `height`.
    */
  def ancestorAtHeight(hash: Hash, height: Long): Option[Hash] =
    blockHeight(hash).filter(_ >= height).flatMap { start =>
      var cursor: Option[Hash] = Some(hash)
      var h = start
      while h > height && cursor.isDefined do
        cursor = cursor.flatMap(parentOf)
        h -= 1
      cursor
    }

  private def parentOf(h: Hash): Option[Hash] =
    blockStore.getHeader(h).filter(_.length >= 32).map(b => Hash(b.take(32)))

  def leaves: List[Head] =
    val best = bestHead
    val rest = leafMap.values().asScala.filterNot(_.hash == best.hash).toList
    best :: rest

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
    blockStore.deleteMeta(s"blockroot:${h.toHex}")
  private def blockHeight(h: Hash): Option[Long] =
    metaLong(s"height:${h.toHex}")
  private def putBlockHeight(h: Hash, height: Long): Unit =
    putMetaLong(s"height:${h.toHex}", height)

  // Fork-choice metadata, derived from headers alone so unvalidated side
  // branches can be weighed:
  //   ticketed:<h>  block h's epoch is sealed by tickets (γ'_s ∈ tickets)
  //   marked:<h>    h's chain carries a winning-tickets marker in h's epoch
  //   weight:<h>    number of ticket-sealed blocks among h and its ancestors
  // γ'_s takes the ticket sequence exactly when e' = e + 1 and the previous
  // epoch closed with a saturated accumulator, which is when that epoch's
  // chain carried H_w (§safrole eqs. slotkeysequence, winningticketsmarker).
  private def flag(name: String): Boolean = metaLong(name).contains(1L)
  def weightOf(h: Hash): Long = metaLong(s"weight:${h.toHex}").getOrElse(0L)

  private def recordForkChoiceMeta(hash: Hash, block: Block): Unit =
    val parent = block.header.parent
    val slot = block.header.slot.value.toLong
    val epoch = slot / config.epochLength
    val parentEpoch = slotOfHeader(parent) / config.epochLength
    val ticketed =
      if epoch == parentEpoch then flag(s"ticketed:${parent.toHex}")
      else epoch == parentEpoch + 1 && flag(s"marked:${parent.toHex}")
    val marked = block.header.ticketsMark.isDefined || (epoch == parentEpoch && flag(s"marked:${parent.toHex}"))
    putMetaLong(s"ticketed:${hash.toHex}", if ticketed then 1L else 0L)
    putMetaLong(s"marked:${hash.toHex}", if marked then 1L else 0L)
    putMetaLong(s"weight:${hash.toHex}", weightOf(parent) + (if ticketed then 1L else 0L))
    recordSlotAuthor(hash, slot, block.header.authorIndex.toInt)

  /** Slot from stored header bytes (parent ++ prior root ++ extrinsic hash ++
    * slot as E_4), 0 for genesis or unknown blocks.
    */
  private def slotOfHeader(h: Hash): Long =
    blockStore.getHeader(h).filter(_.length >= 100)
      .map(b => io.forge.jam.core.LittleEndian.get(b, 96, 4))
      .getOrElse(0L)

  /** Equivocation tracking: two distinct blocks at one timeslot by one author
    * A branch holding an unfinalized equivocated slot
    * is never adopted.
    */
  private def recordSlotAuthor(hash: Hash, slot: Long, author: Int): Unit =
    val key = s"slotauthor:$slot:$author"
    blockStore.getMeta(key).map(Hash(_)) match
      case None                    => blockStore.setMeta(key, hash.bytes.toArray)
      case Some(seen) if seen != hash =>
        if !flag(s"equivocated:$slot") then
          logger.warn(
            s"equivocation at slot $slot by validator $author: ${seen.toHex.take(18)} vs ${hash.toHex.take(18)}"
          )
        putMetaLong(s"equivocated:$slot", 1L)
      case _ => ()

  private def isEquivocated(h: Hash): Boolean = flag(s"equivocated:${slotOfHeader(h)}")

  /** Mark genesis as ticket-sealed when its state already carries a ticket
    * sealer sequence (γ_s ∈ tickets).
    */
  private def recordGenesisForkChoiceMeta(genesisHash: Hash): Unit =
    val ticketed = stateView().gamma.st match
      case io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Tickets(_) => true
      case _                                                                => false
    putMetaLong(s"ticketed:${genesisHash.toHex}", if ticketed then 1L else 0L)
    putMetaLong(s"marked:${genesisHash.toHex}", 0L)
    putMetaLong(s"weight:${genesisHash.toHex}", 0L)

  /** Slot of a stored block, decoded from its full block bytes; 0 when the
    * block bytes are missing/empty (genesis, whose body is never stored) or
    * undecodable. Mirrors the existing `finalize`/`finalizeAtDepth` pattern.
    */
  private def blockSlot(h: Hash): Long =
    blockStore
      .getBlock(h)
      .filter(_.nonEmpty)
      .flatMap(b => decodeBlock(b).toOption)
      .map(_.header.slot.value.toLong)
      .getOrElse(0L)

  /** The `Head` for an already-imported block, for leaf bookkeeping:
    * validated post-state root when known, `Hash.zero` for an unvalidated
    * side-branch block.
    */
  private def headOf(h: Hash): Head = Head(h, blockSlot(h), blockRoot(h).getOrElse(Hash.zero))

  /** Rebuild `leafMap` from scratch by walking `blockStore.children` from
    * genesis: any reachable block with no imported children is a leaf. Used
    * on restore, where the in-memory `leafMap` built during the previous
    * process run is gone but the persisted parent -> children index isn't.
    */
  private def rebuildLeaves(genesisHash: Hash): Unit =
    leafMap.clear()
    val finalizedHeight =
      blockStore.getHead(BlockStore.FinalizedHead).flatMap(blockHeight).getOrElse(0L)
    val stack = mutable.ArrayDeque(genesisHash)
    val visited = mutable.HashSet.empty[Hash]
    while stack.nonEmpty do
      val h = stack.removeHead()
      if !visited.contains(h) then
        visited += h
        blockStore.children(h) match
          case Nil =>
            if blockHeight(h).getOrElse(0L) > finalizedHeight then leafMap.put(h, headOf(h))
          case children => children.foreach(stack.prepend)

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
        val bestHash = blockStore
          .getHead(BlockStore.BestHead)
          .getOrElse(
            throw new IllegalStateException(
              "block store invariant: genesis head present but BestHead missing on restore"
            )
          )
        bestHead = Head(bestHash, metaLong("best_slot").getOrElse(0L), root)
        val genesisHash = blockStore
          .getHead(BlockStore.GenesisHead)
          .getOrElse(
            throw new IllegalStateException(
              "block store invariant: genesis head present but its hash lookup failed on restore"
            )
          )
        rebuildLeaves(genesisHash)
        if blockStore.getMeta("finalized_root").isEmpty then
          blockStore.getHead(BlockStore.FinalizedHead).flatMap(blockRoot).foreach { finalizedRoot =>
            blockStore.setMeta("finalized_root", finalizedRoot.bytes.toArray)
          }
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
        putMetaLong("finalized_slot", 0L)
        blockStore.setMeta("finalized_root", root.bytes.toArray)
        putBlockRoot(genesisHash, root)
        putBlockHeight(genesisHash, 0L)
        bestHead = Head(genesisHash, 0L, root)
        recordGenesisForkChoiceMeta(genesisHash)
        leafMap.clear()
        leafMap.put(genesisHash, bestHead)
        logger.info(
          s"initialized genesis ${genesisHash.toHex.take(18)} root=${root.toHex.take(18)}"
        )
        true

  def decodeBlock(bytes: Array[Byte]): Either[String, Block] =
    blockCodec.decodeAt(bytes, 0) match
      case Right((value, used)) if used == bytes.length =>
        Right(value)
      case Right(_) => Left("trailing bytes after block")
      case Left(err)  => Left(err)

  def encodeBlock(block: Block): Array[Byte] =
    blockCodec.encode(block)

  /** Split a CE 128 response (`[Block]`, the sequence length implied by the
    * message size) into the encoded blocks, decoding one at a time.
    */
  def splitBlocks(bytes: Array[Byte]): Either[String, List[Array[Byte]]] =
    val out = mutable.ListBuffer.empty[Array[Byte]]
    var offset = 0
    while offset < bytes.length do
      blockCodec.decodeAt(bytes, offset) match
        case Right((_, used)) if used > 0 =>
          out += java.util.Arrays.copyOfRange(bytes, offset, offset + used)
          offset += used
        case Right(_)  => return Left(s"empty block at offset $offset")
        case Left(err) => return Left(s"block at offset $offset: $err")
    Right(out.toList)

  def headerHashOf(block: Block): Hash =
    Hashing.blake2b256(block.header.encode.toArray)

  /** Import a block: extends the best head directly, or is stored as a side
    * branch and adopted via reorg when its branch becomes strictly heavier
    * (ticket-sealed count, then height) and holds no equivocation
    */
  def importBlock(blockBytes: Array[Byte]): Either[String, Head] =
    val events = mutable.ListBuffer.empty[(Head, Block)]
    val result = synchronized {
      decodeBlock(blockBytes).flatMap { block =>
        val hash = headerHashOf(block)
        val parent = block.header.parent
        if blockRoot(hash).isDefined then Left("block already imported")
        else if parent == bestHead.hash then importOnBest(block, blockBytes, events)
        else
          (blockHeight(parent), blockStore.hasBlock(parent)) match
            case (Some(parentHeight), true) =>
              // Known parent on some branch: store and evaluate.
              blockStore.putBlock(hash, parent, block.header.encode.toArray, blockBytes)
              val height = parentHeight + 1
              putBlockHeight(hash, height)
              recordForkChoiceMeta(hash, block)
              leafMap.remove(parent)
              if blockStore.children(hash).isEmpty then
                leafMap.put(hash, Head(hash, block.header.slot.value.toLong, Hash.zero))
              else leafMap.remove(hash)
              val bestHeight = blockHeight(bestHead.hash).getOrElse(0L)
              val (weight, bestWeight) = (weightOf(hash), weightOf(bestHead.hash))
              val heavier = weight > bestWeight || (weight == bestWeight && height > bestHeight)
              if heavier && !branchEquivocates(hash) then reorgTo(hash, events)
              else
                Left(
                  s"stored on side branch (weight $weight height $height vs best $bestWeight/$bestHeight)"
                )
            case _ =>
              Left(s"unknown parent ${parent.toHex.take(18)}")
      }
    }
    events.foreach((head, block) => postImportBus.publish(head, block))
    result

  private def importOnBest(
      block: Block,
      blockBytes: Array[Byte],
      events: mutable.ListBuffer[(Head, Block)]
  ): Either[String, Head] =
    val parent = block.header.parent
    val preState = RawState(bestHead.stateRoot, Nil)
    importer.importBlock(block, preState) match
      case ImportResult.Failure(error, message) =>
        Left(s"$error: $message")
      case ImportResult.Success(postRoot, _) =>
        val hash = headerHashOf(block)
        val slot = block.header.slot.value.toLong
        blockStore.putBlock(hash, parent, block.header.encode.toArray, blockBytes)
        recordForkChoiceMeta(hash, block)
        blockStore.setHead(BlockStore.BestHead, hash)
        blockStore.setMeta("state_root", postRoot.bytes.toArray)
        putMetaLong("best_slot", slot)
        putBlockRoot(hash, postRoot)
        putBlockHeight(hash, blockHeight(parent).getOrElse(0L) + 1)
        bestHead = Head(hash, slot, postRoot)
        leafMap.remove(parent)
        if blockStore.children(hash).isEmpty then leafMap.put(hash, bestHead)
        else leafMap.remove(hash)
        logger.info(
          s"imported block ${hash.toHex.take(18)} slot=$slot root=${postRoot.toHex.take(18)}"
        )
        events += ((bestHead, block))
        Right(bestHead)

  /** Whether the unvalidated branch ending at `tip` (back to its first
    * validated ancestor) holds a block at an equivocated slot.
    */
  private def branchEquivocates(tip: Hash): Boolean =
    var cursor = tip
    while blockRoot(cursor).isEmpty do
      if isEquivocated(cursor) then return true
      parentOf(cursor) match
        case Some(p) => cursor = p
        case None    => return false
    false

  /** Adopt the branch ending at `tip`: rewind to the common ancestor (the
    * nearest tip-ancestor with a validated post-state root) and replay the
    * branch
    */
  private def reorgTo(
      tip: Hash,
      events: mutable.ListBuffer[(Head, Block)]
  ): Either[String, Head] =
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
    // The loop above exits precisely when `blockRoot(cursor)` is defined.
    val ancestorRoot = blockRoot(ancestor).getOrElse(
      throw new IllegalStateException(
        s"block store invariant: common ancestor ${ancestor.toHex} has no validated post-state root during reorg to ${tip.toHex}"
      )
    )

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
      val backBytes = blockStore
        .getBlock(back)
        .getOrElse(
          throw new IllegalStateException(
            s"block store invariant: main-chain block ${back.toHex} missing while rewinding to ${ancestor.toHex}"
          )
        )
      decodeBlock(backBytes) match
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
      decodeBlock(bytes).flatMap(b => importOnBest(b, bytes, events)) match
        case Right(head) =>
          replayed += head.hash
        case Left(err) =>
          replayed.foreach(dropBlockRoot)
          trieStore.markCommitted(previousBest.stateRoot)
          bestHead = previousBest
          blockStore.setHead(BlockStore.BestHead, previousBest.hash)
          blockStore.setMeta("state_root", previousBest.stateRoot.bytes.toArray)
          putMetaLong("best_slot", previousBest.slot)
          return Left(s"reorg replay failed: $err")
    }
    abandoned.foreach { h =>
      dropBlockRoot(h)
      Option(leafMap.get(h)).foreach(head => leafMap.put(h, head.copy(stateRoot = Hash.zero)))
    }
    Right(bestHead)

  def hasBlock(hash: Hash): Boolean = blockStore.hasBlock(hash)

  /** Raw read of a 31-byte state key from the current best state. */
  def readRawState(stateKey: JamBytes): Option[JamBytes] =
    trieStore.at(bestHead.stateRoot).read(stateKey)

  def stateAt(headerHash: Hash): Option[StateTrie] =
    blockRoot(headerHash).map(root => StateTrie.at(trieStore.backend, root))

  /** A read view over the current best state (mutations are staged in the
    * view and discarded; imports go through importBlock).
    */
  def stateView(): TrieBackedJamState = {
    val root = bestHead.stateRoot
    val trie = StateTrie.at(trieStore.backend, root)
    new TrieBackedJamState(
      trie,
      config,
      new ServiceStorageView(trie),
      Some(trieStore)
    )
  }

  /** Whether stored block `hash` can still be finalized from our view: it
    * is the finalized block, one of its ancestors, or one of its descendants
    * (JAMNP-S CE 128 must not return any other block).
    */
  def canBeFinalized(hash: Hash): Boolean =
    val fin = finalized
    (blockHeight(hash), blockHeight(fin.hash)) match
      case (Some(h), Some(fh)) =>
        if h <= fh then ancestorAtHeight(fin.hash, h).contains(hash)
        else ancestorAtHeight(hash, fh).contains(fin.hash)
      case _ => false

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
      blockStore.children(cursor).filter(canBeFinalized) match
        case child :: _ =>
          blockStore.getBlock(child) match
            case Some(bytes) if bytes.nonEmpty =>
              out += bytes
              cursor = child
            case _ => continue = false
        case Nil => continue = false
    out.toList
