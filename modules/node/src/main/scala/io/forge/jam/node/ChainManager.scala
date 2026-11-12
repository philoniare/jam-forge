package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}
import io.forge.jam.core.trie.{StateTrie, StateTrieStore}
import io.forge.jam.core.types.block.Block
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend}
import io.forge.jam.protocol.pipeline.BlockPipeline
import io.forge.jam.protocol.report.ReportTypes.AncestorHeader
import io.forge.jam.protocol.state.{ServiceStorageView, TrieBackedJamState}
import io.forge.jam.protocol.traces.{BlockImporter, ImportResult, RawState}

import scala.collection.mutable
import scala.util.boundary, boundary.break
import scala.util.control.NonFatal
import scala.jdk.CollectionConverters.*

object ChainManager:
  /** Side-branch blocks stored without a seal check, in all. They are an honest fork's blocks only when the fork
    * is more than an epoch deep and not finalized, which a node can then still follow for this many blocks past the
    * point where seals stop being checkable.
    */
  val MaxUnverifiedBlocks: Int = 1024

  /** Finalized blocks pruned at one finalization. Finality advances a block or a few at a time, so this only
    * limits how fast a backlog (a node upgraded with a long history) is worked off.
    */
  val MaxPrunedPerFinalization: Int = 512

  /** Of those, from one peer: a quarter, so one peer cannot use up the node's allowance. */
  val MaxUnverifiedPerPeer: Int = 256

  /** Their total encoded size: the disk an attacker can make the node hold until finality passes. */
  val MaxUnverifiedBytes: Long = 1L << 30

  /** Slots a side-branch block may be ahead of the wall clock (clock skew between author and this node). */
  val FutureSlotTolerance: Long = 1L

/** Owns the canonical chain: the persistent state trie, block storage and the
  * best/finalized heads, and drives [[BlockImporter]] for every new block.
  *
  * Forks: blocks with a known non-best parent are stored unexecuted on a
  * side branch, after the header checks that need no execution (see
  * "Side-branch admission"); when a branch becomes strictly heavier than the best chain
  * and contains no equivocation, it is adopted by rewinding to the common
  * ancestor and replaying.
  */
final class ChainManager(
    val config: ChainConfig,
    trieBackend: RocksDbTrieBackend,
    val blockStore: BlockStore,
    maxUnverifiedBlocks: Int = ChainManager.MaxUnverifiedBlocks,
    maxUnverifiedPerPeer: Int = ChainManager.MaxUnverifiedPerPeer,
    maxUnverifiedBytes: Long = ChainManager.MaxUnverifiedBytes,
    /** Delete finalized history more than `maxLookupAnchorAge` slots behind the finalized block (see "Pruning of
      * finalized history").
      */
    pruneFinalizedHistory: Boolean = false,
    /** Delete the state of blocks more than `maxLookupAnchorAge` slots behind the finalized block (they are never
      * read again).
      */
    pruneOldState: Boolean = true
) extends LazyLogging:

  require(
    !trieBackend.tracksGarbage,
    "ChainManager never runs trie gc: open its RocksDbTrieBackend with trackGarbage = false"
  )

  private val trieStore = new StateTrieStore(trieBackend)
  private val importer =
    new BlockImporter(
      config,
      skipAncestryValidation = false,
      externalTrieStore = Some(trieStore),
      gcAfterImport = false
    )
  private val blockCodec = Block.blockCodec(config)

  // Stored headers by hash for the ancestor walk; keeps the last L slots (plus an epoch of slack for empty slots, so
  // the header that ends a walk usually stays in memory too).
  private[node] val headerLinks =
    new HeaderLinkCache(config.maxLookupAnchorAge + config.epochLength, h => blockStore.getHeader(h))

  private def storeBlock(hash: Hash, parent: Hash, headerBytes: Array[Byte], blockBytes: Array[Byte]): Unit =
    blockStore.putBlock(hash, parent, headerBytes, blockBytes)
    headerLinks.record(hash, headerBytes)

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
  def finalize(hash: Hash, belowCurrent: Boolean = false): Either[ChainError, Head] =
    synchronized {
      blockRoot(hash) match
        case None => Left(ChainError.NotOnMainChain)
        case Some(root) =>
          val slot = blockStore
            .getBlock(hash)
            .filter(_.nonEmpty)
            .flatMap(b => decodeBlock(b).toOption)
            .map(_.header.slot.toLong)
            .getOrElse(0L)
          val current = finalized
          if !belowCurrent && metaLong(s"height:${hash.toHex}").getOrElse(0L) <
              metaLong(s"height:${current.hash.toHex}").getOrElse(0L)
          then Left(ChainError.BelowFinalized)
          else
            blockStore.setHead(BlockStore.FinalizedHead, hash)
            putMetaLong("finalized_slot", slot)
            blockStore.setMeta("finalized_root", root.bytes.toArray)
            val finalizedHeight = metaLong(s"height:${hash.toHex}").getOrElse(0L)
            val it = leafMap.entrySet().iterator()
            while it.hasNext do
              val e = it.next()
              if blockHeight(e.getKey).getOrElse(0L) <= finalizedHeight then it.remove()
            pruneUnverified(slot, finalizedHeight)
            indexFinalized(hash, finalizedHeight)
            pruneHistory(hash, slot, finalizedHeight)
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

  /** Follow the finality that `needed` distinct peers announce on UP 0 (their `Final`): finalize the newest such
    * block, first reorging to the heaviest leaf that contains it when it is off our best chain. This keeps a node
    * that runs no GRANDPA voter (GP 0.8.0 §grandpa) on the chain the voters finalized, even below our own devnet
    * finality, which is only a local guess. Returns the block to fetch when the agreed block is not stored yet.
    */
  def followPeerFinality(peerFinals: Iterable[(Hash, Long)], needed: Int): Either[Hash, Option[Head]] =
    val events = mutable.ListBuffer.empty[(Head, Block)]
    val result = synchronized {
      val finals = peerFinals.toList
      val current = finalized
      def supports(f: Hash, x: Hash, xHeight: Long): Boolean = f == x || ancestorAtHeight(f, xHeight).contains(x)
      val agreed = finals
        .filter(_._2 > current.slot)
        .distinctBy(_._1)
        .sortBy(-_._2)
        .find { case (x, _) =>
          blockHeight(x) match
            case Some(h) => finals.count(f => supports(f._1, x, h)) >= needed
            case None    => finals.count(_._1 == x) >= needed
        }
      agreed match
        case None => Right(None)
        case Some((x, _)) if !blockStore.hasBlock(x) || blockHeight(x).isEmpty => Left(x)
        case Some((x, _)) =>
          val r = adoptAndFinalize(x, events, belowCurrent = true)
          r.left.foreach(e => logger.warn(s"cannot follow peer finality of ${x.toHex.take(18)}: $e"))
          Right(r.toOption)
    }
    events.foreach((head, block) => postImportBus.publish(head, block))
    result

  /** Finalize stored block `x` (GRANDPA), first reorging to the heaviest leaf containing it when it is off our best
    * chain.
    */
  def finalizeAdopting(x: Hash): Either[ChainError, Head] =
    val events = mutable.ListBuffer.empty[(Head, Block)]
    val result = synchronized(adoptAndFinalize(x, events, belowCurrent = false))
    events.foreach((head, block) => postImportBus.publish(head, block))
    result

  private def adoptAndFinalize(
      x: Hash,
      events: mutable.ListBuffer[(Head, Block)],
      belowCurrent: Boolean
  ): Either[ChainError, Head] =
    blockHeight(x) match
      case None => Left(ChainError.UnknownBlock)
      case Some(xHeight) =>
        val onBest = blockRoot(x).isDefined && ancestorAtHeight(bestHead.hash, xHeight).contains(x)
        val adopted =
          if onBest then Right(bestHead)
          else
            val tips = (x :: leafMap.keySet.asScala.toList).filter(l => ancestorAtHeight(l, xHeight).contains(x))
            val tip = tips.maxBy(l => (weightOf(l), blockHeight(l).getOrElse(0L)))
            if blockRoot(tip).isDefined then Right(bestHead)
            else
              logger.warn(s"adopting ${tip.toHex.take(18)}, which contains finalized ${x.toHex.take(18)}")
              reorgTo(tip, events, ignoreFinality = true)
        adopted.flatMap(_ =>
          if blockRoot(x).isDefined then finalize(x, belowCurrent) else Left(ChainError.NotValidated)
        )

  /** Parent of a stored block. */
  def parentHash(h: Hash): Option[Hash] = parentOf(h)

  /** Slot of a stored block, from its header. */
  def slotOf(h: Hash): Option[Long] =
    blockStore.getHeader(h).filter(_.length >= 100).map(b => io.forge.jam.core.LittleEndian.get(b, 96, 4))

  /** Posterior state root of a validated block. */
  def postStateRoot(h: Hash): Option[Hash] = blockRoot(h)

  /** Encoded header of a stored block. */
  def headerBytes(h: Hash): Option[Array[Byte]] = blockStore.getHeader(h)

  /** Decoded header of a stored block. */
  def header(h: Hash): Option[io.forge.jam.core.types.header.Header] =
    blockStore.getHeader(h).flatMap(b => headerCodec.decodeExact(b).toOption)

  val headerCodec = io.forge.jam.core.types.header.Header.headerCodec(config.epochLength)

  def meta(name: String): Option[Array[Byte]] = blockStore.getMeta(name)
  def putMeta(name: String, value: Array[Byte]): Unit = blockStore.setMeta(name, value)

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
    val slot = block.header.slot.toLong
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
      .map(_.header.slot.toLong)
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
    // From the oldest block kept: what was below it has been pruned, child lists included.
    val floor = pruneFloor
    val stack = mutable.ArrayDeque(if floor == Hash.zero then genesisHash else floor)
    val visited = mutable.HashSet.empty[Hash]
    while stack.nonEmpty do
      val h = stack.removeHead()
      if !visited.contains(h) then
        visited += h
        // Blocks stored unverified by the previous run count against the caps again (their peers are not known).
        metaLong(s"unverified:${h.toHex}").foreach { size =>
          if blockRoot(h).isEmpty then
            parentOf(h).foreach(p => trackUnverified(h, slotOfHeader(h), p, size, None))
        }
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
        headerLinks.advance(bestHead.slot)
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
        val genesisHash = spec.genesisHeaderHash
        // Genesis's nodes are added at height 0, so a later block's replacements of them can be pruned.
        val root = trieBackend.withJournal(genesisHash, 0L) {
          trieStore.bootstrap(spec.genesisState.map(kv => (kv.key, kv.value)))
        }
        spec.genesisHeaderBytes.foreach { hb =>
          storeBlock(genesisHash, Hash.zero, hb, Array.emptyByteArray)
        }
        headerLinks.advance(0L)
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
    Hashing.blake2b256(headerCodec.encode(block.header))

  // ---------------------------------------------------------------------------------------------------------------
  // Side-branch admission. A block that does not extend the best head is stored without being executed, so before
  // it is stored it must pass every check that needs no execution:
  //   - its slot is after its parent's and, when a wall clock is set, not ahead of it by more than
  //     `FutureSlotTolerance`;
  //   - its extrinsic hash, seal and entropy VRF signature verify (BlockPipeline.verifyHeader, the import pipeline's
  //     own steps) against the posterior state of its nearest validated ancestor A. That state is the parent's when
  //     A is the parent, and then the epoch and winning-tickets markers are checked as well. When A is further back
  //     it still fixes the author key, the seal entropy and the sealing sequence of the block's slot in two cases:
  //     the block is in A's epoch (none of them changes within an epoch), or it is in the next epoch and A is at or
  //     past the ticket submission cutoff (the next epoch's validator keys and entropy are already fixed, and the
  //     ticket accumulator no longer changes after the cutoff, so the ticket or fallback sequence is fixed too).
  // Beyond that horizon the sealing keys depend on blocks of the branch that were never executed (tickets
  // accumulated before the cutoff, or a whole epoch of entropy), so the seal cannot be checked. Such a block is stored
  // "unverified" (only its slot order and clock bound were checked) under caps: `maxUnverifiedBlocks` and
  // `maxUnverifiedBytes` in all, `maxUnverifiedPerPeer` per sending peer. Unverified blocks leave the caps when a
  // reorg validates them, and are deleted once finality reaches their slot, with their descendants.
  // ---------------------------------------------------------------------------------------------------------------

  private final case class Unverified(slot: Long, parent: Hash, size: Long, peer: Option[Ed25519PublicKey])

  // Guarded by `this`, like every import.
  private val unverified = mutable.LinkedHashMap.empty[Hash, Unverified]
  private val unverifiedPerPeer = mutable.HashMap.empty[Ed25519PublicKey, Int]
  private var unverifiedBytes = 0L

  @volatile private var wallClockSlot: Option[() => Long] = None

  /** Refuse side-branch blocks whose slot is more than [[ChainManager.FutureSlotTolerance]] ahead of `now()`. */
  def useWallClock(now: () => Long): Unit = wallClockSlot = Some(now)

  /** Side-branch blocks stored without a seal check. */
  def unverifiedCount: Int = synchronized(unverified.size)

  /** Right(true) when the block may be stored but its seal could not be checked, Right(false) when it verified. */
  private def admitSideBlock(
      block: Block,
      size: Int,
      source: Option[Ed25519PublicKey]
  ): Either[ChainError, Boolean] =
    val parent = block.header.parent
    val slot = block.header.slot.toLong
    val parentSlot = slotOfHeader(parent)
    if slot <= parentSlot then
      Left(ChainError.SideBranchRefused(s"slot $slot is not after its parent's slot $parentSlot"))
    else if wallClockSlot.exists(now => slot > now() + ChainManager.FutureSlotTolerance) then
      Left(ChainError.SideBranchRefused(s"slot $slot is in the future"))
    else
      // Nearest validated ancestor: the first block back from the parent that is on the best chain.
      var ancestor = parent
      var root = blockRoot(ancestor)
      while root.isEmpty && ancestor != Hash.zero do
        ancestor = parentOf(ancestor).getOrElse(Hash.zero)
        root = blockRoot(ancestor)
      root match
        case None => Left(ChainError.SideBranchRefused("its branch does not reach a validated block"))
        case Some(ancestorRoot) =>
          val ancestorSlot = slotOfHeader(ancestor)
          val epochs = slot / config.epochLength - ancestorSlot / config.epochLength
          val sealKeysKnown =
            ancestor == parent || epochs == 0 ||
              (epochs == 1 && ancestorSlot % config.epochLength >= config.ticketCutoff)
          if sealKeysKnown then
            val trie = StateTrie.at(trieStore.backend, ancestorRoot)
            val view = new TrieBackedJamState(trie, config, new ServiceStorageView(trie), None)
            val checked =
              try BlockPipeline.verifyHeader(block, view, parentState = ancestor == parent).left.map(_.toString)
              catch case NonFatal(e) => Left(s"header check failed: ${e.getMessage}")
            checked.left.map(ChainError.SideBranchRefused(_)).map(_ => false)
          else if unverified.size >= maxUnverifiedBlocks || unverifiedBytes + size > maxUnverifiedBytes then
            Left(ChainError.UnverifiedLimit("node"))
          else if source.exists(p => unverifiedPerPeer.getOrElse(p, 0) >= maxUnverifiedPerPeer) then
            Left(ChainError.UnverifiedLimit("peer"))
          else Right(true)

  private def trackUnverified(hash: Hash, slot: Long, parent: Hash, size: Long, peer: Option[Ed25519PublicKey]): Unit =
    if !unverified.contains(hash) then
      unverified(hash) = Unverified(slot, parent, size, peer)
      unverifiedBytes += size
      peer.foreach(p => unverifiedPerPeer(p) = unverifiedPerPeer.getOrElse(p, 0) + 1)
      // Persisted (with the size) so a restart can count and prune it again.
      putMetaLong(s"unverified:${hash.toHex}", size)

  private def untrackUnverified(hash: Hash): Unit =
    unverified.remove(hash).foreach { u =>
      unverifiedBytes -= u.size
      u.peer.foreach { p =>
        val left = unverifiedPerPeer.getOrElse(p, 1) - 1
        if left <= 0 then unverifiedPerPeer.remove(p) else unverifiedPerPeer(p) = left
      }
      blockStore.deleteMeta(s"unverified:${hash.toHex}")
    }

  /** Delete the unverified blocks at or below the finalized slot, and the unverified blocks built on them: none is
    * on the finalized block's chain (its ancestors are validated), so none can be adopted any more.
    */
  private def pruneUnverified(finalizedSlot: Long, finalizedHeight: Long): Unit =
    if unverified.nonEmpty then
      val doomed = mutable.LinkedHashSet.empty[Hash]
      // Insertion order has parents before children, so one pass closes over descendants.
      unverified.foreach { (h, u) =>
        if u.slot <= finalizedSlot || doomed.contains(u.parent) then doomed += h
      }
      doomed.foreach { h =>
        val u = unverified(h)
        untrackUnverified(h)
        if blockRoot(h).isEmpty then
          header(h).foreach { hd =>
            val key = s"slotauthor:${hd.slot.toLong}:${hd.authorIndex.toInt}"
            if blockStore.getMeta(key).exists(java.util.Arrays.equals(_, h.bytes.toArray)) then
              blockStore.deleteMeta(key)
          }
          blockStore.deleteBlock(h, u.parent)
          headerLinks.invalidate(h)
          leafMap.remove(h)
          List("height", "ticketed", "marked", "weight").foreach(k => blockStore.deleteMeta(s"$k:${h.toHex}"))
          // Its parent is a leaf again when nothing else builds on it, as a restart's rebuild would find.
          if !doomed.contains(u.parent) && blockStore.hasBlock(u.parent) && blockStore.children(u.parent).isEmpty &&
            blockHeight(u.parent).getOrElse(0L) > finalizedHeight
          then leafMap.put(u.parent, headOf(u.parent))
      }
      if doomed.nonEmpty then
        logger.info(s"pruned ${doomed.size} unverified side-branch block(s) at or below finalized slot $finalizedSlot")

  // ---------------------------------------------------------------------------------------------------------
  // Pruning of finalized history
  //
  // A node needs the headers of the last `maxLookupAnchorAge` slots (the ancestor set a lookup anchor is checked
  // against) and nothing else of the finalized chain. Everything of a block is therefore deleted once it is more
  // than `maxLookupAnchorAge` slots behind the finalized block: body, header, fork-choice and equivocation
  // metadata, its finality proof (warp-sync fragments are kept apart, per voter set), and every side branch that
  // forks off it. The oldest block kept (the "floor") anchors the walk and the leaf rebuild after a restart;
  // genesis is always kept.
  // ---------------------------------------------------------------------------------------------------------

  private def finalizedIndexKey(height: Long): String = f"fin:$height%016x"

  /** The oldest main-chain block whose data is kept; genesis until anything has been pruned. */
  private def pruneFloor: Hash =
    blockStore.getMeta("prune_floor").filter(_.length == 32).map(Hash(_))
      .orElse(blockStore.getHead(BlockStore.GenesisHead))
      .getOrElse(Hash.zero)

  private[node] def pruneFloorHeight: Long = blockHeight(pruneFloor).getOrElse(0L)

  /** State trie nodes and raw values on disk (a full scan; for tests and diagnostics). */
  private[node] def storedStateCounts: (Long, Long) = trieBackend.storedCounts

  /** Record the finalized chain by height, from `hash` down to the first block already recorded, so pruning can
    * walk it upward from the floor. Entries above `height` belong to a finalized head this one replaced (finality
    * followed from peers may move it back) and are dropped.
    */
  private def indexFinalized(hash: Hash, height: Long): Unit =
    var stale = height + 1
    while blockStore.getMeta(finalizedIndexKey(stale)).isDefined do
      blockStore.deleteMeta(finalizedIndexKey(stale))
      stale += 1
    val floorHeight = pruneFloorHeight
    var cursor: Option[Hash] = Some(hash)
    var h = height
    var recorded = false
    while !recorded && cursor.isDefined && h >= floorHeight do
      val c = cursor.get
      if blockStore.getMeta(finalizedIndexKey(h)).exists(java.util.Arrays.equals(_, c.bytes.toArray)) then
        recorded = true
      else
        blockStore.setMeta(finalizedIndexKey(h), c.bytes.toArray)
        cursor = parentOf(c)
        h -= 1

  /** Delete what lies more than `maxLookupAnchorAge` slots behind the finalized block, at most
    * `MaxPrunedPerFinalization` main-chain blocks at a time so a finalization never stalls on a long backlog (the
    * rest goes at the next ones).
    */
  private def pruneHistory(finalizedHash: Hash, finalizedSlot: Long, finalizedHeight: Long): Unit =
    val cutoff = finalizedSlot - config.maxLookupAnchorAge
    if (pruneFinalizedHistory || pruneOldState) && cutoff > 0 then
      var floor = pruneFloor
      var floorHeight = blockHeight(floor).getOrElse(0L)
      val genesis = blockStore.getHead(BlockStore.GenesisHead)
      var pruned = 0
      var forks = 0
      var stateNodes = 0
      var stateValues = 0
      var more = true
      while more && pruned < ChainManager.MaxPrunedPerFinalization do
        // The floor goes once the block after it is itself outside the window; the finalized block always stays.
        val next = blockStore.getMeta(finalizedIndexKey(floorHeight + 1)).filter(_.length == 32).map(Hash(_))
        next match
          case Some(n) if floorHeight + 1 < finalizedHeight && n != finalizedHash &&
                blockStore.getHeader(n).isDefined && slotOfHeader(n) < cutoff =>
            // Side branches off the old floor fork outside the lookup-anchor window: none can be adopted.
            // Blocks go only when asked (pruneFinalizedHistory); the state before the new floor goes either way,
            // so a side branch off the old floor is unusable from here on and its journal is dropped.
            val branches = blockStore.children(floor).filterNot(_ == n)
            if pruneFinalizedHistory then branches.foreach(root => forks += deleteBranch(root))
            else branches.foreach(dropBranchJournals)
            blockStore.setMeta("prune_floor", n.bytes.toArray)
            if pruneFinalizedHistory then
              if !genesis.contains(floor) then deleteBlockRecord(floor, finalizedChain = true)
              blockStore.deleteMeta(finalizedIndexKey(floorHeight))
            // The old floor's state goes: every state still kept descends from `n`, so the nodes `n` replaced
            // are unreachable unless a later block brought them back.
            if pruneOldState then
              val (nodes, values) = trieBackend.pruneJournal(n, floorHeight + 1)
              stateNodes += nodes
              stateValues += values
            else trieBackend.dropJournal(n)
            floor = n
            floorHeight += 1
            pruned += 1
          case _ => more = false
      if pruned > 0 then
        logger.info(
          s"pruned ${if pruneFinalizedHistory then pruned else 0} finalized block(s), $forks side-branch block(s), " +
            s"$stateNodes state node(s) and $stateValues value(s) of $pruned state(s) more than ${config.maxLookupAnchorAge} slots behind finalized slot " +
            s"$finalizedSlot; oldest kept at height $floorHeight"
        )

  /** Drop the state journals of `root` and everything built on it, keeping the blocks. */
  private def dropBranchJournals(root: Hash): Unit =
    val stack = mutable.ArrayDeque(root)
    while stack.nonEmpty do
      val h = stack.removeHead()
      trieBackend.dropJournal(h)
      blockStore.children(h).foreach(stack.append)

  /** Delete the block `root` and everything built on it; returns how many blocks went. */
  private def deleteBranch(root: Hash): Int =
    val stack = mutable.ArrayDeque(root)
    val order = mutable.ListBuffer.empty[Hash]
    while stack.nonEmpty do
      val h = stack.removeHead()
      order += h
      blockStore.children(h).foreach(stack.append)
    // Children before parents, so each delete finds its parent's child list.
    order.reverseIterator.foreach(deleteBlockRecord(_, finalizedChain = false))
    order.size

  /** Delete everything stored under one block. The equivocation mark of its slot goes only with the finalized
    * chain's block: a side-branch block may sit at a slot that is not finalized yet, where the mark still counts.
    */
  private def deleteBlockRecord(h: Hash, finalizedChain: Boolean): Unit =
    val parent = parentOf(h).getOrElse(Hash.zero)
    untrackUnverified(h)
    header(h).foreach { hd =>
      val slot = hd.slot.toLong
      val authorKey = s"slotauthor:$slot:${hd.authorIndex.toInt}"
      if blockStore.getMeta(authorKey).exists(java.util.Arrays.equals(_, h.bytes.toArray)) then
        blockStore.deleteMeta(authorKey)
      if finalizedChain then blockStore.deleteMeta(s"equivocated:$slot")
    }
    blockStore.deleteBlock(h, parent)
    // A side branch's journal is never acted on: it describes what the branch replaced, not the finalized chain.
    if !finalizedChain then trieBackend.dropJournal(h)
    headerLinks.invalidate(h)
    leafMap.remove(h)
    List("height", "ticketed", "marked", "weight", "blockroot", "unverified", "invalid", "grandpa:just").foreach(k =>
      blockStore.deleteMeta(s"$k:${h.toHex}")
    )

  /** Import a block: extends the best head directly, or is stored as a side
    * branch and adopted via reorg when its branch becomes strictly heavier
    * (ticket-sealed count, then height) and holds no equivocation. A side-branch
    * block is stored only after the checks described above; `source` is the peer
    * it came from, for the per-peer cap on blocks whose seal cannot be checked.
    */
  def importBlock(blockBytes: Array[Byte], source: Option[Ed25519PublicKey] = None): Either[ChainError, Head] =
    val events = mutable.ListBuffer.empty[(Head, Block)]
    val result = synchronized {
      decodeBlock(blockBytes).left.map(ChainError.Undecodable(_)).flatMap { block =>
        val hash = headerHashOf(block)
        val parent = block.header.parent
        if blockRoot(hash).isDefined then Left(ChainError.AlreadyImported)
        else if flag(s"invalid:${hash.toHex}") then Left(ChainError.PreviouslyInvalid)
        else if parent == bestHead.hash then importOnBest(block, blockBytes, events)
        else
          (blockHeight(parent), blockStore.hasBlock(parent)) match
            case (Some(parentHeight), true) =>
              // Known parent on some branch: check what can be checked without executing it, store and evaluate.
              // A block already stored on a side branch was admitted when it was stored.
              val stored = blockHeight(hash).isDefined && blockStore.hasBlock(hash)
              val admitted =
                if stored then Right(false) else admitSideBlock(block, blockBytes.length, source)
              admitted.flatMap { unverifiable =>
                storeBlock(hash, parent, headerCodec.encode(block.header), blockBytes)
                if unverifiable then trackUnverified(hash, block.header.slot.toLong, parent, blockBytes.length, source)
                val height = parentHeight + 1
                putBlockHeight(hash, height)
                recordForkChoiceMeta(hash, block)
                leafMap.remove(parent)
                if blockStore.children(hash).isEmpty then
                  leafMap.put(hash, Head(hash, block.header.slot.toLong, Hash.zero))
                else leafMap.remove(hash)
                val bestHeight = blockHeight(bestHead.hash).getOrElse(0L)
                val (weight, bestWeight) = (weightOf(hash), weightOf(bestHead.hash))
                val heavier = weight > bestWeight || (weight == bestWeight && height > bestHeight)
                if heavier && !branchEquivocates(hash) && !branchInvalid(hash) then reorgTo(hash, events)
                else
                  Left(ChainError.StoredOnSideBranch(weight, height, bestWeight, bestHeight))
              }
            case _ =>
              Left(ChainError.UnknownParent(parent))
      }
    }
    events.foreach((head, block) => postImportBus.publish(head, block))
    result

  private def importOnBest(
      block: Block,
      blockBytes: Array[Byte],
      events: mutable.ListBuffer[(Head, Block)]
  ): Either[ChainError, Head] =
    val parent = block.header.parent
    val preState = RawState(bestHead.stateRoot, Nil)
    val ancestry = ancestorsOf(parent, block.header.parentStateRoot, block.header.slot.toLong)
    // The save belongs to this block at this height: what it replaces can be deleted once the parent's state is no
    // longer kept (see pruneHistory).
    val journalHeight = blockHeight(parent).getOrElse(0L) + 1
    trieBackend.withJournal(headerHashOf(block), journalHeight)(importer.importBlock(block, preState, ancestry)) match
      case ImportResult.Failure(error, message, _) =>
        markInvalid(headerHashOf(block))
        Left(ChainError.ImportFailed(error, message))
      case ImportResult.Success(postRoot, _) =>
        val hash = headerHashOf(block)
        val slot = block.header.slot.toLong
        storeBlock(hash, parent, headerCodec.encode(block.header), blockBytes)
        if unverified.nonEmpty then untrackUnverified(hash) // validated now (a reorg replayed it)
        headerLinks.advance(slot)
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

  /** Ancestor set A for a block on `parent` at `slot`: stored headers back to slot - L (GP 0.8.0 §header
    * eq:ancestors), each with its posterior state root read from its child's prior state root. Follows `parent`'s
    * own chain (not the best chain); headers come from [[headerLinks]], which falls back to the block store.
    */
  private[node] def ancestorsOf(parent: Hash, parentPostRoot: Hash, slot: Long): List[AncestorHeader] =
    val out = List.newBuilder[AncestorHeader]
    var cursor = parent
    var postRoot = parentPostRoot
    var more = true
    while more do
      headerLinks.get(cursor) match
        case Some(link) =>
          if slot - link.slot > config.maxLookupAnchorAge then more = false
          else
            out += AncestorHeader(link.slot, cursor, Some(postRoot))
            postRoot = link.priorStateRoot
            if link.parent == Hash.zero then more = false else cursor = link.parent
        case None => more = false
    out.result()

  // A block that failed validation, and so every block built on it, is never adopted.
  private def markInvalid(h: Hash): Unit = putMetaLong(s"invalid:${h.toHex}", 1L)

  private def branchInvalid(tip: Hash): Boolean =
    var cursor = tip
    while blockRoot(cursor).isEmpty do
      if flag(s"invalid:${cursor.toHex}") then return true
      parentOf(cursor) match
        case Some(p) => cursor = p
        case None    => return false
    false

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
      events: mutable.ListBuffer[(Head, Block)],
      ignoreFinality: Boolean = false
  ): Either[ChainError, Head] =
    // Collect the unvalidated branch (tip backwards until a validated block).
    val branch = mutable.ListBuffer.empty[Array[Byte]]
    var cursor = tip
    while blockRoot(cursor).isEmpty do
      blockStore.getBlock(cursor) match
        case Some(bytes) if bytes.nonEmpty =>
          branch.prepend(bytes)
          decodeBlock(bytes) match
            case Right(b) => cursor = b.header.parent
            case Left(e)  => return Left(ChainError.Undecodable(s"reorg: undecodable branch block: $e"))
        case _ => return Left(ChainError.BranchBlockMissing)
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
    if blockHeight(ancestor).getOrElse(0L) < finalizedHeight && !ignoreFinality then
      return Left(ChainError.RevertsFinalized)

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
        case Left(e)  => return Left(ChainError.Undecodable(s"reorg: undecodable main block: $e"))

    logger.info(
      s"reorg: rewinding ${abandoned.size} block(s) to ${ancestor.toHex.take(18)}, " +
        s"replaying ${branch.size}"
    )
    trieStore.markCommitted(ancestorRoot)
    val ancestorBlock = blockStore.getBlock(ancestor)
    val ancestorSlot =
      ancestorBlock.filter(_.nonEmpty).flatMap(b => decodeBlock(b).toOption)
        .map(_.header.slot.toLong)
        .getOrElse(0L)
    bestHead = Head(ancestor, ancestorSlot, ancestorRoot)

    val replayed = mutable.ListBuffer.empty[Hash]
    boundary:
      branch.foreach { bytes =>
        decodeBlock(bytes).left.map(ChainError.Undecodable(_)).flatMap(b => importOnBest(b, bytes, events)) match
          case Right(head) =>
            replayed += head.hash
          case Left(err) =>
            replayed.foreach(dropBlockRoot)
            trieStore.markCommitted(previousBest.stateRoot)
            bestHead = previousBest
            blockStore.setHead(BlockStore.BestHead, previousBest.hash)
            blockStore.setMeta("state_root", previousBest.stateRoot.bytes.toArray)
            putMetaLong("best_slot", previousBest.slot)
            break(Left(ChainError.ReplayFailed(err)))
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

  /** The stored state with root `root`, if its trie nodes are still held (the empty state always is). */
  def stateAtRoot(root: Hash): Option[StateTrie] =
    if root == Hash.zero || trieStore.backend.readNode(root).isDefined then Some(StateTrie.at(trieStore.backend, root))
    else None

  def stateAt(headerHash: Hash): Option[StateTrie] =
    // Below the pruning floor the state has been deleted (in part or whole), whether or not the block is kept.
    if pruneOldState && blockHeight(headerHash).exists(_ < pruneFloorHeight) then None
    else blockRoot(headerHash).map(root => StateTrie.at(trieStore.backend, root))

  /** A read view over the posterior state of stored block `headerHash`. */
  def stateViewAt(headerHash: Hash): Option[TrieBackedJamState] =
    stateAt(headerHash).map(trie => new TrieBackedJamState(trie, config, new ServiceStorageView(trie), None))

  /** Disregard block `hash` and every descendant (a report judged invalid by a third of the
    * validators). When it is on the best chain above finality, the best head rewinds to its parent and the heaviest
    * remaining branch is adopted.
    */
  def disregard(hash: Hash): Either[ChainError, Head] =
    val events = mutable.ListBuffer.empty[(Head, Block)]
    val result = synchronized {
      markInvalid(hash)
      val finalizedHeight = metaLong(s"height:${finalized.hash.toHex}").getOrElse(0L)
      (blockRoot(hash), parentOf(hash)) match
        case (None, _) => Left(ChainError.NotOnBestChain)
        case (_, None) => Left(ChainError.NoParent)
        case (Some(_), Some(parent)) if blockHeight(hash).getOrElse(0L) <= finalizedHeight =>
          Left(ChainError.AlreadyFinalized(parent))
        case (Some(_), Some(parent)) =>
          rewindTo(parent)
          val (pw, ph) = (weightOf(parent), blockHeight(parent).getOrElse(0L))
          val candidates = leafMap.keySet.asScala.toList.filter(l =>
            l != parent && !branchInvalid(l) && !branchEquivocates(l) && blockRoot(l).isEmpty &&
              (weightOf(l) > pw || (weightOf(l) == pw && blockHeight(l).getOrElse(0L) > ph))
          )
          candidates.sortBy(l => (-weightOf(l), -blockHeight(l).getOrElse(0L))).iterator
            .map(l => reorgTo(l, events))
            .collectFirst { case Right(h) => h }
            .map(Right(_))
            .getOrElse(Right(bestHead))
    }
    events.foreach((head, block) => postImportBus.publish(head, block))
    result

  /** Move the best head back to its ancestor `ancestor`; the blocks above it lose main-chain status. */
  private def rewindTo(ancestor: Hash): Unit =
    var back = bestHead.hash
    while back != ancestor && back != Hash.zero do
      val parent = parentOf(back).getOrElse(Hash.zero)
      dropBlockRoot(back)
      Option(leafMap.get(back)).foreach(h => leafMap.put(back, h.copy(stateRoot = Hash.zero)))
      back = parent
    val root = blockRoot(ancestor).getOrElse(Hash.zero)
    trieStore.markCommitted(root)
    bestHead = Head(ancestor, blockSlot(ancestor), root)
    blockStore.setHead(BlockStore.BestHead, ancestor)
    blockStore.setMeta("state_root", root.bytes.toArray)
    putMetaLong("best_slot", bestHead.slot)
    if blockStore.children(ancestor).isEmpty then leafMap.put(ancestor, bestHead)
    logger.warn(s"best head rewound to ${ancestor.toHex.take(18)} slot=${bestHead.slot}")

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
