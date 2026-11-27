package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}
import io.forge.jam.core.trie.{BufferedTrieBackend, MissingTrieNodeException, StateTrie, StateTrieStore}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.header.Header
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend}
import io.forge.jam.protocol.pipeline.{BlockPipeline, HeaderChecks, PipelineError}
import io.forge.jam.protocol.report.ReportTypes.{AncestorHeader, Ancestry, ReportErrorCode}
import io.forge.jam.protocol.state.{ServiceStorageView, TrieBackedJamState}
import io.forge.jam.protocol.importing.{BlockImporter, ImportResult}
import io.forge.jam.protocol.state.RawState

import scala.collection.mutable
import scala.util.boundary, boundary.break
import scala.util.control.NonFatal
import scala.jdk.CollectionConverters.*

object ChainManager:
  val KeptAccumulateCosts: Int = 64

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

  /** Blocks of one slot executed off the best chain as they arrive; a further one is stored unexecuted. */
  val MaxExecutedPerSlot: Int = 2

  val StateSweepIntervalMillis: Long = 60L * 60 * 1000

  val BatchStateDepthProperty = "jam.sync.batchStateDepth"

  private[node] def batchStateDepth(config: ChainConfig, pruneOldState: Boolean, setting: Option[String]): Option[Long] =
    if !pruneOldState then None
    else
      setting match
        case None    => Some(config.maxLookupAnchorAge + config.epochLength)
        case Some(s) => s.trim.toLongOption.filter(_ >= 0)

/** Owns the canonical chain: the persistent state trie, block storage and the
  * best/finalized heads, and drives [[BlockImporter]] for every new block.
  *
  * Forks: a block with a known non-best parent is on a side branch. It is executed as it arrives when its parent
  * was (see "Side-branch execution"), and otherwise stored unexecuted after the header checks that need no
  * execution (see "Side-branch admission"). When a branch is preferred to the best chain (more ticket-sealed
  * blocks, or as many and a head at a later slot: [[ForkChoice.preferred]]) and contains no equivocation, the best
  * head moves to it, executing whichever of its blocks were not executed yet.
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
    pruneOldState: Boolean = true,
    stateSweepIntervalMillis: Long = ChainManager.StateSweepIntervalMillis
) extends ChainReader
    with LazyLogging:

  require(
    !trieBackend.tracksGarbage,
    "ChainManager never runs trie gc: open its RocksDbTrieBackend with trackGarbage = false"
  )

  private val stateBuffer = new BufferedTrieBackend(trieBackend)
  private var bufferedTip: (Hash, Long) | Null = null
  private val trieStore = new StateTrieStore(stateBuffer)
  private val importer =
    new BlockImporter(
      config,
      skipAncestryValidation = false,
      externalTrieStore = Some(trieStore),
      gcAfterImport = false
    )
  private val blockCodec = Block.blockCodec(config)

  val meta = new ChainMeta(blockStore)
  private val headers = new StoredHeaders(blockStore)
  private val forkChoice = new ForkChoice(config, meta, headers)

  // Guarded by `this`, like every import.
  private val unverified =
    new UnverifiedBlocks(meta, maxUnverifiedBlocks, maxUnverifiedPerPeer, maxUnverifiedBytes)

  // Blocks executed off the best chain whose state is kept (meta.sideRoot), with their heights. Guarded by `this`.
  private val sideStates = mutable.LinkedHashMap.empty[Hash, Long]

  // Stored headers by hash for the ancestor walk; keeps the last L slots (plus an epoch of slack for empty slots, so
  // the header that ends a walk usually stays in memory too).
  private[node] val headerLinks =
    new HeaderLinkCache(config.maxLookupAnchorAge + config.epochLength, h => blockStore.getHeader(h))

  private[node] val ancestorWindow = new AncestorWindow(config.maxLookupAnchorAge, headerLinks.get)

  // The same for the side branch last executed on. Guarded by `this`.
  private val sideWindow = new AncestorWindow(config.maxLookupAnchorAge, headerLinks.get)

  private[node] def ancestryOf(parent: Hash, parentPostRoot: Hash, slot: Long): Ancestry =
    synchronized(ancestorWindow.at(parent, parentPostRoot, slot))

  /** A change to the chain (import, reorg, finality, disregard): serialised, and its metadata (heads, state roots,
    * heights, fork-choice and validity marks) written as one atomic, synced batch after the state trie it points to
    * is durable. A crash anywhere leaves the chain as it was before the change or after it.
    */
  private def commit[A](f: => A): A =
    var finalizedMoved: Option[ChainHead] = None
    val result = synchronized {
      val outermost = commitDepth == 0
      commitDepth += 1
      try
        val before = published._2
        val started = System.nanoTime()
        var bodyNanos = 0L
        var syncNanos = -1L
        val result = blockStore.atomically { () =>
          val syncing = System.nanoTime()
          trieBackend.syncWal()
          syncNanos = System.nanoTime() - syncing
          faultBeforeMetaCommit()
        } {
          try f
          finally bodyNanos = System.nanoTime() - started
        }
        if outermost && syncNanos >= 0 then
          timings.walSync.observeNanos(syncNanos)
          timings.metaWrite.observeNanos(math.max(0L, System.nanoTime() - started - bodyNanos - syncNanos))
        if outermost then
          publishHeads()
          if published._2.hash != before.hash then finalizedMoved = Some(published._2)
          runCommitted()
        result
      catch
        case t: Throwable if outermost =>
          afterCommit.clear()
          recoverFromStore(t)
          publishHeads()
          throw t
      finally commitDepth -= 1
    }
    finalizedMoved.foreach(head => if !Thread.holdsLock(this) then tellFinalized(head))
    result

  val timings = new ChainTimings

  private def publishImported(events: Iterable[(ChainHead, Block)]): Unit =
    val mayWait = !Thread.holdsLock(this)
    events.foreach((head, block) => postImportBus.publish(head, block, mayWait))

  private val finalizedListeners = new java.util.concurrent.CopyOnWriteArrayList[ChainHead => Unit]()

  def onFinalized(listener: ChainHead => Unit): Unit =
    finalizedListeners.add(listener)
    ()

  private def tellFinalized(head: ChainHead): Unit =
    finalizedListeners.forEach { l =>
      try l(head)
      catch case NonFatal(e) => logger.error("finalized-head listener failed", e)
    }

  private var commitDepth = 0

  private val afterCommit = mutable.ArrayBuffer.empty[() => Unit]

  private def deferUntilCommitted(action: () => Unit): Unit =
    synchronized {
      if commitDepth > 0 then afterCommit += action else action()
    }

  private def runCommitted(): Unit =
    val actions = afterCommit.toList
    afterCommit.clear()
    actions.foreach { action =>
      try action()
      catch case NonFatal(e) => logger.warn(s"deleting pruned history failed (left behind): ${e.getMessage}")
    }
    maybeSweepState()

  @volatile private[node] var faultBeforeMetaCommit: () => Unit = () => ()

  @volatile private[node] var faultAfterStateSave: () => Unit = () => ()

  private def storeBlock(hash: Hash, parent: Hash, headerBytes: Array[Byte], blockBytes: Array[Byte]): Unit =
    blockStore.putBlock(hash, parent, headerBytes, blockBytes)
    headerLinks.record(hash, headerBytes)

  @volatile private var bestHead: ChainHead = ChainHead(Hash.zero, 0L, Hash.zero)

  @volatile private var published: (ChainHead, ChainHead) =
    (ChainHead(Hash.zero, 0L, Hash.zero), ChainHead(Hash.zero, 0L, Hash.zero))

  private def publishHeads(): Unit = published = (bestHead, storedFinalized)

  private val leafMap = new java.util.concurrent.ConcurrentHashMap[Hash, ChainHead]()

  private val statePruning = java.util.concurrent.Executors.newSingleThreadExecutor { r =>
    val t = new Thread(r, "jam-state-prune")
    t.setDaemon(true)
    t
  }

  @volatile private var closing = false

  private def pruneStateOf(block: Hash, height: Long): Unit =
    if !closing then
      val floor = pruner.floorHeight
      if floor < height then
        logger.warn(s"state pruning below height $height deferred: the stored prune floor is at height $floor")
      else
        try
          val (nodes, values) = trieBackend.pruneJournal(block, height)
          logger.debug(s"deleted $nodes state node(s) and $values value(s) below height $height")
        catch case e: Exception => logger.warn(s"state pruning below height $height failed: ${e.getMessage}")

  private def resumeStatePruning(): Unit =
    trieBackend.scheduledPrunes.foreach((block, height) => statePruning.execute(() => pruneStateOf(block, height)))

  private[node] def awaitStatePruning(): Unit = statePruning.submit((() => ()): Runnable).get()


  private val downloads = new java.util.concurrent.atomic.AtomicInteger(0)
  private val downloadedRoots = mutable.LinkedHashSet.empty[Hash]
  private val sweepQueued = new java.util.concurrent.atomic.AtomicBoolean(false)
  @volatile private var lastSweepNanos: Long = System.nanoTime()

  @volatile private[node] var lastStateSweep: Option[RocksDbTrieBackend.SweepOutcome] = None

  @volatile private[node] var sweepAfterMark: () => Unit = () => ()

  private def maybeSweepState(now: Boolean = false): Unit =
    if !closing && downloads.get == 0 && meta.sweepWanted.isDefined &&
      (now || System.nanoTime() - lastSweepNanos >= stateSweepIntervalMillis * 1_000_000L) &&
      sweepQueued.compareAndSet(false, true)
    then
      try statePruning.execute(() => sweepState())
      catch case _: java.util.concurrent.RejectedExecutionException => sweepQueued.set(false)

  private[node] def requestStateSweep(): Unit =
    synchronized(meta.requestSweep())
    maybeSweepState()

  private def keptStateRoots(floorHeight: Long): Vector[Hash] =
    val out = Vector.newBuilder[Hash]
    out ++= downloadedRoots
    sideStates.keysIterator.foreach(h => meta.sideRoot(h).filter(stateHeld).foreach(out += _))
    var cursor = bestHead.hash
    var more = cursor != Hash.zero
    while more do
      blockHeight(cursor) match
        case Some(h) if !pruneOldState || h >= floorHeight =>
          blockRoot(cursor).foreach(out += _)
          parentOf(cursor) match
            case Some(p) if p != Hash.zero => cursor = p
            case _                         => more = false
        case _ => more = false
    out.result()

  private def sweepState(): Unit =
    try
      if !closing && downloads.get == 0 then
        val prepared = synchronized {
          meta.sweepWanted match
            case Some(request) if !closing && downloads.get == 0 =>
              val sweep = trieBackend.beginSweep()
              try
                val floor = pruner.floorHeight
                Some((sweep, request, keptStateRoots(floor), floor))
              catch
                case e: Throwable =>
                  sweep.close()
                  throw e
            case _ => None
        }
        prepared.foreach { (sweep, request, roots, floor) =>
          lastSweepNanos = System.nanoTime()
          try
            val outcome = sweep.run(
              roots,
              floorHeight = floor,
              journalWanted = label => blockHeight(label).exists(_ > floor),
              stop = () => closing || downloads.get > 0,
              afterMark = sweepAfterMark
            )
            lastStateSweep = Some(outcome)
            outcome match
              case RocksDbTrieBackend.SweepOutcome.Completed(stats) =>
                trieBackend.syncWal()
                synchronized(meta.clearSweepWanted(request))
                logger.info(s"state swept from ${roots.size} kept root(s) above height $floor: $stats")
              case RocksDbTrieBackend.SweepOutcome.Stopped(stats) =>
                logger.info(s"state sweep stopped, to be taken up again: $stats")
              case RocksDbTrieBackend.SweepOutcome.Incomplete(hash, what, _) =>
                logger.error(
                  s"state sweep deleted nothing: kept state is not whole (trie $what ${hash.toHex} is missing)"
                )
          finally sweep.close()
        }
    catch case NonFatal(e) => logger.warn(s"state sweep failed (nothing unreachable is lost): ${e.getMessage}")
    finally sweepQueued.set(false)

  private val pruner = new HistoryPruner(
    config,
    blockStore,
    trieBackend,
    meta,
    headers,
    pruneFinalizedHistory = pruneFinalizedHistory,
    pruneOldState = pruneOldState,
    pruneState = (block, height) =>
      trieBackend.schedulePrune(block, height)
      if !closing then statePruning.execute(() => pruneStateOf(block, height)),
    header = h => header(h),
    forget = h =>
      unverified.untrack(h)
      sideStates.remove(h)
      headerLinks.invalidate(h)
      leafMap.remove(h): Unit,
    defer = deferUntilCommitted
  )

  /** Listeners invoked after every successful import (assurers, watchers). */
  private val importListeners =
    new java.util.concurrent.CopyOnWriteArrayList[(ChainHead, Block) => Unit]()

  def onImported(listener: (ChainHead, Block) => Unit): Unit =
    importListeners.add(listener)
    ()

  private val sealedSideListeners = new java.util.concurrent.CopyOnWriteArrayList[(Hash, Block) => Unit]()

  def onSealedSideBlock(listener: (Hash, Block) => Unit): Unit =
    sealedSideListeners.add(listener)
    ()

  private val abandonListeners = new java.util.concurrent.CopyOnWriteArrayList[(List[Block], List[Block]) => Unit]()

  /** Called when a reorg leaves best-chain blocks behind, with those blocks (newest first) and the blocks adopted
    * in their place (oldest first), before any import listener sees the adopted ones. It runs inside the chain
    * change, so a listener must be quick and must not call back into the chain.
    */
  def onAbandoned(listener: (List[Block], List[Block]) => Unit): Unit =
    abandonListeners.add(listener)
    ()

  private val postImportBus = new PostImportBus[ChainHead, Block]((head, block) =>
    importListeners.forEach { l =>
      try l(head, block)
      catch case e: Exception => logger.error("import listener failed", e)
    }
  )

  def shutdown(): Boolean =
    try postImportBus.close()
    catch case NonFatal(e) => logger.error("shutdown: the post-import bus failed to close", e)
    synchronized { closing = true }
    statePruning.shutdown()
    val stopped = statePruning.awaitTermination(30, java.util.concurrent.TimeUnit.SECONDS)
    if !stopped then logger.error("a state prune is still running 30 s into shutdown; the stores are left open")
    stopped

  def best: ChainHead = if Thread.holdsLock(this) then bestHead else published._1

  def finalized: ChainHead = if Thread.holdsLock(this) then storedFinalized else published._2

  def heads: (ChainHead, ChainHead) = if Thread.holdsLock(this) then (bestHead, storedFinalized) else published

  private def storedFinalized: ChainHead =
    val hash = blockStore.getHead(BlockStore.FinalizedHead).getOrElse(Hash.zero)
    ChainHead(hash, meta.finalizedSlot.getOrElse(0L), meta.finalizedRoot.getOrElse(Hash.zero))

  /** Mark a main-chain block as finalized: the finalized head advances and
    * side branches forking at or below it are no longer adoptable
    */
  def finalize(hash: Hash, belowCurrent: Boolean = false): Either[ChainError, ChainHead] =
    commit {
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
          if !belowCurrent && blockHeight(hash).getOrElse(0L) < blockHeight(current.hash).getOrElse(0L)
          then Left(ChainError.BelowFinalized)
          else
            blockStore.setHead(BlockStore.FinalizedHead, hash)
            meta.finalizedSlot = slot
            meta.finalizedRoot = root
            val finalizedHeight = blockHeight(hash).getOrElse(0L)
            val it = leafMap.entrySet().iterator()
            while it.hasNext do
              val e = it.next()
              if blockHeight(e.getKey).getOrElse(0L) <= finalizedHeight then it.remove()
            pruneUnverified(slot, finalizedHeight)
            releaseSideStates(hash, finalizedHeight)
            pruner.indexFinalized(hash, finalizedHeight)
            pruner.prune(hash, slot, finalizedHeight)
            logger.info(s"finalized ${hash.toHex.take(18)} slot=$slot")
            Right(ChainHead(hash, slot, root))
    }

  /** Devnet finality rule: finalize the ancestor `depth` blocks behind the
    * best head once at least `quorum` nodes hold it on their best chain: this
    * node plus every peer whose announced best (`peerBests`) descends from it.
    * A stand-in for GRANDPA's supermajority; with a quorum
    * of 2V/3+1 neither side of a network split can finalize alone, so a healed
    * split can still reorg. The finalized pointer feeds the UP 0 handshake.
    * Returns the newly finalized head when it advanced.
    */
  def finalizeAtDepth(depth: Int, peerBests: Iterable[Hash] = Nil, quorum: Int = 1): Option[ChainHead] =
    val (head, fin) = heads
    val bestHeight = blockHeight(head.hash).getOrElse(0L)
    val targetHeight = bestHeight - depth
    if targetHeight <= blockHeight(fin.hash).getOrElse(0L) then None
    else
      ancestorAtHeight(head.hash, targetHeight) match
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
  def followPeerFinality(
      peerFinals: Iterable[(Hash, Long)],
      needed: Int,
      beingFetched: Hash => Boolean = _ => false
  ): Either[Hash, Option[ChainHead]] =
    val events = mutable.ListBuffer.empty[(ChainHead, Block)]
    val result = commit {
      val finals = peerFinals.iterator.map(_._1).toList
      val current = finalized
      def supports(f: Hash, x: Hash, xHeight: Long): Boolean = f == x || ancestorAtHeight(f, xHeight).contains(x)
      val (held, missing) = finals.distinct.partition(x => blockHeight(x).isDefined && blockStore.hasBlock(x))
      val agreed = held
        .flatMap(x => for h <- blockHeight(x); slot <- slotOf(x) if slot > current.slot yield (x, h, slot))
        .sortBy((_, h, slot) => (-slot, -h))
        .find((x, h, _) => finals.count(supports(_, x, h)) >= needed)
      agreed match
        case Some((x, _, _)) =>
          val r = adoptAndFinalize(x, events, belowCurrent = true)
          r.left.foreach {
            case _: ChainError.BelowPruneFloor => () // reorgTo warned once already
            case e                             => logger.warn(s"cannot follow peer finality of ${x.toHex.take(18)}: $e")
          }
          Right(r.toOption)
        case None =>
          missing
            .filter(x => !beingFetched(x) && finals.count(_ == x) >= needed)
            .sortBy(x => -finals.count(_ == x))
            .headOption
            .toLeft(None)
    }
    publishImported(events)
    result

  /** Finalize stored block `x` (GRANDPA), first reorging to the heaviest leaf containing it when it is off our best
    * chain.
    */
  def finalizeAdopting(x: Hash): Either[ChainError, ChainHead] =
    val events = mutable.ListBuffer.empty[(ChainHead, Block)]
    val result = commit(adoptAndFinalize(x, events, belowCurrent = false))
    publishImported(events)
    result

  private def adoptAndFinalize(
      x: Hash,
      events: mutable.ListBuffer[(ChainHead, Block)],
      belowCurrent: Boolean
  ): Either[ChainError, ChainHead] =
    blockHeight(x) match
      case None => Left(ChainError.UnknownBlock)
      case Some(xHeight) =>
        val onBest = blockRoot(x).isDefined && ancestorAtHeight(bestHead.hash, xHeight).contains(x)
        val adopted =
          if onBest then Right(bestHead)
          else
            // With `x` final, what is below it no longer counts against a head.
            val heads = (x :: leafMap.keySet.asScala.toList)
              .filter(l => ancestorAtHeight(l, xHeight).contains(x))
              .map(acceptableHeadAbove(x, _))
              .distinct
            val tip = heads.maxBy(h => (weightOf(h), slotOf(h).getOrElse(0L)))
            if blockRoot(tip).isDefined then Right(bestHead)
            else
              logger.warn(s"adopting ${tip.toHex.take(18)}, which contains finalized ${x.toHex.take(18)}")
              reorgTo(tip, events, ignoreFinality = true)
        val result = adopted.flatMap(_ =>
          if blockRoot(x).isDefined then finalize(x, belowCurrent) else Left(ChainError.NotValidated)
        )
        settleEquivocations(events)
        result

  def genesisHash: Option[Hash] = blockStore.getHead(BlockStore.GenesisHead)

  /** Parent of a stored block. */
  def parentHash(h: Hash): Option[Hash] = parentOf(h)

  /** Slot of a stored block, from its header. */
  def slotOf(h: Hash): Option[Long] = headers.slotOf(h)

  /** Posterior state root of a validated block. */
  def postStateRoot(h: Hash): Option[Hash] = blockRoot(h)

  def heightOf(h: Hash): Option[Long] = blockHeight(h)

  /** Encoded header of a stored block. */
  def headerBytes(h: Hash): Option[Array[Byte]] = blockStore.getHeader(h)

  /** Decoded stored block. */
  def block(h: Hash): Option[Block] = blockStore.getBlock(h).flatMap(b => decodeBlock(b).toOption)

  /** Decoded header of a stored block. */
  def header(h: Hash): Option[io.forge.jam.core.types.header.Header] =
    blockStore.getHeader(h).flatMap(b => headerCodec.decodeExact(b).toOption)

  val headerCodec = io.forge.jam.core.types.header.Header.headerCodec(config.epochLength)

  /** Ancestor of stored block `hash` (inclusive) at `height`; None when `hash` is below `height` or its ancestry
    * leaves the store.
    *
    * Parent links are followed one header at a time, but only down to the finalized block. The finalized chain is
    * indexed by height at every finalization (`HistoryPruner.indexFinalized`), so from a block on it the answer is
    * one lookup however far down it lies. That matters to whoever can name the two ends: a peer asking for blocks
    * from an early one (CE 128) made the node walk the whole chain for each block it returned. Where the index has
    * no entry (a chain that has not finalized since it was opened, heights below a checkpoint) or the block is on
    * a branch that finality passed by, the links are followed as before.
    */
  def ancestorAtHeight(hash: Hash, height: Long): Option[Hash] =
    blockHeight(hash).filter(_ >= height).flatMap { start =>
      val fin = finalized.hash
      val finHeight = blockHeight(fin).getOrElse(-1L)
      val top = math.max(height, math.min(start, finHeight))
      followParents(hash, start, top).flatMap { atTop =>
        if top == height then Some(atTop)
        else
          val onFinalizedChain = if top == finHeight then atTop == fin else meta.isFinalizedAt(top, atTop)
          (if onFinalizedChain then meta.finalizedAt(height) else None).orElse(followParents(atTop, top, height))
      }
    }

  private def followParents(from: Hash, at: Long, to: Long): Option[Hash] =
    var cursor: Option[Hash] = Some(from)
    var h = at
    while h > to && cursor.isDefined do
      cursor = cursor.flatMap(parentOf)
      h -= 1
    cursor

  private[node] def parentLookups: Long = headers.parentLookups

  private def parentOf(h: Hash): Option[Hash] = headers.parentOf(h)

  def leaves: List[ChainHead] =
    val best = this.best
    val rest = leafMap.values().asScala.filterNot(_.hash == best.hash).toList
    best :: rest

  // Per-block chain metadata. A block root marks a block as part of the best chain; a block executed off it has a
  // side root instead (meta.sideRoot), and an unexecuted side-branch block has neither.
  private def blockRoot(h: Hash): Option[Hash] = meta.blockRoot(h)
  private def blockHeight(h: Hash): Option[Long] = meta.height(h)

  private def executedRoot(h: Hash): Option[Hash] = blockRoot(h).orElse(meta.sideRoot(h))

  /** A recorded root need not have its state: a batch far behind the head stores one state for all its blocks. */
  private def stateHeld(root: Hash): Boolean = root == Hash.zero || trieBackend.readNode(root).isDefined

  /** Posterior state root of an executed block, on the best chain or off it. */
  def executedStateRoot(h: Hash): Option[Hash] = executedRoot(h)

  def sideExecutedCount: Int = synchronized(sideStates.size)

  def weightOf(h: Hash): Long = forkChoice.weightOf(h)

  private def recordForkChoiceMeta(hash: Hash, block: Block, sealChecked: Boolean): Unit =
    forkChoice.record(hash, block, sealChecked).foreach(newEquivocations += _)

  private def recordExecuted(hash: Hash, slot: Long): Unit =
    forkChoice.recordExecuted(hash, slot).foreach(newEquivocations += _)

  /** Slot from stored header bytes (parent ++ prior root ++ extrinsic hash ++
    * slot as E_4), 0 for genesis or unknown blocks.
    */
  private def slotOfHeader(h: Hash): Long = headers.slotOrZero(h)

  /** Slots first seen equivocated during the chain change in progress (under [[commit]]). */
  private val newEquivocations = mutable.TreeSet.empty[Long]

  /** Lowest first: leaving one may adopt another branch, whose execution may find more. */
  private def settleEquivocations(events: mutable.ListBuffer[(ChainHead, Block)]): Unit =
    if newEquivocations.nonEmpty then
      storeBufferedState()
      while newEquivocations.nonEmpty do
        val slot = newEquivocations.head
        newEquivocations.remove(slot)
        leaveEquivocatedSlot(slot, events)
      // Blocks the best chain has just left are not announced as imported heads.
      events.filterInPlace((head, _) => blockRoot(head.hash).isDefined)

  /** The best block may have no unfinalized ancestor at a slot where two valid blocks were seen (Gray Paper
    * section 19). When the best chain holds such a block above finality, the best head moves below it, to the best
    * acceptable head ([[leaveBestFrom]]); which of the two came first does not matter.
    */
  private def leaveEquivocatedSlot(slot: Long, events: mutable.ListBuffer[(ChainHead, Block)]): Unit =
    val finalizedHeight = blockHeight(finalized.hash).getOrElse(0L)
    var cursor = bestHead.hash
    while blockHeight(cursor).exists(_ > finalizedHeight) && slotOfHeader(cursor) > slot do
      cursor = parentOf(cursor).getOrElse(Hash.zero)
    if blockHeight(cursor).exists(_ > finalizedHeight) && slotOfHeader(cursor) == slot then
      parentOf(cursor).foreach { parent =>
        logger.warn(s"best chain holds ${cursor.toHex.take(18)} at equivocated slot $slot; leaving it")
        val head = leaveBestFrom(parent, events)
        logger.warn(s"best is now ${head.hash.toHex.take(18)} slot=${head.slot}")
      }

  /** Mark genesis as ticket-sealed when its state already carries a ticket
    * sealer sequence (γ_s ∈ tickets).
    */
  private def recordGenesisForkChoiceMeta(genesisHash: Hash): Unit =
    val ticketed = stateView().gamma.st match
      case io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Tickets(_) => true
      case _                                                                => false
    forkChoice.recordGenesis(genesisHash, ticketed)

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

  /** The [[ChainHead]] for an already-imported block, for leaf bookkeeping:
    * validated post-state root when known, `Hash.zero` for an unvalidated
    * side-branch block.
    */
  private def headOf(h: Hash): ChainHead = ChainHead(h, blockSlot(h), blockRoot(h).getOrElse(Hash.zero))

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
    val floor = pruner.floor
    val stack = mutable.ArrayDeque(if floor == Hash.zero then genesisHash else floor)
    val visited = mutable.HashSet.empty[Hash]
    while stack.nonEmpty do
      val h = stack.removeHead()
      if !visited.contains(h) then
        visited += h
        // Blocks stored unverified by the previous run count against the caps again (their peers are not known).
        meta.unverifiedSize(h).foreach { size =>
          if blockRoot(h).isEmpty then
            parentOf(h).foreach(p => unverified.track(h, slotOfHeader(h), p, size, None))
        }
        if meta.sideRoot(h).isDefined then sideStates(h) = blockHeight(h).getOrElse(0L)
        blockStore.children(h).filter(c => blockHeight(c).isDefined) match
          case Nil =>
            if blockHeight(h).getOrElse(0L) > finalizedHeight then leafMap.put(h, headOf(h))
          case children => children.foreach(stack.prepend)

  private def loadStoredChain(genesisHash: Hash, bestHash: Hash, root: Hash): Unit =
    trieStore.markCommitted(root)
    bestHead = ChainHead(bestHash, meta.bestSlot.getOrElse(0L), root)
    headerLinks.advance(bestHead.slot)
    newEquivocations.clear()
    unverified.clear()
    sideStates.clear()
    sideWindow.clear()
    rebuildLeaves(genesisHash)

  private def recoverFromStore(cause: Throwable): Unit =
    try
      (blockStore.getHead(BlockStore.GenesisHead), blockStore.getHead(BlockStore.BestHead), meta.stateRoot) match
        case (Some(genesisHash), Some(bestHash), Some(root)) =>
          loadStoredChain(genesisHash, bestHash, root)
          logger.warn(
            s"chain change failed (${cause.getClass.getSimpleName}: ${cause.getMessage}); chain restored to " +
              s"best=${bestHash.toHex.take(18)} slot=${bestHead.slot} root=${root.toHex.take(18)}"
          )
        case (None, _, _) =>
          trieStore.markCommitted(Hash.zero)
          bestHead = ChainHead(Hash.zero, 0L, Hash.zero)
          newEquivocations.clear()
          unverified.clear()
          sideStates.clear()
          sideWindow.clear()
          leafMap.clear()
          logger.warn(s"chain initialization failed (${cause.getClass.getSimpleName}: ${cause.getMessage})")
        case _ =>
          throw new IllegalStateException("stored chain has a genesis head but no best head or state root")
    catch
      case e: Throwable =>
        cause.addSuppressed(e)
        logger.error("restoring the chain after a failed chain change failed; the node's view may be wrong", e)

  /** Initialize a fresh database from the chain spec, or restore heads from a
    * previous run. Returns true when genesis was (re-)initialized.
    */
  def initializeOrRestore(spec: ChainSpec): Boolean =
    blockStore.getHead(BlockStore.GenesisHead) match
      case Some(_) =>
        // Restore: re-pin the persisted state root.
        val root = meta.stateRoot
          .getOrElse(throw new IllegalStateException("missing state_root meta"))
        val bestHash = blockStore
          .getHead(BlockStore.BestHead)
          .getOrElse(
            throw new IllegalStateException(
              "block store invariant: genesis head present but BestHead missing on restore"
            )
          )
        val genesisHash = blockStore
          .getHead(BlockStore.GenesisHead)
          .getOrElse(
            throw new IllegalStateException(
              "block store invariant: genesis head present but its hash lookup failed on restore"
            )
          )
        if genesisHash != spec.genesisHeaderHash then
          throw new IllegalStateException(
            s"the data directory holds the chain with genesis ${genesisHash.toHex}, but the chain spec's genesis is " +
              s"${spec.genesisHeaderHash.toHex}: start the new chain with an empty data directory"
          )
        loadStoredChain(genesisHash, bestHash, root)
        resumeStatePruning()
        maybeSweepState(now = true)
        if !meta.hasFinalizedRoot then
          blockStore.getHead(BlockStore.FinalizedHead).flatMap(blockRoot).foreach { finalizedRoot =>
            meta.finalizedRoot = finalizedRoot
          }
        synchronized(publishHeads())
        logger.info(
          s"restored chain: best=${bestHash.toHex.take(18)} slot=${bestHead.slot} root=${root.toHex.take(18)}"
        )
        false
      case None => commit(initializeGenesis(spec))

  private def initializeGenesis(spec: ChainSpec): Boolean =
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
        meta.stateRoot = root
        meta.bestSlot = 0L
        meta.finalizedSlot = 0L
        meta.finalizedRoot = root
        meta.putBlockRoot(genesisHash, root)
        meta.putHeight(genesisHash, 0L)
        bestHead = ChainHead(genesisHash, 0L, root)
        recordGenesisForkChoiceMeta(genesisHash)
        leafMap.clear()
        leafMap.put(genesisHash, bestHead)
        logger.info(
          s"initialized genesis ${genesisHash.toHex.take(18)} root=${root.toHex.take(18)}"
        )
        true


  def atGenesis: Boolean = synchronized(blockStore.getHead(BlockStore.GenesisHead).contains(bestHead.hash))

  def downloadState(
      target: Hash,
      height: Long,
      stateRoot: Hash,
      peers: Iterable[io.forge.jam.network.PeerConnection],
      ask: (io.forge.jam.network.PeerConnection, io.forge.jam.core.JamBytes) => Option[
        (List[Array[Byte]], IndexedSeq[(io.forge.jam.core.JamBytes, io.forge.jam.core.JamBytes)])
      ],
      lied: io.forge.jam.network.PeerConnection => Unit = _ => ()
  ): Either[String, Long] =
    synchronized {
      downloads.incrementAndGet()
      meta.requestSweep()
    }
    var result: Either[String, Long] = Left("the download failed")
    try
      result = new StateDownload(
        target,
        stateRoot,
        trieBackend,
        ask,
        save => synchronized(trieBackend.withJournal(target, height, replaced = false)(save())),
        lied
      ).run(peers)
    finally
      synchronized {
        result.foreach(_ => downloadedRoots += stateRoot)
        downloads.decrementAndGet()
      }
    if result.isLeft then maybeSweepState()
    result

  def startAt(
      targetBytes: Array[Byte],
      stateRoot: Hash,
      ancestorHeaders: List[Array[Byte]],
      grandpaSetId: Option[Long] = None
  ): Either[String, ChainHead] =
    commit {
      for
        block <- decodeBlock(targetBytes)
        hash = headerHashOf(block)
        _ <- Either.cond(atGenesis, (), "the chain has already left genesis")
        _ <- Either.cond(bodyMatchesHeader(block), (), s"start block ${hash.toHex.take(18)}: ${ChainError.BodyMismatch.message}")
        _ <- Either.cond(
          stateRoot == Hash.zero || trieBackend.readNode(stateRoot).isDefined,
          (),
          s"state ${stateRoot.toHex.take(18)} is not in the store"
        )
        older <- ancestorHeaders.foldLeft[Either[String, (Hash, List[(Hash, Header, Array[Byte])])]](Right((block.header.parent, Nil))) {
          case (Right((expected, acc)), bytes) =>
            headerCodec.decodeExact(bytes).left.map(e => s"ancestor header: $e").flatMap { header =>
              val h = Hashing.blake2b256(bytes)
              Either.cond(h == expected, (header.parent, (h, header, bytes) :: acc), s"ancestor ${h.toHex.take(18)} is not the parent expected")
            }
          case (left, _) => left
        }.map(_._2.reverse)
      yield
        val slot = block.header.slot.toLong
        val height = slot
        older.foreach { (h, header, bytes) =>
          if !blockStore.hasBlock(h) then storeBlock(h, header.parent, bytes, Array.emptyByteArray)
        }
        storeBlock(hash, block.header.parent, headerCodec.encode(block.header), targetBytes)
        headerLinks.advance(slot)
        trieStore.markCommitted(stateRoot)
        blockStore.setHead(BlockStore.BestHead, hash)
        blockStore.setHead(BlockStore.FinalizedHead, hash)
        meta.stateRoot = stateRoot
        meta.bestSlot = slot
        meta.finalizedSlot = slot
        meta.finalizedRoot = stateRoot
        meta.putBlockRoot(hash, stateRoot)
        meta.putHeight(hash, height)
        meta.pruneFloor = hash
        meta.putFinalizedAt(height, hash)
        meta.requestSweep()
        downloadedRoots.clear()
        bestHead = ChainHead(hash, slot, stateRoot)
        val ticketed = stateView().gamma.st match
          case io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys.Tickets(_) => true
          case _                                                                => false
        val epoch = slot / config.epochLength
        val marked = (block.header :: older.map(_._2)).exists(header =>
          header.slot.toLong / config.epochLength == epoch && header.ticketsMark.isDefined
        )
        meta.putForkChoice(hash, ticketed, marked, weight = 0L)
        val genesis = blockStore.getHead(BlockStore.GenesisHead)
        val (oldestSlot, belowOldest) = older.lastOption.fold((slot, block.header.parent))((_, header, _) => (header.slot.toLong, header.parent))
        meta.ancestryFloor = Option.unless(genesis.contains(belowOldest))(oldestSlot)
        meta.startGrandpaAtCheckpoint(grandpaSetId.getOrElse(GrandpaVoter.UnknownSet))
        leafMap.clear()
        leafMap.put(hash, bestHead)
        logger.info(
          s"chain started at ${hash.toHex.take(18)} slot=$slot root=${stateRoot.toHex.take(18)} with ${older.size} ancestor header(s)"
        )
        bestHead
    }

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
  // Side-branch execution. A block that does not extend the best head is executed as it arrives when its parent
  // was executed (state in the store), it passes the checks below against that state, fewer than
  // `MaxExecutedPerSlot` executed blocks are known at its slot, and its branch passes through the finalized block.
  // Its state root is recorded as a side root. Adopting its branch moves the best head without executing it again,
  // and the blocks the best chain leaves keep their states the same way. Side states that can no longer be adopted
  // are released at the finalization that settles it.
  //
  // Side-branch admission. A side-branch block that is not executed is stored without being executed, so before
  // it is stored it must pass every check that needs no execution:
  //   - its slot is after its parent's and, when a wall clock is set, not ahead of it by more than
  //     `FutureSlotTolerance`;
  //   - its extrinsic hash, seal and entropy VRF signature verify (BlockPipeline.verifyHeader, the import pipeline's
  //     own steps) against the posterior state of its nearest executed ancestor A. That state is the parent's when
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

  @volatile private var wallClockSlot: Option[() => Long] = None

  /** Refuse side-branch blocks whose slot is more than [[ChainManager.FutureSlotTolerance]] ahead of `now()`. */
  def useWallClock(now: () => Long): Unit = wallClockSlot = Some(now)

  @volatile private[node] var batchStateDepth: Option[Long] =
    ChainManager.batchStateDepth(config, pruneOldState, Option(System.getProperty(ChainManager.BatchStateDepthProperty)))

  def nextBlockSlot: Long =
    val afterHead = best.slot + 1
    wallClockSlot.fold(afterHead)(now => math.max(now(), afterHead))

  /** Side-branch blocks stored without a seal check. */
  def unverifiedCount: Int = synchronized(unverified.size)

  /** Off, no side-branch block is executed as it arrives (for tests of the admission rules). */
  @volatile private[node] var executeSideBlocks: Boolean = true

  private enum SideAdmission:
    case Unverifiable

    /** `parentState` is set when the seal was checked against the parent's own state: the block can be executed. */
    case Sealed(parentState: Option[Hash])

  private def admitSideBlock(
      block: Block,
      size: Int,
      source: Option[Ed25519PublicKey]
  ): Either[ChainError, SideAdmission] =
    val parent = block.header.parent
    val slot = block.header.slot.toLong
    val parentSlot = slotOfHeader(parent)
    if slot <= parentSlot then
      Left(ChainError.SideBranchRefused(s"slot $slot is not after its parent's slot $parentSlot"))
    else if wallClockSlot.exists(now => slot > now() + ChainManager.FutureSlotTolerance) then
      Left(ChainError.SideBranchRefused(s"slot $slot is in the future"))
    else
      // Nearest executed ancestor: on the best chain, or executed off it with its state still held.
      def usable(h: Hash): Option[Hash] = blockRoot(h).orElse(meta.sideRoot(h).filter(stateHeld))
      var ancestor = parent
      var root = usable(ancestor)
      while root.isEmpty && ancestor != Hash.zero do
        ancestor = parentOf(ancestor).getOrElse(Hash.zero)
        root = usable(ancestor)
      root match
        case None => Left(ChainError.SideBranchRefused("its branch does not reach a validated block"))
        case Some(_) if belowStateFloor(ancestor) =>
          Left(ChainError.SideBranchRefused("its branch forks below the oldest state kept"))
        case Some(ancestorRoot) =>
          val ancestorSlot = slotOfHeader(ancestor)
          val epochs = slot / config.epochLength - ancestorSlot / config.epochLength
          val sealKeysKnown =
            ancestor == parent || epochs == 0 ||
              (epochs == 1 && ancestorSlot % config.epochLength >= config.ticketCutoff)
          if sealKeysKnown then
            val trie = StateTrie.at(trieStore.backend, ancestorRoot)
            val view = new TrieBackedJamState(trie, config, new ServiceStorageView(trie), None)
            try
              BlockPipeline.verifyHeader(block, view, parentState = ancestor == parent)
                .left.map(e => ChainError.SideBranchRefused(e.toString))
                .map(_ => SideAdmission.Sealed(Option.when(ancestor == parent)(ancestorRoot)))
            catch
              case e: MissingTrieNodeException =>
                logger.error(s"side-branch block not checked: the state of ${ancestor.toHex.take(18)} is not whole (${e.getMessage})")
                Left(ChainError.ImportFailed(s"state of ${ancestor.toHex.take(18)} is not whole: ${e.getMessage}"))
              case NonFatal(e) => Left(ChainError.SideBranchRefused(s"header check failed: ${e.getMessage}"))
          else unverified.admits(size, source).map(_ => SideAdmission.Unverifiable)

  /** Delete the unverified blocks at or below the finalized slot, and the unverified blocks built on them: none is
    * on the finalized block's chain (its ancestors are validated), so none can be adopted any more.
    */
  private def pruneUnverified(finalizedSlot: Long, finalizedHeight: Long): Unit =
    if unverified.nonEmpty then
      val doomed = unverified.doomedAt(finalizedSlot)
      doomed.foreach { (h, u) =>
        unverified.untrack(h)
        if blockRoot(h).isEmpty then
          header(h).foreach(hd => meta.dropSlotAuthorIf(hd.slot.toLong, hd.authorIndex.toInt, h))
          blockStore.deleteBlock(h, u.parent)
          headerLinks.invalidate(h)
          leafMap.remove(h)
          meta.dropSideBranchMarks(h)
          // Its parent is a leaf again when nothing else builds on it, as a restart's rebuild would find.
          if !doomed.contains(u.parent) && blockStore.hasBlock(u.parent) && blockStore.children(u.parent).isEmpty &&
            blockHeight(u.parent).getOrElse(0L) > finalizedHeight
          then leafMap.put(u.parent, headOf(u.parent))
      }
      if doomed.nonEmpty then
        logger.info(s"pruned ${doomed.size} unverified side-branch block(s) at or below finalized slot $finalizedSlot")

  private[node] def pruneFloorHeight: Long = pruner.floorHeight

  private def belowStateFloor(h: Hash): Boolean =
    pruneOldState && blockHeight(h).exists(_ < pruneFloorHeight)

  private var refusedBelowFloor: Hash = Hash.zero

  private[node] def stateBackendForTesting: RocksDbTrieBackend = trieBackend

  private[node] def scheduledStatePrunes: List[(Hash, Long)] = trieBackend.scheduledPrunes

  /** State trie nodes and raw values on disk (a full scan; for tests and diagnostics). */
  private[node] def storedStateCounts: (Long, Long) =
    awaitStatePruning()
    trieBackend.storedCounts

  private val accumulateCosts =
    java.util.Collections.synchronizedMap(
      new java.util.LinkedHashMap[Hash, Map[Long, io.forge.jam.protocol.pvmhost.AccumulateCost]](16, 0.75f, false) {
        override def removeEldestEntry(e: java.util.Map.Entry[Hash, Map[Long, io.forge.jam.protocol.pvmhost.AccumulateCost]]) =
          size > ChainManager.KeptAccumulateCosts
      }
    )

  private def noteAccumulateCosts(hash: Hash, costs: Map[Long, io.forge.jam.protocol.pvmhost.AccumulateCost]): Unit =
    accumulateCosts.put(hash, costs): Unit

  def takeAccumulateCosts(hash: Hash): Option[Map[Long, io.forge.jam.protocol.pvmhost.AccumulateCost]] =
    Option(accumulateCosts.remove(hash))

  /** Import a block: extends the best head directly, or is stored as a side
    * branch and adopted via reorg when its branch is preferred to the best chain
    * (ticket-sealed count, then the slot of the head) and holds no equivocation. A side-branch
    * block is stored only after the checks described above; `source` is the peer
    * it came from, for the per-peer cap on blocks whose seal cannot be checked.
    */
  def importBlock(blockBytes: Array[Byte], source: Option[Ed25519PublicKey] = None): Either[ChainError, ChainHead] =
    importBatch(List(blockBytes), source).head

  def importBatch(
      blocks: Seq[Array[Byte]],
      source: Option[Ed25519PublicKey] = None,
      budgetNanos: Long = Long.MaxValue,
      each: (Array[Byte], () => Either[ChainError, ChainHead]) => Either[ChainError, ChainHead] = (_, run) => run(),
      bufferBelowSlot: Long = Long.MaxValue
  ): Vector[Either[ChainError, ChainHead]] =
    if blocks.isEmpty then return Vector.empty
    val events = mutable.ListBuffer.empty[(ChainHead, Block)]
    val sealedSide = mutable.ListBuffer.empty[(Hash, Block)]
    val executed = mutable.ListBuffer.empty[Hash]
    val results =
      try
        commit {
          val started = System.nanoTime()
          val outcomes = Vector.newBuilder[Either[ChainError, ChainHead]]
          val remaining = blocks.iterator
          val several = blocks.sizeIs > 1
          var more = true
          try
            while more && remaining.hasNext do
              val blockBytes = remaining.next()
              val run = () =>
                val outcome = importOne(blockBytes, source, events, sealedSide, several, bufferBelowSlot)
                outcome.foreach { head =>
                  executedUncommitted.add(head.hash)
                  executed += head.hash
                }
                outcome
              outcomes += each(blockBytes, run)
              more = System.nanoTime() - started < budgetNanos
            storeBufferedState()
          catch
            case t: Throwable =>
              bufferedTip = null
              stateBuffer.discard()
              throw t
          outcomes.result()
        }
      finally executed.foreach(executedUncommitted.remove)
    publishImported(events)
    sealedSide.foreach { (hash, block) =>
      sealedSideListeners.forEach { l =>
        try l(hash, block)
        catch case NonFatal(e) => logger.error("side-branch block listener failed", e)
      }
    }
    results

  private def farBehindHead(slot: Long, bufferBelowSlot: Long): Boolean =
    batchStateDepth.exists(depth => wallClockSlot.exists(now => slot + depth < now())) &&
      slot < math.max(bufferBelowSlot, storedFinalized.slot)

  private def storeBufferedState(): Unit =
    if stateBuffer.buffering then
      val tip = bufferedTip
      bufferedTip = null
      if tip == null then stateBuffer.discard()
      else trieBackend.withJournal(tip._1, tip._2)(stateBuffer.flush())

  private def importOne(
      blockBytes: Array[Byte],
      source: Option[Ed25519PublicKey],
      events: mutable.ListBuffer[(ChainHead, Block)],
      sealedSide: mutable.ListBuffer[(Hash, Block)],
      inBatch: Boolean,
      bufferBelowSlot: Long
  ): Either[ChainError, ChainHead] =
    newEquivocations.clear()
    val imported = importUnderCommit(blockBytes, source, events, sealedSide, inBatch, bufferBelowSlot)
    settleEquivocations(events)
    imported

  private def importUnderCommit(
      blockBytes: Array[Byte],
      source: Option[Ed25519PublicKey],
      events: mutable.ListBuffer[(ChainHead, Block)],
      sealedSide: mutable.ListBuffer[(Hash, Block)],
      inBatch: Boolean,
      bufferBelowSlot: Long
  ): Either[ChainError, ChainHead] =
      decodeBlock(blockBytes).left.map(ChainError.Undecodable(_)).flatMap { block =>
        val hash = headerHashOf(block)
        val parent = block.header.parent
        if blockRoot(hash).isDefined then Left(ChainError.AlreadyImported)
        else if meta.invalid(hash) then
          Left(if meta.disregarded(hash) then ChainError.Disregarded else ChainError.PreviouslyInvalid)
        else if !bodyMatchesHeader(block) then Left(ChainError.BodyMismatch)
        else if parent == bestHead.hash then
          // The same clock check side-branch blocks get: a block ahead of our clock is not executed, and nothing
          // is recorded against it.
          val slot = block.header.slot.toLong
          wallClockSlot.map(_()).filter(now => slot > now + ChainManager.FutureSlotTolerance) match
            case Some(now) => Left(ChainError.FutureSlot(slot, now))
            case None =>
              meta.sideRoot(hash).filter(stateHeld) match
                case Some(root) =>
                  storeBufferedState()
                  Right(promote(block, hash, root, events))
                case None =>
                  importOnBest(block, blockBytes, events, bodyChecked = true, buffered = inBatch && farBehindHead(slot, bufferBelowSlot))
        else
          storeBufferedState()
          (blockHeight(parent), blockStore.hasBlock(parent)) match
            case (Some(parentHeight), true) =>
              // Known parent on some branch: check what can be checked without executing it, execute it when it
              // can be, and evaluate. A block already stored on a side branch was admitted when it was stored.
              val stored = blockHeight(hash).isDefined && blockStore.hasBlock(hash)
              val slot = block.header.slot.toLong
              val admitted =
                if stored then
                  Right(if unverified.contains(hash) then SideAdmission.Unverifiable else SideAdmission.Sealed(None))
                else admitSideBlock(block, blockBytes.length, source)
              admitted.flatMap {
                case SideAdmission.Sealed(Some(parentRoot))
                    if executeSideBlocks && meta.slotBlocks(slot).sizeIs < ChainManager.MaxExecutedPerSlot &&
                      descendsFromFinalized(parent, parentHeight) =>
                  executeOnSide(block, blockBytes, hash, parentHeight + 1, parentRoot).flatMap { _ =>
                    sealedSide += ((hash, block))
                    chooseAfterSideBlock(hash, block, events)
                  }
                case admission =>
                  val unverifiable = admission == SideAdmission.Unverifiable
                  storeBlock(hash, parent, headerCodec.encode(block.header), blockBytes)
                  if unverifiable then unverified.track(hash, slot, parent, blockBytes.length, source)
                  meta.putHeight(hash, parentHeight + 1)
                  if meta.sideRoot(hash).isEmpty then
                    recordForkChoiceMeta(hash, block, sealChecked = !unverifiable)
                    if !unverifiable then sealedSide += ((hash, block))
                  addLeaf(hash, parent, slot)
                  chooseAfterSideBlock(hash, block, events)
              }
            case _ =>
              Left(ChainError.UnknownParent(parent))
      }

  private def descendsFromFinalized(parent: Hash, parentHeight: Long): Boolean =
    val fin = finalized.hash
    blockHeight(fin).forall(finHeight => parentHeight >= finHeight && ancestorAtHeight(parent, finHeight).contains(fin))

  private def addLeaf(hash: Hash, parent: Hash, slot: Long): Unit =
    leafMap.remove(parent)
    if blockStore.children(hash).isEmpty then leafMap.put(hash, ChainHead(hash, slot, Hash.zero))
    else leafMap.remove(hash)

  private def chooseAfterSideBlock(
      hash: Hash,
      block: Block,
      events: mutable.ListBuffer[(ChainHead, Block)]
  ): Either[ChainError, ChainHead] =
    val (slot, bestSlot) = (block.header.slot.toLong, bestHead.slot)
    val (weight, bestWeight) = (weightOf(hash), weightOf(bestHead.hash))
    val preferred = forkChoice.preferred(weight, slot, bestWeight, bestSlot)
    if preferred && !forkChoice.branchEquivocates(hash) && !forkChoice.branchInvalid(hash) then reorgTo(hash, events)
    else Left(ChainError.StoredOnSideBranch(weight, slot, bestWeight, bestSlot))

  /** The best head, and the importer's place on it, are as before on return. */
  private def executeOnSide(
      block: Block,
      blockBytes: Array[Byte],
      hash: Hash,
      height: Long,
      parentRoot: Hash
  ): Either[ChainError, Unit] =
    val parent = block.header.parent
    val slot = block.header.slot.toLong
    val ancestry = sideWindow.at(parent, block.header.parentStateRoot, slot)
    val outcome =
      try
        trieStore.markCommitted(parentRoot)
        trieBackend.withJournal(hash, height)(
          importer.importBlock(block, RawState(parentRoot, Nil), ancestry, extrinsicHashChecked = true)
        )
      finally trieStore.markCommitted(bestHead.stateRoot)
    outcome match
      case ImportResult.Invalid(_, _, cause) if anchoredBelowHistory(block, cause) =>
        Left(ChainError.CannotCheck("its report is anchored before the history this node holds"))
      case ImportResult.Invalid(error, message, _) =>
        meta.markInvalid(hash)
        Left(ChainError.InvalidBlock(error, message))
      case ImportResult.Failed(message, _) => Left(ChainError.ImportFailed(message))
      case done @ ImportResult.Success(postRoot, _) =>
        faultAfterStateSave()
        noteAccumulateCosts(hash, done.accumulateCosts)
        storeBlock(hash, parent, headerCodec.encode(block.header), blockBytes)
        sideWindow.extend(hash, slot, parent, block.header.parentStateRoot)
        meta.putHeight(hash, height)
        meta.putSideRoot(hash, postRoot)
        sideStates(hash) = height
        recordForkChoiceMeta(hash, block, sealChecked = true)
        recordExecuted(hash, slot)
        addLeaf(hash, parent, slot)
        logger.info(
          s"executed side-branch block ${hash.toHex.take(18)} slot=$slot root=${postRoot.toHex.take(18)} " +
            s"(weight ${weightOf(hash)})"
        )
        Right(())

  /** Side states at or below the finalized height, or off the finalized block's branch, can no longer be adopted. */
  private def releaseSideStates(finalized: Hash, finalizedHeight: Long): Unit =
    if sideStates.nonEmpty then
      val dead = sideStates.iterator.collect {
        case (h, height) if height <= finalizedHeight || !ancestorAtHeight(h, finalizedHeight).contains(finalized) => h
      }.toList
      dead.foreach { h =>
        sideStates.remove(h)
        meta.dropSideRoot(h)
        meta.dropSlotBlock(slotOfHeader(h), h)
        deferUntilCommitted(() => trieBackend.dropJournal(h))
      }
      if dead.nonEmpty then
        sideWindow.clear()
        meta.requestSweep()
        logger.info(s"released the state of ${dead.size} side-branch block(s) that finality passed by")

  private[node] def bodyMatchesHeader(block: Block): Boolean = HeaderChecks.extrinsicHash(block, config).isRight

  private def importOnBest(
      block: Block,
      blockBytes: Array[Byte],
      events: mutable.ListBuffer[(ChainHead, Block)],
      bodyChecked: Boolean,
      buffered: Boolean = false
  ): Either[ChainError, ChainHead] =
    if !bodyChecked && !bodyMatchesHeader(block) then return Left(ChainError.BodyMismatch)
    if !buffered then storeBufferedState()
    else if !stateBuffer.buffering then stateBuffer.begin()
    val started = System.nanoTime()
    val parent = block.header.parent
    val preState = RawState(bestHead.stateRoot, Nil)
    val ancestry = ancestorWindow.at(parent, block.header.parentStateRoot, block.header.slot.toLong)
    // The save belongs to this block at this height: what it replaces can be deleted once the parent's state is no
    // longer kept (see pruneHistory).
    val journalHeight = blockHeight(parent).getOrElse(0L) + 1
    trieBackend.withJournal(headerHashOf(block), journalHeight)(
      importer.importBlock(block, preState, ancestry, extrinsicHashChecked = true)
    ) match
      case ImportResult.Invalid(_, _, cause) if anchoredBelowHistory(block, cause) =>
        Left(ChainError.CannotCheck("its report is anchored before the history this node holds"))
      case ImportResult.Invalid(error, message, _) =>
        meta.markInvalid(headerHashOf(block))
        Left(ChainError.InvalidBlock(error, message))
      case ImportResult.Failed(message, _) => Left(ChainError.ImportFailed(message))
      case done @ ImportResult.Success(postRoot, _) =>
        faultAfterStateSave()
        val hash = headerHashOf(block)
        if stateBuffer.buffering then bufferedTip = (hash, journalHeight)
        noteAccumulateCosts(hash, done.accumulateCosts)
        storeBlock(hash, parent, headerCodec.encode(block.header), blockBytes)
        if unverified.nonEmpty then unverified.untrack(hash) // validated now (a reorg replayed it)
        recordForkChoiceMeta(hash, block, sealChecked = true)
        meta.putHeight(hash, journalHeight)
        becomeBest(block, hash, postRoot, events)
        recordExecuted(hash, block.header.slot.toLong)
        logger.info(
          s"imported block ${hash.toHex.take(18)} slot=${bestHead.slot} root=${postRoot.toHex.take(18)}"
        )
        timings.blockImport.observeNanos(System.nanoTime() - started)
        Right(bestHead)

  private def becomeBest(block: Block, hash: Hash, postRoot: Hash, events: mutable.ListBuffer[(ChainHead, Block)]): Unit =
    val parent = block.header.parent
    val slot = block.header.slot.toLong
    if sideStates.remove(hash).isDefined then meta.dropSideRoot(hash)
    headerLinks.advance(slot)
    ancestorWindow.extend(hash, slot, parent, block.header.parentStateRoot)
    blockStore.setHead(BlockStore.BestHead, hash)
    meta.stateRoot = postRoot
    meta.bestSlot = slot
    meta.putBlockRoot(hash, postRoot)
    bestHead = ChainHead(hash, slot, postRoot)
    leafMap.remove(parent)
    if blockStore.children(hash).isEmpty then leafMap.put(hash, bestHead)
    else leafMap.remove(hash)
    events += ((bestHead, block))

  /** A block executed off the best chain joins it with the state it was given then. */
  private def promote(block: Block, hash: Hash, root: Hash, events: mutable.ListBuffer[(ChainHead, Block)]): ChainHead =
    becomeBest(block, hash, root, events)
    trieStore.markCommitted(root)
    logger.info(s"adopted block ${hash.toHex.take(18)} slot=${bestHead.slot} root=${root.toHex.take(18)} (executed earlier)")
    bestHead

  /** A block the best chain leaves keeps its state as a side state. */
  private def demote(h: Hash): Unit =
    blockRoot(h).foreach { root =>
      meta.putSideRoot(h, root)
      sideStates(h) = blockHeight(h).getOrElse(0L)
    }
    meta.dropBlockRoot(h)
    Option(leafMap.get(h)).foreach(head => leafMap.put(h, head.copy(stateRoot = Hash.zero)))

  private def anchoredBelowHistory(block: Block, cause: Option[PipelineError]): Boolean =
    cause.contains(PipelineError.ReportErr(ReportErrorCode.LookupAnchorNotRecent)) &&
      meta.ancestryFloor.exists { floor =>
        val windowStart = block.header.slot.toLong - config.maxLookupAnchorAge
        windowStart < floor && block.extrinsic.guarantees.exists { g =>
          val anchorSlot = g.report.context.lookupAnchorSlot.toLong
          anchorSlot >= windowStart && anchorSlot < floor
        }
      }

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

  /** Adopt the branch ending at `tip`: rewind to the common ancestor (the nearest ancestor of `tip` on the best
    * chain) and move the best head along the branch, executing the blocks of it that were not executed yet.
    */
  private def reorgTo(
      tip: Hash,
      events: mutable.ListBuffer[(ChainHead, Block)],
      ignoreFinality: Boolean = false
  ): Either[ChainError, ChainHead] =
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
      blockHeight(finalized.hash).getOrElse(0L)
    if blockHeight(ancestor).getOrElse(0L) < finalizedHeight && !ignoreFinality then
      return Left(ChainError.RevertsFinalized)

    if belowStateFloor(ancestor) || (ancestorRoot != Hash.zero && trieBackend.readNode(ancestorRoot).isEmpty) then
      val refused = ChainError.BelowPruneFloor(blockHeight(ancestor).getOrElse(0L), pruneFloorHeight)
      if refusedBelowFloor != tip then
        refusedBelowFloor = tip
        logger.warn(s"cannot adopt the branch ending at ${tip.toHex.take(18)}: ${refused.message}")
      else logger.debug(s"cannot adopt the branch ending at ${tip.toHex.take(18)}: ${refused.message}")
      return Left(refused)

    val previousBest = bestHead
    // Abandoned main-chain blocks (best back to ancestor) lose main status.
    val abandoned = mutable.ListBuffer.empty[Hash]
    val abandonedBlocks = mutable.ListBuffer.empty[Block]
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
        case Right(b) =>
          abandonedBlocks += b
          back = b.header.parent
        case Left(e) => return Left(ChainError.Undecodable(s"reorg: undecodable main block: $e"))

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
    bestHead = ChainHead(ancestor, ancestorSlot, ancestorRoot)

    val replayed = mutable.ListBuffer.empty[Hash]
    val adoptedBlocks = mutable.ListBuffer.empty[Block]
    boundary:
      branch.foreach { bytes =>
        val decoded = decodeBlock(bytes).left.map(ChainError.Undecodable(_))
        decoded.flatMap { b =>
          val hash = headerHashOf(b)
          meta.sideRoot(hash).filter(stateHeld) match
            case Some(root) => Right(promote(b, hash, root, events))
            case None       => importOnBest(b, bytes, events, bodyChecked = false)
        } match
          case Right(head) =>
            replayed += head.hash
            decoded.foreach(adoptedBlocks += _)
          case Left(err) =>
            replayed.foreach(demote)
            trieStore.markCommitted(previousBest.stateRoot)
            bestHead = previousBest
            blockStore.setHead(BlockStore.BestHead, previousBest.hash)
            meta.stateRoot = previousBest.stateRoot
            meta.bestSlot = previousBest.slot
            break(Left(ChainError.ReplayFailed(err)))
      }
      abandoned.foreach(demote)
      if abandonedBlocks.nonEmpty then
        abandonListeners.forEach { l =>
          try l(abandonedBlocks.toList, adoptedBlocks.toList)
          catch case e: Exception => logger.error("abandoned-block listener failed", e)
        }
      Right(bestHead)

  def hasBlock(hash: Hash): Boolean = blockStore.hasBlock(hash)

  /** Raw read of a 31-byte state key from the current best state. */
  def readRawState(stateKey: JamBytes): Option[JamBytes] =
    trieStore.at(best.stateRoot).read(stateKey)

  /** The stored state with root `root`, if its trie nodes are still held (the empty state always is). */
  def stateAtRoot(root: Hash): Option[StateTrie] =
    if root == Hash.zero || trieStore.backend.readNode(root).isDefined then Some(StateTrie.at(trieStore.backend, root))
    else None

  def stateAt(headerHash: Hash): Option[StateTrie] =
    // Below the pruning floor the state has been deleted (in part or whole), whether or not the block is kept.
    if pruneOldState && blockHeight(headerHash).exists(_ < pruneFloorHeight) then None
    else executedRoot(headerHash).flatMap(stateAtRoot)

  /** A read view over the posterior state of stored block `headerHash`. */
  def stateViewAt(headerHash: Hash): Option[TrieBackedJamState] =
    stateAt(headerHash).map(trie => new TrieBackedJamState(trie, config, new ServiceStorageView(trie), None))

  /** Disregard block `hash` and every descendant (a report judged invalid by a third of the
    * validators). When it is on the best chain above finality, the best head rewinds to its parent and the heaviest
    * remaining branch is adopted.
    */
  def disregard(hash: Hash): Either[ChainError, ChainHead] =
    val events = mutable.ListBuffer.empty[(ChainHead, Block)]
    val result = commit {
      meta.markDisregarded(hash)
      val finalizedHeight = blockHeight(finalized.hash).getOrElse(0L)
      (blockRoot(hash), parentOf(hash)) match
        case (None, _) => Left(ChainError.NotOnBestChain)
        case (_, None) => Left(ChainError.NoParent)
        case (Some(_), Some(parent)) if blockHeight(hash).getOrElse(0L) <= finalizedHeight =>
          Left(ChainError.AlreadyFinalized(parent))
        case (Some(_), Some(parent)) =>
          leaveBestFrom(parent, events)
          settleEquivocations(events)
          Right(bestHead)
    }
    publishImported(events)
    result

  /** Rewind the best head to `parent` and adopt the best acceptable head that is preferred to it; `parent` itself
    * when there is none.
    */
  private def leaveBestFrom(parent: Hash, events: mutable.ListBuffer[(ChainHead, Block)]): ChainHead =
    rewindTo(parent)
    val (pw, ps) = (weightOf(parent), slotOfHeader(parent))
    val candidates = leafMap.keySet.asScala.toList.flatMap(acceptableHead).distinct.filter(h =>
      forkChoice.preferred(weightOf(h), slotOfHeader(h), pw, ps)
    )
    candidates.sortBy(h => (-weightOf(h), -slotOfHeader(h))).iterator
      .map(h => reorgTo(h, events))
      .collectFirst { case Right(h) => h }
      .getOrElse(bestHead)

  private def unacceptable(h: Hash): Boolean = forkChoice.isEquivocated(h) || meta.invalid(h)

  /** Of the blocks from the best chain up to `tip`, the last before the first that may not be on the best chain. */
  private def acceptableHead(tip: Hash): Option[Hash] =
    val branch = mutable.ListBuffer.empty[Hash]
    var cursor = tip
    while cursor != Hash.zero && blockRoot(cursor).isEmpty do
      branch.prepend(cursor)
      cursor = parentOf(cursor).getOrElse(Hash.zero)
    branch.takeWhile(!unacceptable(_)).lastOption

  /** As [[acceptableHead]] for the blocks above `x` up to `tip`, once `x` is final. */
  private def acceptableHeadAbove(x: Hash, tip: Hash): Hash =
    val above = mutable.ListBuffer.empty[Hash]
    var cursor = tip
    while cursor != x && cursor != Hash.zero do
      above.prepend(cursor)
      cursor = parentOf(cursor).getOrElse(Hash.zero)
    above.takeWhile(!unacceptable(_)).lastOption.getOrElse(x)

  /** Move the best head back to its ancestor `ancestor`; the blocks above it lose main-chain status. */
  private def rewindTo(ancestor: Hash): Unit =
    var back = bestHead.hash
    val left = mutable.ListBuffer.empty[Block]
    while back != ancestor && back != Hash.zero do
      val parent = parentOf(back).getOrElse(Hash.zero)
      blockStore.getBlock(back).filter(_.nonEmpty).flatMap(b => decodeBlock(b).toOption).foreach(left += _)
      demote(back)
      back = parent
    if left.nonEmpty then
      abandonListeners.forEach { l =>
        try l(left.toList, Nil)
        catch case e: Exception => logger.error("abandoned-block listener failed", e)
      }
    val root = blockRoot(ancestor).getOrElse(Hash.zero)
    trieStore.markCommitted(root)
    bestHead = ChainHead(ancestor, blockSlot(ancestor), root)
    blockStore.setHead(BlockStore.BestHead, ancestor)
    meta.stateRoot = root
    meta.bestSlot = bestHead.slot
    if blockStore.children(ancestor).isEmpty then leafMap.put(ancestor, bestHead)
    logger.warn(s"best head rewound to ${ancestor.toHex.take(18)} slot=${bestHead.slot}")

  /** A read view over the current best state (mutations are staged in the
    * view and discarded; imports go through importBlock).
    */
  def stateView(): TrieBackedJamState = stateViewOf(best)

  /** The state after `head`, which the caller read once: a role that takes the head and then its state with two
    * reads of the chain can get the state of a newer head, and build on a parent whose state it never saw.
    */
  def stateViewOf(head: ChainHead): TrieBackedJamState = stateViewAtRoot(head.stateRoot)

  /** As [[stateViewOf]], by the head's state root. The view reads service accounts from its own state, never through
    * the store's caches: those are the importer's, for the pinned root.
    */
  def stateViewAtRoot(root: Hash): TrieBackedJamState = {
    val trie = StateTrie.at(trieStore.backend, root)
    new TrieBackedJamState(
      trie,
      config,
      new ServiceStorageView(trie),
      None
    )
  }

  private val executedUncommitted = java.util.concurrent.ConcurrentHashMap.newKeySet[Hash]()

  def servable(hash: Hash): Boolean =
    hasBlock(hash) && (executedUncommitted.contains(hash) || canBeFinalized(hash))

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

  /** Walk up to `max` blocks strictly after `from` (ascending, exclusive) via the children index. At a fork the
    * child on our best chain is taken, else the heaviest: a peer catching up should be led along the chain we
    * would have it adopt, not along whichever sibling was stored first.
    */
  def blocksAscending(from: Hash, max: Int): List[Array[Byte]] =
    val out = mutable.ListBuffer.empty[Array[Byte]]
    var cursor = from
    var continue = true
    val bestNow = best.hash
    val finHeight = blockHeight(finalized.hash).getOrElse(-1L)
    lazy val bestAboveFinality: Map[Long, Hash] =
      val chain = mutable.HashMap.empty[Long, Hash]
      var c: Option[Hash] = Some(bestNow)
      var h = blockHeight(bestNow).getOrElse(-1L)
      while h > finHeight && c.isDefined do
        chain(h) = c.get
        c = c.flatMap(parentOf)
        h -= 1
      chain.toMap
    def onBestChain(c: Hash): Boolean =
      blockHeight(c).exists(h => h <= finHeight || bestAboveFinality.get(h).contains(c))
    while out.size < max && continue do
      val children = blockStore.children(cursor).filter(canBeFinalized)
      val preferred = children
        .find(onBestChain)
        .orElse(children.maxByOption(c => (weightOf(c), slotOf(c).getOrElse(0L))))
      preferred.toList match
        case child :: _ =>
          blockStore.getBlock(child) match
            case Some(bytes) if bytes.nonEmpty =>
              out += bytes
              cursor = child
            case _ => continue = false
        case Nil => continue = false
    out.toList
