package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.header.Header
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend}

import scala.collection.mutable

/** Pruning of finalized history.
  *
  * A node needs the headers of the last `maxLookupAnchorAge` slots (the ancestor set a lookup anchor is checked
  * against) and nothing else of the finalized chain. Everything of a block is therefore deleted once it is more
  * than `maxLookupAnchorAge` slots behind the finalized block: body, header, fork-choice and equivocation
  * metadata, its finality proof (warp-sync fragments are kept apart, per voter set), and every side branch that
  * forks off it. The oldest block kept (the "floor") anchors the walk and the leaf rebuild after a restart;
  * genesis is always kept.
  */
private[node] final class HistoryPruner(
    config: ChainConfig,
    blockStore: BlockStore,
    trieBackend: RocksDbTrieBackend,
    meta: ChainMeta,
    headers: StoredHeaders,
    pruneFinalizedHistory: Boolean,
    pruneOldState: Boolean,
    pruneState: (Hash, Long) => Unit,
    header: Hash => Option[Header],
    forget: Hash => Unit,
    defer: (() => Unit) => Unit
) extends LazyLogging:

  /** The oldest main-chain block whose data is kept; genesis until anything has been pruned. */
  def floor: Hash =
    meta.pruneFloor
      .orElse(blockStore.getHead(BlockStore.GenesisHead))
      .getOrElse(Hash.zero)

  def floorHeight: Long = meta.height(floor).getOrElse(0L)

  /** Record the finalized chain by height, from `hash` down to the first block already recorded, so pruning can
    * walk it upward from the floor. Entries above `height` belong to a finalized head this one replaced (finality
    * followed from peers may move it back) and are dropped.
    */
  def indexFinalized(hash: Hash, height: Long): Unit =
    var stale = height + 1
    while meta.hasFinalizedAt(stale) do
      meta.dropFinalizedAt(stale)
      stale += 1
    val lowest = floorHeight
    var cursor: Option[Hash] = Some(hash)
    var h = height
    var recorded = false
    while !recorded && cursor.isDefined && h >= lowest do
      val c = cursor.get
      if meta.isFinalizedAt(h, c) then recorded = true
      else
        meta.putFinalizedAt(h, c)
        meta.dropSlotBlock(headers.slotOrZero(c), c)
        cursor = headers.parentOf(c)
        h -= 1

  /** Delete what lies more than `maxLookupAnchorAge` slots behind the finalized block, at most
    * `MaxPrunedPerFinalization` main-chain blocks at a time so a finalization never stalls on a long backlog (the
    * rest goes at the next ones).
    */
  def prune(finalizedHash: Hash, finalizedSlot: Long, finalizedHeight: Long): Unit =
    val cutoff = finalizedSlot - config.maxLookupAnchorAge
    if (pruneFinalizedHistory || pruneOldState) && cutoff > 0 then
      var oldest = floor
      var oldestHeight = meta.height(oldest).getOrElse(0L)
      val genesis = blockStore.getHead(BlockStore.GenesisHead)
      var pruned = 0
      var forks = 0
      var more = true
      while more && pruned < ChainManager.MaxPrunedPerFinalization do
        // The floor goes once the block after it is itself outside the window; the finalized block always stays.
        meta.finalizedAt(oldestHeight + 1) match
          case Some(n) if oldestHeight + 1 < finalizedHeight && n != finalizedHash &&
                blockStore.getHeader(n).isDefined && headers.slotOrZero(n) < cutoff =>
            // Side branches off the old floor fork outside the lookup-anchor window: none can be adopted.
            // Blocks go only when asked (pruneFinalizedHistory); the state before the new floor goes either way,
            // so a side branch off the old floor is unusable from here on and its journal is dropped.
            val branches = blockStore.children(oldest).filterNot(_ == n)
            if pruneFinalizedHistory then branches.foreach(root => forks += deleteBranch(root))
            else branches.foreach(dropBranchJournals)
            if branches.nonEmpty then meta.requestSweep()
            meta.pruneFloor = n
            if pruneFinalizedHistory then
              if !genesis.contains(oldest) then deleteBlockRecord(oldest, finalizedChain = true)
              meta.dropFinalizedAt(oldestHeight)
            // The old floor's state goes: every state still kept descends from `n`, so the nodes `n` replaced
            // are unreachable unless a later block brought them back.
            val height = oldestHeight + 1
            if pruneOldState then defer(() => pruneState(n, height))
            else defer(() => trieBackend.dropJournal(n))
            oldest = n
            oldestHeight += 1
            pruned += 1
          case _ => more = false
      if pruned > 0 then
        logger.info(
          s"pruned ${if pruneFinalizedHistory then pruned else 0} finalized block(s), $forks side-branch block(s), " +
            s"${if pruneOldState then pruned else 0} state(s) more than ${config.maxLookupAnchorAge} slots behind finalized slot " +
            s"$finalizedSlot; oldest kept at height $oldestHeight"
        )

  /** Drop the state journals of `root` and everything built on it, keeping the blocks. */
  private def dropBranchJournals(root: Hash): Unit =
    val stack = mutable.ArrayDeque(root)
    val branch = mutable.ListBuffer.empty[Hash]
    while stack.nonEmpty do
      val h = stack.removeHead()
      branch += h
      blockStore.children(h).foreach(stack.append)
    defer(() => branch.foreach(trieBackend.dropJournal))

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

  /** Delete everything stored under one block: its metadata with the chain change, the block itself once that is
    * durable. The equivocation mark of its slot goes only with the finalized chain's block: a side-branch block may
    * sit at a slot that is not finalized yet, where the mark still counts.
    */
  private def deleteBlockRecord(h: Hash, finalizedChain: Boolean): Unit =
    val parent = headers.parentOf(h).getOrElse(Hash.zero)
    header(h).foreach { hd =>
      val slot = hd.slot.toLong
      meta.dropSlotAuthorIf(slot, hd.authorIndex.toInt, h)
      meta.dropSlotBlock(slot, h)
      if finalizedChain then meta.dropEquivocated(slot)
    }
    meta.dropBlock(h)
    defer { () =>
      blockStore.deleteBlock(h, parent)
      if !finalizedChain then trieBackend.dropJournal(h)
      forget(h)
    }
