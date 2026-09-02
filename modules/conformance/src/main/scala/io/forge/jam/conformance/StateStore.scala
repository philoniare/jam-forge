package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.{Hash, Timeslot}
import io.forge.jam.protocol.traces.RawState

import scala.collection.mutable

/** In-memory state store indexed by header hash.
  *
  * Implements a bounded storage strategy to prevent OOM during long fuzzing
  * sessions:
  *   - Always retains states referenced by the current ancestry (up to
  *     `maxAncestryLength` entries)
  *   - Keeps a sliding window of recently stored states for fork support
  *   - Prunes oldest non-ancestry states when the window is exceeded
  *
  * @param config
  *   chain configuration; `maxLookupAnchorAge` (the lookup-anchor window L)
  *   sizes the ancestry window (24 TINY / 14400 FULL).
  */
class StateStore(config: ChainConfig = ChainConfig.TINY):
  // State indexed by header hash
  private val states: mutable.Map[Hash, RawState] = mutable.Map.empty

  // Track which header hashes are "original" blocks (not mutations/forks)
  private val originalBlocks: mutable.Set[Hash] = mutable.Set.empty

  // Per-block parentage: headerHash -> (parentHash, slot). Used to derive a
  // per-chain ancestry by walking parent links, instead of a single global
  // ancestry list that fork mutations would pollute.
  private val parentage: mutable.Map[Hash, (Hash, Timeslot)] =
    mutable.Map.empty

  private var newestParentageSlot: Long = Long.MinValue

  // Insertion-ordered list of stored hashes for LRU-style pruning
  private val insertionOrder: mutable.ArrayBuffer[Hash] =
    mutable.ArrayBuffer.empty

  // Current ancestry list (from Initialize message). Seeds the pre-session
  // segment that predates any block imported into this store.
  private var ancestry: List[AncestryItem] = List.empty

  // Maximum ancestry length = lookup-anchor window L (24 TINY / 14400 FULL).
  val maxAncestryLength: Int = config.maxLookupAnchorAge.toInt

  // Maximum number of non-ancestry states to retain for fork support.
  private var maxExtraStates: Int = 4

  def setForksEnabled(forksEnabled: Boolean): Unit =
    synchronized {
      maxExtraStates = if forksEnabled then 8 else 4
    }

  /** Initialize the state store with genesis state and ancestry.
    *
    * @param headerHash
    *   Hash of the genesis-like header
    * @param state
    *   Initial state (RawState with keyvals)
    * @param initialAncestry
    *   Ancestry list from Initialize message
    */
  def initialize(
      headerHash: Hash,
      state: RawState,
      initialAncestry: List[AncestryItem],
      genesisParent: Option[(Hash, Timeslot)] = None
  ): Unit =
    synchronized {
      clear()
      states.put(headerHash, state)
      originalBlocks.add(headerHash)
      genesisParent.foreach { p =>
        parentage.put(headerHash, p)
        newestParentageSlot = math.max(newestParentageSlot, p._2.value.toLong & 0xffffffffL)
      }
      insertionOrder += headerHash
      ancestry = initialAncestry.take(maxAncestryLength)
    }

  /** Store state for a given header hash.
    *
    * @param headerHash
    *   Hash identifying this state
    * @param state
    *   The RawState to store
    * @param isOriginal
    *   Whether this is an original block (true) or mutation/fork (false)
    * @param parent
    *   Optional `(parentHash, slot)` parentage for this block, used to derive a
    *   per-chain ancestry via `ancestryFor`.
    */
  def store(
      headerHash: Hash,
      state: RawState,
      isOriginal: Boolean = true,
      parent: Option[(Hash, Timeslot)] = None
  ): Unit =
    synchronized {
      states.put(headerHash, state)
      if isOriginal then originalBlocks.add(headerHash)
      parent.foreach { p =>
        parentage.put(headerHash, p)
        newestParentageSlot = math.max(newestParentageSlot, p._2.value.toLong & 0xffffffffL)
      }
      insertionOrder += headerHash
      pruneIfNeeded()
      pruneParentageByAge()
    }

  /** Prune oldest non-ancestry states when storage exceeds the limit. Must be
    * called within synchronized block.
    */
  private def pruneIfNeeded(): Unit =
    val ancestryHashes = ancestry.map(_.headerHash).toSet
    // Count how many stored states are NOT in ancestry
    val extraCount = states.size - states.keys.count(ancestryHashes.contains)

    if extraCount > maxExtraStates then
      // Walk insertion order from oldest, removing non-ancestry entries
      var removed = 0
      val toRemoveCount = extraCount - maxExtraStates
      val toRemoveHashes = mutable.ListBuffer[Hash]()

      val it = insertionOrder.iterator
      while it.hasNext && removed < toRemoveCount do
        val hash = it.next()
        if !ancestryHashes.contains(hash) && states.contains(hash) then
          toRemoveHashes += hash
          removed += 1

      for hash <- toRemoveHashes do
        states.remove(hash)
        originalBlocks.remove(hash)

      // Compact insertion order: remove entries no longer in states
      val stateKeys = states.keySet
      val newOrder = insertionOrder.filter(stateKeys.contains)
      insertionOrder.clear()
      insertionOrder ++= newOrder

  /** Prune parentage links older than the lookup-anchor window L, independently
    * of state retention
    */
  private def pruneParentageByAge(): Unit =
    if parentage.nonEmpty then
      def slotOf(s: Timeslot): Long = s.value.toLong & 0xffffffffL
      val minKeep = newestParentageSlot - maxAncestryLength.toLong
      if minKeep > 0 then
        val stale = parentage.iterator.collect {
          case (h, (_, slot)) if slotOf(slot) < minKeep => h
        }.toList
        stale.foreach(parentage.remove)

  /** Retrieve state by header hash.
    *
    * @param headerHash
    *   Hash to look up
    * @return
    *   Some(state) if found, None otherwise
    */
  def get(headerHash: Hash): Option[RawState] =
    synchronized {
      states.get(headerHash)
    }

  /** Check if a header hash exists in the store.
    */
  def contains(headerHash: Hash): Boolean =
    synchronized {
      states.contains(headerHash)
    }

  /** Check if a header hash is an original block (not a mutation).
    */
  def isOriginalBlock(headerHash: Hash): Boolean =
    synchronized {
      originalBlocks.contains(headerHash)
    }

  /** Derive the ancestry for a block whose parent is `parentHash`, per-chain.
    *
    * Walks the recorded parent links from `parentHash` toward genesis,
    * collecting `(headerHash, slot)` for every known imported ancestor, then
    * appends the Initialize-supplied ancestry for the pre-session segment. The
    * result is sorted newest-first by slot and trimmed to `maxAncestryLength`.
    */
  def ancestryFor(parentHash: Hash): List[AncestryItem] =
    synchronized {
      val acc = mutable.ListBuffer[AncestryItem]()
      val seen = mutable.Set[Hash]()
      var current: Hash = parentHash
      var continue = true
      while continue do
        parentage.get(current) match
          case Some((nextParent, slot)) if !seen.contains(current) =>
            acc += AncestryItem(slot, current)
            seen += current
            current = nextParent
          case _ =>
            continue = false
      // Append the Initialize-supplied pre-session ancestry, skipping any
      // header already captured by walking parent links.
      ancestry.foreach(item =>
        if !seen.contains(item.headerHash) then
          acc += item
          seen += item.headerHash
      )
      acc.toList
        .sortBy(item => -(item.slot.value.toLong & 0xffffffffL))
        .take(maxAncestryLength)
    }


  /** Get the number of stored states.
    */
  def size: Int =
    synchronized {
      states.size
    }

  /** Clear all stored state.
    */
  def clear(): Unit =
    synchronized {
      states.clear()
      originalBlocks.clear()
      parentage.clear()
      newestParentageSlot = Long.MinValue
      insertionOrder.clear()
      ancestry = List.empty
    }
