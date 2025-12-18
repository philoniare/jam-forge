package io.forge.jam.conformance

import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.traces.RawState

import scala.collection.mutable

/**
 * In-memory state store indexed by header hash.
 */
class StateStore:
  // State indexed by header hash
  private val states: mutable.Map[Hash, RawState] = mutable.Map.empty

  // Track which header hashes are "original" blocks (not mutations/forks)
  private val originalBlocks: mutable.Set[Hash] = mutable.Set.empty

  // Current ancestry list (from Initialize message)
  private var ancestry: List[AncestryItem] = List.empty

  // Maximum ancestry length (L=24 for tiny spec)
  val maxAncestryLength: Int = 24

  /**
   * Initialize the state store with genesis state and ancestry.
   *
   * @param headerHash Hash of the genesis-like header
   * @param state Initial state (RawState with keyvals)
   * @param initialAncestry Ancestry list from Initialize message
   */
  def initialize(headerHash: Hash, state: RawState, initialAncestry: List[AncestryItem]): Unit =
    synchronized {
      clear()
      states.put(headerHash, state)
      originalBlocks.add(headerHash)
      ancestry = initialAncestry.take(maxAncestryLength)
    }

  /**
   * Store state for a given header hash.
   *
   * @param headerHash Hash identifying this state
   * @param state The RawState to store
   * @param isOriginal Whether this is an original block (true) or mutation/fork (false)
   */
  def store(headerHash: Hash, state: RawState, isOriginal: Boolean = true): Unit =
    synchronized {
      states.put(headerHash, state)
      if isOriginal then
        originalBlocks.add(headerHash)
    }

  /**
   * Retrieve state by header hash.
   *
   * @param headerHash Hash to look up
   * @return Some(state) if found, None otherwise
   */
  def get(headerHash: Hash): Option[RawState] =
    synchronized {
      states.get(headerHash)
    }

  /**
   * Check if a header hash exists in the store.
   */
  def contains(headerHash: Hash): Boolean =
    synchronized {
      states.contains(headerHash)
    }

  /**
   * Check if a header hash is an original block (not a mutation).
   */
  def isOriginalBlock(headerHash: Hash): Boolean =
    synchronized {
      originalBlocks.contains(headerHash)
    }

  /**
   * Get the current ancestry list.
   */
  def getAncestry: List[AncestryItem] =
    synchronized {
      ancestry
    }

  /**
   * Update ancestry with a new block.
   * Adds the new item at the front and trims to max length.
   */
  def addToAncestry(item: AncestryItem): Unit =
    synchronized {
      ancestry = (item :: ancestry).take(maxAncestryLength)
    }

  /**
   * Check if a header hash is in the current ancestry.
   */
  def isInAncestry(headerHash: Hash): Boolean =
    synchronized {
      ancestry.exists(_.headerHash == headerHash)
    }

  /**
   * Get the number of stored states.
   */
  def size: Int =
    synchronized {
      states.size
    }

  /**
   * Clear all stored state.
   */
  def clear(): Unit =
    synchronized {
      states.clear()
      originalBlocks.clear()
      ancestry = List.empty
    }

  /**
   * Remove old fork states that are no longer needed.
   * Called after advancing past a fork point.
   *
   * @param keepHashes Set of header hashes to keep
   */
  def pruneForks(keepHashes: Set[Hash]): Unit =
    synchronized {
      val toRemove = states.keys.filterNot(keepHashes.contains).toList
      for hash <- toRemove do
        states.remove(hash)
        originalBlocks.remove(hash)
    }
