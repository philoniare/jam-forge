package io.forge.jam.protocol.history

import io.forge.jam.core.{JamBytes, Hashing, ChainConfig, constants}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.history.HistoryTypes.*
import io.forge.jam.protocol.state.JamState

/**
 * History State Transition Function.
 *
 * Tracks block history using a Merkle Mountain Range (MMR) for efficient
 * beefy root calculation. Maintains up to BLOCK_HISTORY_LIMIT historical entries.
 */
object HistoryTransition:

  /** Zero hash (32 zero bytes) */
  val ZeroHash: Hash = Hash.zero

  /** MMR peak prefix: "peak" */
  private val PeakPrefix: Array[Byte] = "peak".getBytes("US-ASCII")

  /**
   * Execute the History STF using unified JamState.
   *
   * Reads: recentHistory (beta)
   * Writes: recentHistory (beta)
   *
   * @param input The historical input containing header hash, parent state root,
   *              accumulate root, and work packages.
   * @param state The unified JamState.
   * @param config The chain configuration (for max cores validation).
   * @return The updated JamState.
   */
  def stf(input: HistoricalInput, state: JamState, config: ChainConfig = ChainConfig.FULL): JamState =
    // Extract HistoricalState using lens bundle
    val preState = JamState.HistoryLenses.extract(state)

    val postState = stfInternal(input, preState, config)

    // Apply results back using lens bundle
    JamState.HistoryLenses.apply(state, postState)

  /**
   * Internal History STF implementation using HistoricalState.
   *
   * @param input The historical input containing header hash, parent state root,
   *              accumulate root, and work packages.
   * @param preState The pre-transition state.
   * @param config The chain configuration (for max cores validation).
   * @return The post-transition state.
   */
  def stfInternal(
    input: HistoricalInput,
    preState: HistoricalState,
    config: ChainConfig = ChainConfig.FULL
  ): HistoricalState =
    input.validate(config)

    val history = preState.beta.history
    val currentMmr = preState.beta.mmr

    // Update parent state root in the last history entry
    val updatedHistory =
      if history.nonEmpty then
        val lastBlock = history.last.copy(stateRoot = input.parentStateRoot)
        history.dropRight(1) :+ lastBlock
      else
        history

    // Append accumulate root to MMR
    val newMmr = appendToMmr(currentMmr, input.accumulateRoot)

    // Calculate beefy root from the new MMR peaks
    val beefyRoot = calculateBeefyRoot(newMmr)

    // Create new block entry
    val newBlock = HistoricalBeta(
      headerHash = input.headerHash,
      beefyRoot = beefyRoot,
      stateRoot = ZeroHash,
      reported = input.workPackages
    )

    // Add new block and enforce history limit (H = 8 from Gray Paper)
    val newHistory = (updatedHistory :+ newBlock).takeRight(constants.H)

    HistoricalState(
      beta = HistoricalBetaContainer(
        history = newHistory,
        mmr = newMmr
      )
    )

  /**
   * Append a new peak to the MMR following the mountain range rules.
   * Each peak represents a perfect binary tree of size 2^i where i is the index.
   * When merging, we combine adjacent peaks of the same size and push the result up.
   */
  def appendToMmr(previousMmr: HistoricalMmr, newPeak: Hash): HistoricalMmr =
    val peaks = previousMmr.peaks.toBuffer
    appendRecursive(peaks, newPeak, 0)
    HistoricalMmr(peaks.toList)

  /**
   * Recursively append a peak to the MMR, merging when necessary.
   */
  private def appendRecursive(
    peaks: scala.collection.mutable.Buffer[Option[Hash]],
    data: Hash,
    index: Int
  ): Unit =
    if index >= peaks.size then
      // Beyond current peaks size, add new peak
      peaks += Some(data)
    else if peaks(index).isDefined then
      // Current position has a peak, merge and go up
      val current = peaks(index).get
      peaks(index) = None
      // Concatenate and hash the peaks
      val merged = Hashing.keccak256(JamBytes(current.bytes) ++ JamBytes(data.bytes))
      appendRecursive(peaks, merged, index + 1)
    else
      // Current position is empty, place peak here
      peaks(index) = Some(data)

  /**
   * Calculate the beefy root from MMR peaks using the mmrSuperPeak function.
   */
  def calculateBeefyRoot(mmr: HistoricalMmr): Hash =
    // Filter non-null peaks, keeping them in order
    val h = mmr.peaks.flatten
    mmrSuperPeak(h)

  /**
   * Calculate the super peak of an MMR.
   *
   * Algorithm:
   * - Empty list: return zero hash
   * - Single peak: return that peak
   * - Multiple peaks: keccak($peak || superpeak(h[...len-1]) || h[len-1])
   */
  def mmrSuperPeak(h: List[Hash]): Hash =
    h match
      case Nil => ZeroHash
      case single :: Nil => single
      case _ =>
        val rest = h.dropRight(1)
        val last = h.last
        val superPeakOfRest = mmrSuperPeak(rest)
        keccakHashWithPrefix(PeakPrefix, superPeakOfRest, last)

  /**
   * Keccak-256 hash with prefix: keccak(prefix || left || right)
   */
  private def keccakHashWithPrefix(prefix: Array[Byte], left: Hash, right: Hash): Hash =
    Hashing.keccak256(prefix ++ left.bytes ++ right.bytes)
