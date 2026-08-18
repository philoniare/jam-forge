package io.forge.jam.protocol.statistics

import cats.syntax.all.*
import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.StatisticsBridge
import io.forge.jam.protocol.statistics.StatisticsTypes.*
import scodec.bits.ByteVector

/**
 * Statistics State Transition Function.
 *
 * Tracks validator performance statistics including blocks authored,
 * tickets submitted, preimages, guarantees, and assurances.
 *
 * On epoch transitions, stats are rotated: current becomes last, and current is reset.
 */
object StatisticsTransition:

  /**
   * Execute the Statistics STF using unified JamState.
   *
   * Reads: statistics (current, last), tau, validators.current (kappa)
   * Writes: statistics (current, last)
   *
   * @param input The statistics input containing slot, author index, and extrinsic data.
   * @param state The unified JamState.
   * @param config The chain configuration.
   * @return Tuple of (updated JamState, optional StatOutput).
   */
  def stfView(
    input: StatInput,
    view: TrieBackedJamState
  ): Option[StatOutput] =
    val preState = StatisticsBridge.extract(view)
    val (postState, output) = stfInternal(input, preState, view.config)
    StatisticsBridge.apply(view, postState)
    output

  /**
   * Internal Statistics STF implementation using StatState.
   *
   * @param input The statistics input containing slot, author index, and extrinsic data.
   * @param preState The pre-transition state.
   * @param config The chain configuration.
   * @return Tuple of (post-transition state, optional output).
   */
  def stfInternal(input: StatInput, preState: StatState, config: ChainConfig): (StatState, Option[StatOutput]) =
    // Calculate epochs for pre and post states
    val preEpoch = preState.slot / config.epochLength
    val postEpoch = input.slot / config.epochLength

    // Handle epoch transition: rotate stats
    val (baseStats, lastStats) = if postEpoch > preEpoch then
      // Current becomes last, reset current
      (List.fill(config.validatorCount)(StatCount.zero), preState.valsCurrStats)
    else
      (preState.valsCurrStats, preState.valsLastStats)

    val statsArr: Array[StatCount] = baseStats.toArray

    // Update author's stats
    val authorIdx = input.authorIndex.toInt
    if authorIdx >= 0 && authorIdx < statsArr.length then
      val a = statsArr(authorIdx)
      statsArr(authorIdx) = a.copy(
        blocks = a.blocks + 1,
        tickets = a.tickets + input.extrinsic.tickets.size,
        preImages = a.preImages + input.extrinsic.preimages.size,
        preImagesSize = a.preImagesSize + input.extrinsic.preimages.map(_.blob.length).sum
      )

    // For each guarantee, determine which validator set to use based on epoch
    val reporters: Set[ByteVector] = input.extrinsic.guarantees.flatMap { guarantee =>
      val guaranteeEpoch = guarantee.slot.value.toLong / config.epochLength
      // Use previous validators if guarantee is from a previous epoch, otherwise current
      val validatorSet = if guaranteeEpoch < postEpoch && preState.prevValidators.nonEmpty then
        preState.prevValidators
      else
        preState.currValidators

      // Extract Ed25519 keys for each signer
      guarantee.signatures.flatMap { sig =>
        val idx = sig.validatorIndex.toInt
        if idx >= 0 && idx < validatorSet.size then
          Some(sig.validatorIndex.toInt).map(i => validatorSet(i).ed25519.toByteVector)
        else
          None
      }
    }.toSet

    // Update guarantees - for each validator v, check if their Ed25519 key is in reporters
    for ((validator, idx) <- preState.currValidators.zipWithIndex) do
      if idx < statsArr.length && reporters.contains(validator.ed25519.toByteVector) then
        val s = statsArr(idx)
        statsArr(idx) = s.copy(guarantees = s.guarantees + 1)

    // Update assurances
    for (assurance <- input.extrinsic.assurances) do
      val idx = assurance.validatorIndex.toInt
      if idx >= 0 && idx < statsArr.length then
        val s = statsArr(idx)
        statsArr(idx) = s.copy(assurances = s.assurances + 1)

    val postState = StatState(
      valsCurrStats = statsArr.toList,
      valsLastStats = lastStats,
      slot = preState.slot,
      currValidators = preState.currValidators,
      prevValidators = preState.prevValidators
    )

    (postState, None)
