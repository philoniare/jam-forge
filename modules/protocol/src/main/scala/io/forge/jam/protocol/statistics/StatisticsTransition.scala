package io.forge.jam.protocol.statistics

import cats.syntax.all.*
import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.statistics.StatisticsTypes.*
import io.forge.jam.protocol.state.JamState
import monocle.syntax.all.*
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
  def stf(input: StatInput, state: JamState, config: ChainConfig): (JamState, Option[StatOutput]) =
    // Extract StatState using lens bundle
    val preState = JamState.StatisticsLenses.extract(state)

    val (postState, output) = stfInternal(input, preState, config)

    // Apply results back using lens bundle
    val updatedState = JamState.StatisticsLenses.apply(state, postState)

    (updatedState, output)

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

    // Helper to update stat at index
    def updateStatAt(stats: List[StatCount], idx: Int, f: StatCount => StatCount): List[StatCount] =
      stats.zipWithIndex.map { case (stat, i) => if i == idx then f(stat) else stat }

    // Update author's stats
    val authorIdx = input.authorIndex.toInt
    val afterAuthor = updateStatAt(
      baseStats,
      authorIdx,
      stat =>
        stat.copy(
          blocks = stat.blocks + 1,
          tickets = stat.tickets + input.extrinsic.tickets.size,
          preImages = stat.preImages + input.extrinsic.preimages.size,
          preImagesSize = stat.preImagesSize + input.extrinsic.preimages.map(_.blob.length).sum
        )
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
    val afterGuarantees = preState.currValidators.zipWithIndex.foldLeft(afterAuthor) {
      case (stats, (validator, idx)) =>
        if reporters.contains(validator.ed25519.toByteVector) then
          updateStatAt(stats, idx, stat => stat.copy(guarantees = stat.guarantees + 1))
        else
          stats
    }

    // Update assurances using foldLeft
    val afterAssurances = input.extrinsic.assurances.foldLeft(afterGuarantees) { (stats, assurance) =>
      val idx = assurance.validatorIndex.toInt
      updateStatAt(stats, idx, stat => stat.copy(assurances = stat.assurances + 1))
    }

    val postState = StatState(
      valsCurrStats = afterAssurances,
      valsLastStats = lastStats,
      slot = preState.slot,
      currValidators = preState.currValidators,
      prevValidators = preState.prevValidators
    )

    (postState, None)
