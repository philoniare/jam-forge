package io.forge.jam.protocol.statistics

import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.statistics.StatisticsTypes.*

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
   * Execute the Statistics STF.
   *
   * @param input The statistics input containing slot, author index, and extrinsic data.
   * @param preState The pre-transition state.
   * @param config The chain configuration.
   * @return Tuple of (post-transition state, optional output).
   */
  def stf(input: StatInput, preState: StatState, config: ChainConfig): (StatState, Option[StatOutput]) =
    // Calculate epochs for pre and post states
    val preEpoch = preState.slot / config.epochLength
    val postEpoch = input.slot / config.epochLength

    // Start with mutable copies of stats
    var currStats = preState.valsCurrStats.toArray.map(_.copy())

    // Handle epoch transition: rotate stats
    val lastStats = if postEpoch > preEpoch then
      // Current becomes last, reset current
      val newLastStats = currStats.toList
      currStats = Array.fill(config.validatorCount)(StatCount.zero)
      newLastStats
    else
      preState.valsLastStats

    // Update author's stats
    val authorIdx = input.authorIndex.toInt
    currStats(authorIdx) = currStats(authorIdx).copy(
      blocks = currStats(authorIdx).blocks + 1,
      tickets = currStats(authorIdx).tickets + input.extrinsic.tickets.size,
      preImages = currStats(authorIdx).preImages + input.extrinsic.preimages.size,
      preImagesSize = currStats(authorIdx).preImagesSize + input.extrinsic.preimages.map(_.blob.length).sum
    )

    // Update guarantees - each unique validator who signed any guarantee gets +1 credit per block
    val reporters = scala.collection.mutable.Set[Int]()
    input.extrinsic.guarantees.foreach { guarantee =>
      guarantee.signatures.foreach { sig =>
        reporters.add(sig.validatorIndex.toInt)
      }
    }
    reporters.foreach { validatorIndex =>
      currStats(validatorIndex) = currStats(validatorIndex).copy(
        guarantees = currStats(validatorIndex).guarantees + 1
      )
    }

    // Update assurances
    input.extrinsic.assurances.foreach { assurance =>
      val idx = assurance.validatorIndex.toInt
      currStats(idx) = currStats(idx).copy(
        assurances = currStats(idx).assurances + 1
      )
    }

    val postState = StatState(
      valsCurrStats = currStats.toList,
      valsLastStats = lastStats,
      slot = preState.slot, // Note: slot in state doesn't change based on Kotlin implementation
      currValidators = preState.currValidators
    )

    (postState, None)
