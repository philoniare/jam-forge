package io.forge.jam.protocol.statistics

import io.forge.jam.core.{ChainConfig, constants}
import io.forge.jam.core.primitives.ServiceId
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, Preimage}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.protocol.accumulation.ServiceAccumulation
import io.forge.jam.protocol.report.ReportTypes
import io.forge.jam.protocol.report.ReportTypes.CoreStatisticsRecord
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.StatisticsBridge
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
   * Execute the Statistics STF using unified JamState.
   *
   * Reads: statistics (current, last), tau, validators.current (kappa)
   * Writes: statistics (current, last)
   *
   * @param input The statistics input containing slot, author index, and extrinsic data.
   */
  def stfView(
    input: StatInput,
    view: TrieBackedJamState,
    priorSlot: Long
  ): Option[StatOutput] =
    val preState = StatisticsBridge.extract(view).copy(slot = priorSlot)
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
    val preEpoch = preState.slot / config.epochLength
    val postEpoch = input.slot / config.epochLength

    // pi_dagger: assurances applied to the prior accumulator.
    val daggerArr: Array[StatCount] = preState.valsCurrStats.toArray
    for (assurance <- input.extrinsic.assurances) do
      val idx = assurance.validatorIndex.toInt
      if idx >= 0 && idx < daggerArr.length then
        val s = daggerArr(idx)
        daggerArr(idx) = s.copy(assurances = s.assurances + 1)
    val dagger = daggerArr.toList

    // pi_ddagger / pi_L'
    val (baseStats, lastStats) =
      if postEpoch > preEpoch then
        (List.fill(preState.currValidators.size)(StatCount.zero), dagger)
      else
        (dagger, preState.valsLastStats)

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
    val reporters: Set[io.forge.jam.core.primitives.Ed25519PublicKey] = input.extrinsic.guarantees.flatMap { guarantee =>
      val guaranteeEpoch = guarantee.slot.toLong / config.epochLength
      // Use previous validators if guarantee is from a previous epoch, otherwise current
      val validatorSet = if guaranteeEpoch < postEpoch && preState.prevValidators.nonEmpty then
        preState.prevValidators
      else
        preState.currValidators

      // Extract Ed25519 keys for each signer
      guarantee.signatures.flatMap { sig =>
        val idx = sig.validatorIndex.toInt
        if idx >= 0 && idx < validatorSet.size then
          Some(sig.validatorIndex.toInt).map(i => validatorSet(i).ed25519)
        else
          None
      }
    }.toSet

    // Update guarantees - for each validator v, check if their Ed25519 key is in reporters
    for ((validator, idx) <- preState.currValidators.zipWithIndex) do
      if idx < statsArr.length && reporters.contains(validator.ed25519) then
        val s = statsArr(idx)
        statsArr(idx) = s.copy(guarantees = s.guarantees + 1)

    val postState = StatState(
      valsCurrStats = statsArr.toList,
      valsLastStats = lastStats,
      slot = preState.slot,
      currValidators = preState.currValidators,
      prevValidators = preState.prevValidators
    )

    (postState, None)

  def completeActivityStatistics(
      view: TrieBackedJamState,
      block: Block,
      availableReports: List[WorkReport],
      accumulationStats: Map[ServiceId, ServiceAccumulation],
      accumulationTransferCounts: Map[ServiceId, Int]
  ): Unit =
    // Reports left the guarantee-derived core and service statistics in the view; add the rest to them.
    val coreStats = finalCoreStatistics(
      guaranteeStats = view.cores.statistics,
      availableReports = availableReports,
      assurances = block.extrinsic.assurances,
      maxCores = view.config.coresCount
    )
    val serviceStats = finalServiceStatistics(
      guaranteeStats = view.serviceStatistics,
      preimages = block.extrinsic.preimages,
      accumulationStats = accumulationStats,
      accumulationTransferCounts = accumulationTransferCounts
    )
    view.cores.statistics = coreStats
    view.serviceStatistics = serviceStats

  /** Compute final core statistics by combining:
    *   1. Guarantee-based stats (bundleSize, gasUsed, extrinsicCount, etc.),
    *      as the Reports STF computed them from the block's guarantees
    *   2. dataSize from available reports
    *   3. assuranceCount/popularity from assurance extrinsics
    */
  def finalCoreStatistics(
      guaranteeStats: List[CoreStatisticsRecord],
      availableReports: List[WorkReport],
      assurances: List[AssuranceExtrinsic],
      maxCores: Int
  ): List[CoreStatisticsRecord] =
    val stats = guaranteeStats.toArray

    availableReports.foreach { report =>
      val coreIndex = report.coreIndex.toInt
      if coreIndex >= 0 && coreIndex < maxCores then
        val packageLength = report.packageSpec.length.toLong
        val segmentCount = report.packageSpec.exportsCount.toLong
        val segmentsSize =
          constants.Csegmentsize * constants.erasureExpandedPieces(segmentCount)
        val cur = stats(coreIndex)
        stats(coreIndex) =
          cur.copy(daLoad = cur.daLoad + packageLength + segmentsSize)
    }

    assurances.foreach { assurance =>
      val bitfield = assurance.bitfield.toArray
      val bitfieldLen = bitfield.length
      var byteIndex = 0
      while byteIndex < bitfieldLen do
        val b = bitfield(byteIndex).toInt & 0xff
        if b != 0 then
          var bit = 0
          while bit < 8 do
            if (b & (1 << bit)) != 0 then
              val coreIndex = byteIndex * 8 + bit
              if coreIndex < maxCores then
                val cur = stats(coreIndex)
                stats(coreIndex) = cur.copy(popularity = cur.popularity + 1)
            bit += 1
        byteIndex += 1
    }

    stats.toList

  /** Compute fresh service statistics by combining:
    *   1. Work reports from guarantees (refinementCount, gasUsed, imports,
    *      exports, extrinsicCount, extrinsicSize), as the Reports STF
    *      computed them
    *   2. Preimages (providedCount, providedSize)
    *   3. Accumulation results (accumulateCount, accumulateGasUsed)
    */
  def finalServiceStatistics(
      guaranteeStats: List[ReportTypes.ServiceStatisticsEntry],
      preimages: List[Preimage],
      accumulationStats: Map[ServiceId, ServiceAccumulation],
      accumulationTransferCounts: Map[ServiceId, Int]
  ): List[ReportTypes.ServiceStatisticsEntry] =
    // Collect all service IDs from all sources (immutable)
    val stats =
      scala.collection.mutable.LongMap.empty[ReportTypes.ServiceActivityRecord]
    def getOrEmpty(id: Long): ReportTypes.ServiceActivityRecord =
      stats.getOrElse(id, ReportTypes.ServiceActivityRecord())

    guaranteeStats.foreach { entry =>
      val cur = getOrEmpty(entry.id)
      val g = entry.record
      stats(entry.id) = cur.copy(
        refinementCount = cur.refinementCount + g.refinementCount,
        refinementGasUsed = cur.refinementGasUsed + g.refinementGasUsed,
        imports = cur.imports + g.imports,
        exports = cur.exports + g.exports,
        extrinsicCount = cur.extrinsicCount + g.extrinsicCount,
        extrinsicSize = cur.extrinsicSize + g.extrinsicSize
      )
    }

    preimages.foreach { p =>
      val serviceId = p.requester.toLong
      val cur = getOrEmpty(serviceId)
      stats(serviceId) = cur.copy(
        providedCount = cur.providedCount + 1,
        providedSize = cur.providedSize + p.blob.length.toLong
      )
    }

    accumulationStats.foreach { case (sid, done) =>
      val serviceId = sid.toLong
      val cur = getOrEmpty(serviceId)
      stats(serviceId) = cur.copy(
        accumulateCount = cur.accumulateCount + done.workItems.toLong,
        accumulateGasUsed = cur.accumulateGasUsed + done.gasUsed.toLong
      )
    }

    accumulationTransferCounts.foreach { case (sid, transfers) =>
      val serviceId = sid.toLong
      val cur = getOrEmpty(serviceId)
      stats(serviceId) = cur.copy(
        accumulateTransferCount = cur.accumulateTransferCount + transfers.toLong
      )
    }

    // Sorted-by-service-id list, materialised once at the end.
    stats.toList
      .sortBy(_._1)
      .map { case (id, record) =>
        ReportTypes.ServiceStatisticsEntry(id = id, record = record)
      }
