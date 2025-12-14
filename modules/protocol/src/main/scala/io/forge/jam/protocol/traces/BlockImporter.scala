package io.forge.jam.protocol.traces

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, GuaranteeExtrinsic, Preimage}
import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment}
import io.forge.jam.core.types.history.{HistoricalBetaContainer, HistoricalBeta, ReportedWorkPackage}
import io.forge.jam.core.types.service.ServiceAccount as CoreServiceAccount
import io.forge.jam.protocol.safrole.SafroleTransition
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.assurance.AssuranceTransition
import io.forge.jam.protocol.assurance.AssuranceTypes.*
import io.forge.jam.protocol.report.ReportTransition
import io.forge.jam.protocol.report.ReportTypes
import io.forge.jam.protocol.report.ReportTypes.*
import io.forge.jam.protocol.accumulation.{
  AccumulationTransition,
  AccumulationState,
  AccumulationInput,
  AccumulationOutput,
  AccumulationServiceItem,
  AccumulationServiceData,
  Privileges,
  AlwaysAccItem,
  ServiceStatisticsEntry as AccServiceStatisticsEntry,
  ServiceActivityRecord as AccServiceActivityRecord
}
import io.forge.jam.protocol.history.HistoryTransition
import io.forge.jam.protocol.history.HistoryTypes.*
import io.forge.jam.protocol.authorization.AuthorizationTransition
import io.forge.jam.protocol.authorization.AuthorizationTypes.*
import io.forge.jam.protocol.preimage.PreimageTransition
import io.forge.jam.protocol.preimage.PreimageTypes.*
import io.forge.jam.protocol.statistics.StatisticsTransition
import io.forge.jam.protocol.statistics.StatisticsTypes.*
import io.forge.jam.protocol.dispute.DisputeTransition
import io.forge.jam.protocol.dispute.DisputeTypes.*

/**
 * Result of a block import operation.
 */
sealed trait ImportResult

object ImportResult:
  final case class Success(
    postState: RawState,
    computedFullState: FullJamState,
    safroleState: Option[SafroleState] = None
  ) extends ImportResult

  final case class Failure(
    error: ImportError,
    message: String = ""
  ) extends ImportResult

/**
 * Errors that can occur during block import.
 */
enum ImportError:
  case InvalidHeader
  case InvalidParent
  case InvalidSlot
  case InvalidStateRoot
  case SafroleError
  case AssuranceError
  case AuthorizationError
  case DisputeError
  case HistoryError
  case PreimageError
  case ReportError
  case StatisticsError
  case AccumulationError
  case UnknownError

/**
 * BlockImporter handles importing blocks and applying all state transitions.
 *
 * 1. Safrole - Block production and VRF validation (includes Disputes)
 * 2. Assurances - Process availability assurances
 * 3. Reports - Process work reports (guarantees)
 * 4. Accumulation - Execute PVM accumulation
 * 5. History - Update recent blocks history
 * 6. Authorizations - Update authorization pools
 * 7. Preimages - Handle preimage provisioning
 * 8. Statistics - Update chain statistics
 */
class BlockImporter(config: ChainConfig = ChainConfig.TINY):

  /**
   * Imports a block and applies all state transitions.
   * Returns the computed post-state with updated state root.
   *
   * @param block The block to import
   * @param preState The state before the block (raw keyvals)
   * @return ImportResult indicating success with new state or failure with error
   */
  def importBlock(block: Block, preState: RawState): ImportResult =
    try
      // Step 1: Decode full pre-state from keyvals
      val fullPreState = FullJamState.fromKeyvals(preState.keyvals, config)

      // Step 2: Run Safrole STF (includes Disputes)
      val safroleInput = InputExtractor.extractSafroleInput(block, fullPreState.toSafroleState())
      val safrolePreState = fullPreState.toSafroleState()
      val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, safrolePreState, config)

      if safroleOutput.err.isDefined then
        return ImportResult.Failure(
          ImportError.SafroleError,
          s"Safrole STF error: ${safroleOutput.err.get}"
        )

      // Step 3: Run Disputes STF
      val disputeInput = InputExtractor.extractDisputeInput(block)
      val disputePreState = DisputeState(
        psi = fullPreState.judgements,
        rho = fullPreState.reports,
        tau = safrolePostState.tau,
        kappa = safrolePostState.kappa,
        lambda = safrolePostState.lambda
      )
      val (disputePostState, disputeOutput) = DisputeTransition.stf(disputeInput, disputePreState, config)

      if disputeOutput.err.isDefined then
        return ImportResult.Failure(
          ImportError.DisputeError,
          s"Dispute STF error: ${disputeOutput.err.get}"
        )

      // Step 4: Run Assurances STF
      val assuranceInput = InputExtractor.extractAssuranceInput(block, fullPreState)
      val assurancePreState = AssuranceState(
        availAssignments = fullPreState.reports,
        currValidators = safrolePostState.kappa
      )
      val (assurancePostState, assuranceOutput) = AssuranceTransition.stf(assuranceInput, assurancePreState, config)

      if assuranceOutput.err.isDefined then
        return ImportResult.Failure(
          ImportError.AssuranceError,
          s"Assurance STF error: ${assuranceOutput.err.get}"
        )

      // Get available reports from assurances
      val availableReports = assuranceOutput.ok.map(_.reported).getOrElse(List.empty)

      // Step 4: Run Reports STF
      val updatedRecentHistory = updateRecentHistoryPartial(
        fullPreState.recentHistory,
        block.header.parentStateRoot
      )
      val reportInput = ReportInput(
        guarantees = block.extrinsic.guarantees,
        slot = block.header.slot.value.toLong
      )
      val reportPreState = buildReportState(fullPreState, safrolePostState, assurancePostState, updatedRecentHistory)
      val (reportPostState, reportOutput) = ReportTransition.stf(reportInput, reportPreState, config)

      if reportOutput.err.isDefined then
        return ImportResult.Failure(
          ImportError.ReportError,
          s"Report STF error: ${reportOutput.err.get}"
        )

      // Step 5: Run Accumulation STF
      val accumulationInput = InputExtractor.extractAccumulationInput(availableReports, block.header.slot.value.toLong)
      val baseAccumulationState = fullPreState.toAccumulationState(config)
      // Use entropy from Safrole post-state
      val postSafroleEntropy = if safrolePostState.eta.nonEmpty then
        JamBytes(safrolePostState.eta.head.bytes)
      else
        JamBytes.zeros(32)

      val accumulationPreState = baseAccumulationState.copy(
        entropy = postSafroleEntropy
      )
      val (accumulationPostState, accumulationOutput) = AccumulationTransition.stf(
        accumulationInput,
        accumulationPreState,
        config
      )

      val accumulateRoot = accumulationOutput.ok

      // Step 6: Run History STF
      val historyInput = InputExtractor.extractHistoryInput(block, Hash(accumulateRoot.toArray))
      val historyPreState = fullPreState.toHistoryState()
      val historyPostState = HistoryTransition.stf(historyInput, historyPreState, config)

      // Step 7: Run Authorization STF
      val authInput = InputExtractor.extractAuthInput(block, fullPreState)
      val authPreState = fullPreState.toAuthState()
      val authPostState = AuthorizationTransition.stf(authInput, authPreState, config)

      // Step 8: Run Preimages STF
      val preimageInput = InputExtractor.extractPreimageInput(block, block.header.slot.value.toLong)
      val preimagePreState = fullPreState.toPreimageState()
      val (preimagePostState, preimageOutput) = PreimageTransition.stf(preimageInput, preimagePreState)

      // Step 9: Run Statistics STF
      val statInput = InputExtractor.extractStatInput(block, fullPreState)
      val statPreState = fullPreState.toStatState()
      val (statPostState, _) = StatisticsTransition.stf(statInput, statPreState, config)

      // Step 10: Compute final core statistics (combines guarantees, available reports, assurances)
      val finalCoreStats = computeFinalCoreStatistics(
        guaranteeStats = reportPostState.coresStatistics,
        availableReports = availableReports,
        assurances = block.extrinsic.assurances,
        maxCores = config.coresCount
      )

      // Step 11: Compute final service statistics (fresh each block)
      val finalServiceStats = computeFinalServiceStatistics(
        guarantees = block.extrinsic.guarantees,
        preimages = block.extrinsic.preimages,
        accumulationStats = Map.empty // TODO: Get from accumulation output
      )

      // Step 12: Merge all post-states into unified FullJamState
      val mergedState = mergePostStates(
        preState = fullPreState,
        safrolePost = safrolePostState,
        disputePost = disputePostState,
        assurancePost = assurancePostState,
        reportPost = reportPostState,
        accumulationPost = accumulationPostState,
        accumulationOutput = accumulationOutput,
        historyPost = historyPostState,
        authPost = authPostState,
        preimagePost = preimagePostState,
        preimageOutput = preimageOutput,
        statPost = statPostState,
        finalCoreStats = finalCoreStats,
        finalServiceStats = finalServiceStats
      )

      // Step 13: Encode merged state back to keyvals
      val postKeyvals = StateEncoder.encodeFullState(mergedState, config)

      // Step 14: Compute state root via Merkle trie
      val stateRoot = StateMerklization.stateMerklize(postKeyvals)

      val rawPostState = RawState(stateRoot, postKeyvals)

      ImportResult.Success(rawPostState, mergedState, Some(safrolePostState))
    catch
      case e: Exception =>
        e.printStackTrace()
        ImportResult.Failure(ImportError.UnknownError, e.getMessage)

  /**
   * Compute final core statistics by combining:
   * 1. Guarantee-based stats (bundleSize, gasUsed, extrinsicCount, etc.) from Reports STF
   * 2. dataSize from available reports
   * 3. assuranceCount/popularity from assurance extrinsics
   */
  private def computeFinalCoreStatistics(
    guaranteeStats: List[CoreStatisticsRecord],
    availableReports: List[WorkReport],
    assurances: List[AssuranceExtrinsic],
    maxCores: Int
  ): List[CoreStatisticsRecord] =
    // Start with guarantee-based stats (or fresh zeros if empty)
    val coreStats = if guaranteeStats.size >= maxCores then
      guaranteeStats.toArray
    else
      val arr = guaranteeStats.toArray
      Array.fill(maxCores)(CoreStatisticsRecord()) // Fill with zeros

    // Add dataSize from available reports
    // dataSize = package.length + segmentsSize
    // segmentsSize = segmentSize * ceil((segmentCount * 65) / 64)
    val segmentSize = 4104L // Default segment size for tiny config
    for report <- availableReports do
      val coreIndex = report.coreIndex.toInt
      if coreIndex >= 0 && coreIndex < coreStats.length then
        val packageLength = report.packageSpec.length.toLong
        // segmentCount comes from work results exports
        val segmentCount = report.results.map(_.refineLoad.exports.toLong).sum
        val segmentsSize = segmentSize * ((segmentCount * 65 + 63) / 64)
        val dataSize = packageLength + segmentsSize

        val current = coreStats(coreIndex)
        coreStats(coreIndex) = current.copy(
          daLoad = current.daLoad + dataSize
        )

    // Add assuranceCount/popularity from assurances
    // Each assurance has a bitfield indicating which cores the validator attests to
    for assurance <- assurances do
      val bitfield = assurance.bitfield.toArray
      for coreIndex <- 0 until maxCores do
        // Check if bit at coreIndex is set in the bitfield
        val byteIndex = coreIndex / 8
        val bitIndex = coreIndex % 8
        if byteIndex < bitfield.length then
          val attested = (bitfield(byteIndex).toInt & (1 << bitIndex)) != 0
          if attested && coreIndex < coreStats.length then
            val current = coreStats(coreIndex)
            coreStats(coreIndex) = current.copy(
              popularity = current.popularity + 1
            )

    coreStats.toList

  /**
   * Compute fresh service statistics by combining:
   * 1. Work reports from guarantees (refinementCount, gasUsed, imports, exports, extrinsicCount, extrinsicSize)
   * 2. Preimages (providedCount, providedSize)
   * 3. Accumulation results (accumulateCount, accumulateGasUsed)
   */
  private def computeFinalServiceStatistics(
    guarantees: List[GuaranteeExtrinsic],
    preimages: List[Preimage],
    accumulationStats: Map[Long, (Long, Int)] // serviceId -> (gasUsed, workItemCount)
  ): List[ReportTypes.ServiceStatisticsEntry] =
    // Collect all service IDs from all sources
    val serviceIds = scala.collection.mutable.Set[Long]()

    // From guarantees
    for guarantee <- guarantees do
      for result <- guarantee.report.results do
        serviceIds.add(result.serviceId.value.toLong)
    // From preimages
    for preimage <- preimages do
      serviceIds.add(preimage.requester.value.toLong)
    // From accumulation
    serviceIds ++= accumulationStats.keys

    // Initialize fresh stats for each service
    val statsMap = scala.collection.mutable.Map[Long, ReportTypes.ServiceActivityRecord]()
    for serviceId <- serviceIds do
      statsMap(serviceId) = ReportTypes.ServiceActivityRecord()

    // Update from guarantees (work reports)
    for guarantee <- guarantees do
      for result <- guarantee.report.results do
        val serviceId = result.serviceId.value.toLong
        val current = statsMap.getOrElse(serviceId, ReportTypes.ServiceActivityRecord())
        val refineLoad = result.refineLoad
        statsMap(serviceId) = current.copy(
          refinementCount = current.refinementCount + 1L,
          refinementGasUsed = current.refinementGasUsed + refineLoad.gasUsed.toLong,
          imports = current.imports + refineLoad.imports.toLong,
          exports = current.exports + refineLoad.exports.toLong,
          extrinsicCount = current.extrinsicCount + refineLoad.extrinsicCount.toLong,
          extrinsicSize = current.extrinsicSize + refineLoad.extrinsicSize.toLong
        )

    // Return sorted list by service ID
    statsMap.toList.sortBy(_._1).map {
      case (id, record) =>
        ReportTypes.ServiceStatisticsEntry(id = id, record = record)
    }

  /**
   * Update recent history's last item with the parent state root.
   * This MUST be called BEFORE validating guarantees
   * The last history entry was created with stateRoot=0, and we now set it to the
   * actual computed state root of the parent block.
   */
  private def updateRecentHistoryPartial(
    recentHistory: HistoricalBetaContainer,
    parentStateRoot: Hash
  ): HistoricalBetaContainer =
    if recentHistory.history.isEmpty then
      recentHistory
    else
      val history = recentHistory.history.toArray
      val lastItem = history.last
      history(history.length - 1) = lastItem.copy(stateRoot = parentStateRoot)
      recentHistory.copy(history = history.toList)

  /**
   * Build ReportState from FullJamState, Safrole post-state, and Assurance post-state.
   * Uses availability assignments from Assurance post-state (which has cleared cores
   * for reports that became available).
   */
  private def buildReportState(
    fullPreState: FullJamState,
    safrolePost: SafroleState,
    assurancePost: AssuranceState,
    updatedRecentHistory: HistoricalBetaContainer
  ): ReportState =
    // Convert service accounts to ServiceAccount for ReportState
    val serviceAccounts = fullPreState.serviceAccounts.map { item =>
      CoreServiceAccount(
        id = item.id,
        data = io.forge.jam.core.types.service.ServiceData(item.data.service)
      )
    }

    ReportState(
      // Use availability assignments from Assurance post-state (cores cleared after reports available)
      availAssignments = assurancePost.availAssignments,
      currValidators = safrolePost.kappa,
      prevValidators = safrolePost.lambda,
      entropy = safrolePost.eta,
      offenders = fullPreState.judgements.offenders.map(k => Hash(k.bytes)),
      recentBlocks = updatedRecentHistory,
      authPools = fullPreState.authPools,
      accounts = serviceAccounts,
      coresStatistics = fullPreState.coreStatistics,
      servicesStatistics = fullPreState.serviceStatistics
    )

  /**
   * Merge all STF post-states into a unified FullJamState.
   */
  private def mergePostStates(
    preState: FullJamState,
    safrolePost: SafroleState,
    disputePost: DisputeState,
    assurancePost: AssuranceState,
    reportPost: ReportState,
    accumulationPost: AccumulationState,
    accumulationOutput: AccumulationOutput,
    historyPost: HistoricalState,
    authPost: AuthState,
    preimagePost: PreimageState,
    preimageOutput: PreimageOutput,
    statPost: StatState,
    finalCoreStats: List[CoreStatisticsRecord],
    finalServiceStats: List[ReportTypes.ServiceStatisticsEntry]
  ): FullJamState =
    // Use judgements from Disputes STF
    val judgements = disputePost.psi

    // Merge service accounts from accumulation and preimage
    val serviceAccounts = mergeServiceAccounts(
      accumulationPost.accounts,
      preimagePost.accounts
    )

    FullJamState(
      // From Safrole
      timeslot = safrolePost.tau,
      entropyPool = safrolePost.eta,
      currentValidators = safrolePost.kappa,
      previousValidators = safrolePost.lambda,
      validatorQueue = safrolePost.iota,
      safroleGammaK = safrolePost.gammaK,
      safroleGammaZ = safrolePost.gammaZ,
      safroleGammaS = safrolePost.gammaS,
      safroleGammaA = safrolePost.gammaA,

      // From Authorization
      authPools = authPost.authPools,
      authQueues = authPost.authQueues,

      // From History
      recentHistory = historyPost.beta,

      // From Reports STF (updates availAssignments with new guarantees)
      reports = reportPost.availAssignments,

      // From Safrole (Disputes)
      judgements = judgements,

      // From Accumulation
      privilegedServices = accumulationPost.privileges,
      accumulationQueue = accumulationPost.readyQueue,
      accumulationHistory = accumulationPost.accumulated,

      // Service accounts merged
      serviceAccounts = serviceAccounts,
      // Service statistics computed fresh each block
      serviceStatistics = finalServiceStats,

      // From Statistics
      activityStatsCurrent = statPost.valsCurrStats,
      activityStatsLast = statPost.valsLastStats,

      // Core statistics computed from guarantees, available reports, and assurances
      coreStatistics = finalCoreStats,

      // Post-dispute offenders
      postOffenders = safrolePost.postOffenders,

      // Raw keyvals for pass-through
      otherKeyvals = preState.otherKeyvals
    )

  /**
   * Merge service accounts from accumulation and preimage states.
   */
  private def mergeServiceAccounts(
    accumulationAccounts: List[AccumulationServiceItem],
    preimageAccounts: List[PreimageAccount]
  ): List[AccumulationServiceItem] =
    // Build map from accumulation accounts
    val accountMap = accumulationAccounts.map(a => a.id -> a).toMap.to(scala.collection.mutable.Map)

    // Merge preimage data into accounts
    for preimageAccount <- preimageAccounts do
      accountMap.get(preimageAccount.id).foreach { existing =>
        // Update preimages and lookup metadata
        val updatedData = existing.data.copy(
          preimages = preimageAccount.data.preimages.map(p =>
            io.forge.jam.core.types.preimage.PreimageHash(p.hash, p.blob)
          ),
          preimagesStatus = preimageAccount.data.lookupMeta.map { meta =>
            io.forge.jam.protocol.accumulation.PreimagesStatusMapEntry(
              hash = meta.key.hash,
              status = meta.value
            )
          }
        )
        accountMap(preimageAccount.id) = AccumulationServiceItem(preimageAccount.id, updatedData)
      }

    accountMap.values.toList.sortBy(_.id)

  /**
   * Imports a block and returns just the computed SafroleState for comparison.
   * This is useful for trace testing where we want to compare typed state.
   */
  def importBlockForSafrole(block: Block, preState: RawState): (Option[SafroleState], Option[String]) =
    try
      importBlock(block, preState) match
        case ImportResult.Success(_, _, safroleState) => (safroleState, None)
        case ImportResult.Failure(_, message) => (None, Some(message))
    catch
      case e: Exception =>
        (None, Some(s"Exception: ${e.getMessage}"))

  /**
   * Validates that a block import produces the expected post-state.
   * Used for testing against trace vectors.
   */
  def validateBlockImport(
    block: Block,
    preState: RawState,
    expectedPostState: RawState
  ): Boolean =
    importBlock(block, preState) match
      case ImportResult.Success(actualPostState, _, _) =>
        // Compare computed post-state root with expected
        actualPostState.stateRoot == expectedPostState.stateRoot
      case ImportResult.Failure(_, _) =>
        false

/**
 * Extracts STF inputs from block and state.
 */
object InputExtractor:
  import io.forge.jam.core.types.tickets.TicketEnvelope

  /**
   * Extract SafroleInput from block and state.
   * The entropy source in the header is a VRF signature from which we extract the output.
   */
  def extractSafroleInput(block: Block, state: SafroleState): SafroleInput =
    val header = block.header
    val tickets = block.extrinsic.tickets

    // The header.entropySource is a 96-byte Bandersnatch IETF VRF signature.
    // For now, we extract the first 32 bytes as a simple approximation.
    // In production, this would use native crypto to extract the VRF output.
    val entropyBytes = header.entropySource.toArray
    val vrfOutput = if entropyBytes.length >= 32 then
      Hash(entropyBytes.take(32))
    else
      Hash(entropyBytes.padTo(32, 0.toByte))

    SafroleInput(
      slot = header.slot.value.toLong,
      entropy = vrfOutput,
      extrinsic = tickets
    )

  /**
   * Extract SafroleInput from block and FullJamState.
   */
  def extractSafroleInput(block: Block, state: FullJamState): SafroleInput =
    extractSafroleInput(block, state.toSafroleState())

  /**
   * Extract DisputeInput from block.
   */
  def extractDisputeInput(block: Block): DisputeInput =
    DisputeInput(disputes = block.extrinsic.disputes)

  /**
   * Extract AssuranceInput from block and state.
   */
  def extractAssuranceInput(block: Block, state: FullJamState): AssuranceInput =
    AssuranceInput(
      assurances = block.extrinsic.assurances,
      slot = block.header.slot.value.toLong,
      parent = block.header.parent
    )

  /**
   * Extract AccumulationInput from available reports and slot.
   */
  def extractAccumulationInput(availableReports: List[WorkReport], slot: Long): AccumulationInput =
    AccumulationInput(
      slot = slot,
      reports = availableReports
    )

  /**
   * Extract HistoricalInput from block and accumulate root.
   */
  def extractHistoryInput(block: Block, accumulateRoot: Hash): HistoricalInput =
    import io.forge.jam.core.codec.encode
    val headerHash = Hashing.blake2b256(block.header.encode.toArray)

    // Build work packages from guarantees
    val workPackages = block.extrinsic.guarantees.map { guarantee =>
      ReportedWorkPackage(
        hash = guarantee.report.packageSpec.hash,
        exportsRoot = guarantee.report.packageSpec.exportsRoot
      )
    }

    HistoricalInput(
      headerHash = headerHash,
      parentStateRoot = block.header.parentStateRoot,
      accumulateRoot = accumulateRoot,
      workPackages = workPackages
    )

  /**
   * Extract AuthInput from block and state.
   */
  def extractAuthInput(block: Block, state: FullJamState): AuthInput =
    // Consumed authorizations come from guarantees
    val auths = block.extrinsic.guarantees.map { guarantee =>
      Auth(
        core = guarantee.report.coreIndex,
        authHash = guarantee.report.authorizerHash
      )
    }
    AuthInput(
      slot = block.header.slot.value.toLong,
      auths = auths
    )

  /**
   * Extract PreimageInput from block.
   */
  def extractPreimageInput(block: Block, slot: Long): PreimageInput =
    PreimageInput(
      preimages = block.extrinsic.preimages,
      slot = slot
    )

  /**
   * Extract StatInput from block and state.
   */
  def extractStatInput(block: Block, state: FullJamState): StatInput =
    StatInput(
      slot = block.header.slot.value.toLong,
      authorIndex = block.header.authorIndex.toInt.toLong,
      extrinsic = StatExtrinsic(
        tickets = block.extrinsic.tickets,
        preimages = block.extrinsic.preimages,
        guarantees = block.extrinsic.guarantees,
        assurances = block.extrinsic.assurances,
        disputes = block.extrinsic.disputes
      )
    )

/**
 * Encoder for converting typed state structures back to raw keyvals.
 */
object StateEncoder:
  /**
   * Encode the full FullJamState back to keyvals.
   * This encodes all state components according to the Gray Paper state layout.
   */
  def encodeFullState(state: FullJamState, config: ChainConfig = ChainConfig.TINY): List[KeyValue] =
    state.toKeyvals(config)
