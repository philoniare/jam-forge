package io.forge.jam.protocol.traces

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.vrfs.BandersnatchWrapper
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, GuaranteeExtrinsic, Preimage}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.history.ReportedWorkPackage
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.assurance.AssuranceTypes.*
import io.forge.jam.protocol.report.ReportTypes
import io.forge.jam.protocol.report.ReportTypes.*
import io.forge.jam.protocol.accumulation.AccumulationInput
import io.forge.jam.protocol.history.HistoryTypes.*
import io.forge.jam.protocol.authorization.AuthorizationTypes.*
import io.forge.jam.protocol.preimage.PreimageTypes.*
import io.forge.jam.protocol.statistics.StatisticsTypes.*
import io.forge.jam.protocol.dispute.DisputeTypes.*
import io.forge.jam.protocol.pipeline.{BlockPipeline, PipelineError}
import io.forge.jam.protocol.state.JamState
import org.slf4j.LoggerFactory

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
 * Uses a unified JamState pipeline where state flows sequentially through all 9 STFs:
 * 1. Safrole - Block production and VRF validation
 * 2. Disputes - Process dispute verdicts
 * 3. Assurances - Process availability assurances
 * 4. Reports - Process work reports (guarantees)
 * 5. Accumulation - Execute PVM accumulation
 * 6. History - Update recent blocks history
 * 7. Authorizations - Update authorization pools
 * 8. Preimages - Handle preimage provisioning
 * 9. Statistics - Update chain statistics
 *
 * @param config The chain configuration
 * @param skipAncestryValidation When true, skip anchor recency validation in Reports STF.
 */
class BlockImporter(
  config: ChainConfig = ChainConfig.TINY,
  skipAncestryValidation: Boolean = false
):
  private val logger = LoggerFactory.getLogger(getClass)

  /**
   * Imports a block and applies all state transitions using the unified JamState pipeline.
   * Returns the computed post-state with updated state root.
   *
   * @param block The block to import
   * @param preState The state before the block (raw keyvals)
   * @return ImportResult indicating success with new state or failure with error
   */
  def importBlock(block: Block, preState: RawState): ImportResult =
    try
      // Step 1: Decode full pre-state from keyvals and convert to JamState
      val fullPreState = FullJamState.fromKeyvals(preState.keyvals, config)
      val jamState = JamState.fromFullJamState(fullPreState, config)

      // Step 2: Execute the STF pipeline using Kleisli composition
      BlockPipeline.execute(block, jamState, config, skipAncestryValidation) match
        case Left(error) =>
          ImportResult.Failure(mapPipelineError(error), error.message)

        case Right(result) =>
          logger.debug(
            s"[BlockImporter] availableReports=${result.availableReports.size} guarantees=${block.extrinsic.guarantees.size}"
          )

          // Step 3: Compute final core statistics (combines guarantees, available reports, assurances)
          val finalCoreStats = computeFinalCoreStatistics(
            guarantees = block.extrinsic.guarantees,
            availableReports = result.availableReports,
            assurances = block.extrinsic.assurances,
            maxCores = config.coresCount
          )

          // Step 4: Compute final service statistics (fresh each block)
          val finalServiceStats = computeFinalServiceStatistics(
            guarantees = block.extrinsic.guarantees,
            preimages = block.extrinsic.preimages,
            accumulationStats = result.accumulationStats
          )

          // Step 5: Convert final JamState to FullJamState for encoding
          val finalJamState = result.state.copy(
            cores = result.state.cores.copy(statistics = finalCoreStats),
            serviceStatistics = finalServiceStats
          )
          val mergedState = JamState.toFullJamState(finalJamState)

          // Step 6: Encode merged state back to keyvals
          val postKeyvals = StateEncoder.encodeFullState(mergedState, config)

          // Debug logging
          logger.debug(s"[BlockImporter KEYVALS] slot=${block.header.slot.value} total=${postKeyvals.size}")
          for kv <- postKeyvals.sortBy(_.key.toHex) do
            val prefix = kv.key.toArray(0).toInt & 0xff
            val valueHash = Hashing.blake2b256(kv.value.toArray).toHex.take(16)
            logger.debug(
              f"[KV] slot=${block.header.slot.value} key=${kv.key.toHex.take(16)}... prefix=0x$prefix%02x len=${kv.value.length}%5d valueHash=$valueHash"
            )

          // Step 7: Compute state root via Merkle trie
          val stateRoot = StateMerklization.stateMerklize(postKeyvals)
          val rawPostState = RawState(stateRoot, postKeyvals)

          // Extract SafroleState for backward compatibility
          val safrolePostState = SafroleState(
            tau = finalJamState.tau,
            eta = finalJamState.entropy.pool,
            lambda = finalJamState.validators.previous,
            kappa = finalJamState.validators.current,
            gammaK = finalJamState.validators.nextEpoch,
            iota = finalJamState.validators.queue,
            gammaA = finalJamState.gamma.a,
            gammaS = finalJamState.gamma.s,
            gammaZ = finalJamState.gamma.z,
            postOffenders = finalJamState.postOffenders
          )

          ImportResult.Success(rawPostState, mergedState, Some(safrolePostState))
    catch
      case e: Exception =>
        e.printStackTrace()
        ImportResult.Failure(ImportError.UnknownError, e.getMessage)

  /**
   * Maps pipeline errors to import errors.
   */
  private def mapPipelineError(error: PipelineError): ImportError = error match
    case PipelineError.SafroleErr(_) => ImportError.SafroleError
    case PipelineError.DisputeErr(_) => ImportError.DisputeError
    case PipelineError.AssuranceErr(_) => ImportError.AssuranceError
    case PipelineError.ReportErr(_) => ImportError.ReportError
    case PipelineError.PreimageErr(_) => ImportError.PreimageError
    case PipelineError.AccumulationErr(_) => ImportError.AccumulationError
    case PipelineError.HeaderVerificationErr(_) => ImportError.InvalidHeader
    case PipelineError.InvalidEpochMark => ImportError.InvalidHeader
    case PipelineError.InvalidTicketsMark => ImportError.InvalidHeader
    case PipelineError.InvalidBlockSeal => ImportError.InvalidHeader

  /**
   * Compute final core statistics by combining:
   * 1. Guarantee-based stats (bundleSize, gasUsed, extrinsicCount, etc.) from block's guarantees
   * 2. dataSize from available reports
   * 3. assuranceCount/popularity from assurance extrinsics
   */
  private def computeFinalCoreStatistics(
    guarantees: List[GuaranteeExtrinsic],
    availableReports: List[WorkReport],
    assurances: List[AssuranceExtrinsic],
    maxCores: Int
  ): List[CoreStatisticsRecord] =
    // Helper to update stat at specific core index
    def updateAt(
      stats: List[CoreStatisticsRecord],
      idx: Int,
      f: CoreStatisticsRecord => CoreStatisticsRecord
    ): List[CoreStatisticsRecord] =
      if idx >= 0 && idx < stats.length then
        stats.zipWithIndex.map { case (s, i) => if i == idx then f(s) else s }
      else
        stats

    val initialStats = List.fill(maxCores)(CoreStatisticsRecord())

    // Process guarantees using foldLeft
    val afterGuarantees = guarantees.foldLeft(initialStats) { (stats, guarantee) =>
      val report = guarantee.report
      val coreIdx = report.coreIndex.toInt
      if coreIdx >= 0 && coreIdx < maxCores then
        val totals = report.results.foldLeft((0L, 0L, 0L, 0L, 0L)) {
          case ((imports, extCount, extSize, exports, gas), result) =>
            val load = result.refineLoad
            (
              imports + load.imports.toLong,
              extCount + load.extrinsicCount.toLong,
              extSize + load.extrinsicSize.toLong,
              exports + load.exports.toLong,
              gas + load.gasUsed.toLong
            )
        }
        updateAt(
          stats,
          coreIdx,
          current =>
            current.copy(
              imports = current.imports + totals._1,
              extrinsicCount = current.extrinsicCount + totals._2,
              extrinsicSize = current.extrinsicSize + totals._3,
              exports = current.exports + totals._4,
              bundleSize = current.bundleSize + report.packageSpec.length.toLong,
              gasUsed = current.gasUsed + totals._5
            )
        )
      else
        stats
    }

    // Add dataSize from available reports using foldLeft
    val segmentSize = 4104L
    val afterReports = availableReports.foldLeft(afterGuarantees) { (stats, report) =>
      val coreIndex = report.coreIndex.toInt
      if coreIndex >= 0 && coreIndex < stats.length then
        val packageLength = report.packageSpec.length.toLong
        val segmentCount = report.packageSpec.exportsCount.toLong
        val segmentsSize = segmentSize * ((segmentCount * 65 + 63) / 64)
        val dataSize = packageLength + segmentsSize
        updateAt(stats, coreIndex, c => c.copy(daLoad = c.daLoad + dataSize))
      else
        stats
    }

    // Add popularity from assurances using foldLeft
    assurances.foldLeft(afterReports) { (stats, assurance) =>
      val bitfield = assurance.bitfield.toArray
      (0 until maxCores).foldLeft(stats) { (s, coreIndex) =>
        val byteIndex = coreIndex / 8
        val bitIndex = coreIndex % 8
        if byteIndex < bitfield.length then
          val attested = (bitfield(byteIndex).toInt & (1 << bitIndex)) != 0
          if attested then updateAt(s, coreIndex, c => c.copy(popularity = c.popularity + 1))
          else s
        else
          s
      }
    }

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
    // Collect all service IDs from all sources (immutable)
    val guaranteeServiceIds = guarantees.flatMap(_.report.results.map(_.serviceId.value.toLong))
    val preimageServiceIds = preimages.map(_.requester.value.toLong)
    val allServiceIds = (guaranteeServiceIds ++ preimageServiceIds ++ accumulationStats.keys).toSet

    // Build initial stats map with empty records for all services
    val initialStats = allServiceIds.map(id => id -> ReportTypes.ServiceActivityRecord()).toMap

    // Update from guarantees using foldLeft
    val afterGuarantees = guarantees.foldLeft(initialStats) { (stats, guarantee) =>
      guarantee.report.results.foldLeft(stats) { (s, result) =>
        val serviceId = result.serviceId.value.toLong
        val current = s.getOrElse(serviceId, ReportTypes.ServiceActivityRecord())
        val refineLoad = result.refineLoad
        s.updated(
          serviceId,
          current.copy(
            refinementCount = current.refinementCount + 1L,
            refinementGasUsed = current.refinementGasUsed + refineLoad.gasUsed.toLong,
            imports = current.imports + refineLoad.imports.toLong,
            exports = current.exports + refineLoad.exports.toLong,
            extrinsicCount = current.extrinsicCount + refineLoad.extrinsicCount.toLong,
            extrinsicSize = current.extrinsicSize + refineLoad.extrinsicSize.toLong
          )
        )
      }
    }

    // Update from accumulation stats using foldLeft
    val afterAccumulation = accumulationStats.foldLeft(afterGuarantees) {
      case (stats, (serviceId, (gasUsed, count))) =>
        val current = stats.getOrElse(serviceId, ReportTypes.ServiceActivityRecord())
        stats.updated(
          serviceId,
          current.copy(
            accumulateCount = current.accumulateCount + count.toLong,
            accumulateGasUsed = current.accumulateGasUsed + gasUsed
          )
        )
    }

    // Return sorted list by service ID
    afterAccumulation.toList.sortBy(_._1).map {
      case (id, record) => ReportTypes.ServiceStatisticsEntry(id = id, record = record)
    }

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
  /**
   * Extract SafroleInput from block.
   * The entropy source in the header is a VRF signature from which we extract the output.
   */
  def extractSafroleInput(block: Block): SafroleInput =
    val header = block.header
    val tickets = block.extrinsic.tickets

    // The header.entropySource is a 96-byte Bandersnatch IETF VRF signature.
    val entropyBytes = header.entropySource.toArray
    val vrfOutput =
      try
        BandersnatchWrapper.ensureLibraryLoaded()
        val output = BandersnatchWrapper.getIetfVrfOutput(entropyBytes)
        if output != null && output.length == 32 then
          Hash(output)
        else
          // Fallback: use first 32 bytes if native extraction fails
          Hash(entropyBytes.take(32))
      catch
        case _: Exception =>
          // Fallback: use first 32 bytes if native library unavailable
          Hash(entropyBytes.take(32))

    SafroleInput(
      slot = header.slot.value.toLong,
      entropy = vrfOutput,
      extrinsic = tickets
    )

  /**
   * Extract DisputeInput from block.
   */
  def extractDisputeInput(block: Block): DisputeInput =
    DisputeInput(disputes = block.extrinsic.disputes)

  /**
   * Extract AssuranceInput from block.
   */
  def extractAssuranceInput(block: Block): AssuranceInput =
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

    val workPackages = block.extrinsic.guarantees.map { guarantee =>
      ReportedWorkPackage(
        hash = guarantee.report.packageSpec.hash,
        exportsRoot = guarantee.report.packageSpec.exportsRoot // Segment root is the exports root
      )
    }.sortBy(wp => JamBytes(wp.hash.bytes))

    HistoricalInput(
      headerHash = headerHash,
      parentStateRoot = block.header.parentStateRoot,
      accumulateRoot = accumulateRoot,
      workPackages = workPackages
    )

  /**
   * Extract AuthInput from block.
   */
  def extractAuthInput(block: Block): AuthInput =
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
   * Extract StatInput from block.
   */
  def extractStatInput(block: Block): StatInput =
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
