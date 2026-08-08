package io.forge.jam.protocol.traces

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.vrfs.BandersnatchWrapper
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.extrinsic.{
  AssuranceExtrinsic,
  GuaranteeExtrinsic,
  Preimage
}
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
import io.forge.jam.protocol.state.{ServiceStorageView, TrieBackedJamState}
import io.forge.jam.core.trie.{StateTrieStore, InMemoryTrieBackend}

sealed trait ImportResult

object ImportResult:
  final case class Success(
      postStateRoot: Hash,
      safroleState: Option[SafroleState] = None
  ) extends ImportResult

  final case class Failure(
      error: ImportError,
      message: String = ""
  ) extends ImportResult

/** Errors that can occur during block import.
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
  case UnknownError

/** BlockImporter handles importing blocks and applying all state transitions.
  *
  * Uses a unified JamState pipeline where state flows sequentially through all
  * 9 STFs:
  *   1. Safrole - Block production and VRF validation
  *   2. Disputes - Process dispute verdicts
  *   3. Assurances - Process availability assurances
  *   4. Reports - Process work reports (guarantees)
  *   5. Accumulation - Execute PVM accumulation
  *   6. History - Update recent blocks history
  *   7. Authorizations - Update authorization pools
  *   8. Preimages - Handle preimage provisioning
  *   9. Statistics - Update chain statistics
  *
  * @param config
  *   The chain configuration
  * @param skipAncestryValidation
  *   When true, skip anchor recency validation in Reports STF.
  */
class BlockImporter(
    config: ChainConfig,
    skipAncestryValidation: Boolean = false,
    externalTrieStore: Option[StateTrieStore] = None
):

  // Shared PVM module cache across block imports to avoid recompiling same service code
  private val sharedExecutor =
    new io.forge.jam.protocol.accumulation.AccumulationExecutor(config)

  private val trieStore: StateTrieStore =
    externalTrieStore.getOrElse(new StateTrieStore(new InMemoryTrieBackend))

  def store: StateTrieStore = trieStore

  private var bootstrapCount: Int = 0
  def bootstrapCalls: Int = bootstrapCount

  def currentTrieRoot: Hash = trieStore.currentRoot

  /** Imports a block and applies all state transitions using the unified
    * JamState pipeline. Returns the computed post-state with updated state
    * root.
    *
    * @param block
    *   The block to import
    * @param preState
    *   The state before the block (raw keyvals)
    * @return
    *   ImportResult indicating success with new state or failure with error
    */
  def importBlock(
      block: Block,
      preState: RawState,
      ancestry: List[AncestorHeader] = List.empty
  ): ImportResult =
    try
      if block.header.parentStateRoot != preState.stateRoot then
        return ImportResult.Failure(
          ImportError.InvalidStateRoot,
          "Block's parent_state_root does not match pre-state root"
        )

      if trieStore.currentRoot != block.header.parentStateRoot then
        trieStore.bootstrap(preState.keyvals.map(kv => (kv.key, kv.value)))
        bootstrapCount += 1
        if trieStore.currentRoot != block.header.parentStateRoot then
          return ImportResult.Failure(
            ImportError.InvalidStateRoot,
            s"Bootstrapped state root ${trieStore.currentRoot} does not match " +
              s"block parent_state_root ${block.header.parentStateRoot}"
          )
      val trie = trieStore.at(block.header.parentStateRoot)

      val storageView = new ServiceStorageView(trie)
      val view = new TrieBackedJamState(trie, config, storageView, Some(trieStore))

      sharedExecutor.setStorageView(Some(storageView))

      val pipelineResult =
        try
          BlockPipeline.execute(
            block,
            view,
            skipAncestryValidation,
            Some(sharedExecutor),
            ancestry
          )
        finally sharedExecutor.setStorageView(None)

      pipelineResult match
        case Left(error) =>
          ImportResult.Failure(mapPipelineError(error), error.message)

        case Right(result) =>
          val finalCoreStats = computeFinalCoreStatistics(
            guarantees = block.extrinsic.guarantees,
            availableReports = result.availableReports,
            assurances = block.extrinsic.assurances,
            maxCores = config.coresCount
          )

          val finalServiceStats = computeFinalServiceStatistics(
            guarantees = block.extrinsic.guarantees,
            preimages = block.extrinsic.preimages,
            accumulationStats = result.accumulationStats
          )

          view.cores.statistics = finalCoreStats
          view.serviceStatistics = finalServiceStats

          view.commit(trie)
          storageView.commit(trie)
          trie.save()
          trieStore.markCommitted(trie.rootHash)
          trieStore.gc()

          val safrolePostState = SafroleState(
            tau = view.timeslot,
            eta = view.entropy.pool,
            lambda = view.validators.previous,
            kappa = view.validators.current,
            gammaK = view.validators.nextEpoch,
            iota = view.validators.queue,
            gammaA = view.gamma.a,
            gammaS = view.gamma.st,
            gammaZ = view.gamma.z,
            postOffenders = view.postOffenders
          )

          ImportResult.Success(trie.rootHash, Some(safrolePostState))
    catch
      case e: Throwable =>
        e.printStackTrace()
        ImportResult.Failure(
          ImportError.UnknownError,
          Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
        )

  /** Maps pipeline errors to import errors.
    */
  private def mapPipelineError(error: PipelineError): ImportError = error match
    case PipelineError.SafroleErr(_)            => ImportError.SafroleError
    case PipelineError.DisputeErr(_)            => ImportError.DisputeError
    case PipelineError.AssuranceErr(_)          => ImportError.AssuranceError
    case PipelineError.ReportErr(_)             => ImportError.ReportError
    case PipelineError.PreimageErr(_)           => ImportError.PreimageError
    case PipelineError.HeaderVerificationErr(_) => ImportError.InvalidHeader
    case PipelineError.InvalidEpochMark         => ImportError.InvalidHeader
    case PipelineError.InvalidTicketsMark       => ImportError.InvalidHeader
    case PipelineError.InvalidBlockSeal         => ImportError.InvalidHeader
    case PipelineError.InvalidOffendersMark     => ImportError.InvalidHeader

  /** Compute final core statistics by combining:
    *   1. Guarantee-based stats (bundleSize, gasUsed, extrinsicCount, etc.)
    *      from block's guarantees
    *   2. dataSize from available reports
    *   3. assuranceCount/popularity from assurance extrinsics
    */
  private def computeFinalCoreStatistics(
      guarantees: List[GuaranteeExtrinsic],
      availableReports: List[WorkReport],
      assurances: List[AssuranceExtrinsic],
      maxCores: Int
  ): List[CoreStatisticsRecord] =
    val stats = Array.fill(maxCores)(CoreStatisticsRecord())

    var gi = guarantees
    while gi.nonEmpty do
      val guarantee = gi.head
      gi = gi.tail
      val report = guarantee.report
      val coreIdx = report.coreIndex.toInt
      if coreIdx >= 0 && coreIdx < maxCores then
        var imports = 0L
        var extCount = 0L
        var extSize = 0L
        var exports = 0L
        var gas = 0L
        var rs = report.results
        while rs.nonEmpty do
          val load = rs.head.refineLoad
          imports += load.imports.toLong
          extCount += load.extrinsicCount.toLong
          extSize += load.extrinsicSize.toLong
          exports += load.exports.toLong
          gas += load.gasUsed.toLong
          rs = rs.tail
        val cur = stats(coreIdx)
        stats(coreIdx) = cur.copy(
          imports = cur.imports + imports,
          extrinsicCount = cur.extrinsicCount + extCount,
          extrinsicSize = cur.extrinsicSize + extSize,
          exports = cur.exports + exports,
          bundleSize = cur.bundleSize + report.packageSpec.length.toLong,
          gasUsed = cur.gasUsed + gas
        )

    val segmentSize = 4104L
    var ari = availableReports
    while ari.nonEmpty do
      val report = ari.head
      ari = ari.tail
      val coreIndex = report.coreIndex.toInt
      if coreIndex >= 0 && coreIndex < maxCores then
        val packageLength = report.packageSpec.length.toLong
        val segmentCount = report.packageSpec.exportsCount.toLong
        val segmentsSize = segmentSize * ((segmentCount * 65 + 63) / 64)
        val cur = stats(coreIndex)
        stats(coreIndex) =
          cur.copy(daLoad = cur.daLoad + packageLength + segmentsSize)

    var asi = assurances
    while asi.nonEmpty do
      val assurance = asi.head
      asi = asi.tail
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

    stats.toList

  /** Compute fresh service statistics by combining:
    *   1. Work reports from guarantees (refinementCount, gasUsed, imports,
    *      exports, extrinsicCount, extrinsicSize)
    *   2. Preimages (providedCount, providedSize)
    *   3. Accumulation results (accumulateCount, accumulateGasUsed)
    */
  private def computeFinalServiceStatistics(
      guarantees: List[GuaranteeExtrinsic],
      preimages: List[Preimage],
      accumulationStats: Map[
        Long,
        (Long, Int)
      ] // serviceId -> (gasUsed, workItemCount)
  ): List[ReportTypes.ServiceStatisticsEntry] =
    // Collect all service IDs from all sources (immutable)
    val stats =
      scala.collection.mutable.LongMap.empty[ReportTypes.ServiceActivityRecord]
    def getOrEmpty(id: Long): ReportTypes.ServiceActivityRecord =
      stats.getOrElse(id, ReportTypes.ServiceActivityRecord())

    var gi = guarantees
    while gi.nonEmpty do
      var rs = gi.head.report.results
      while rs.nonEmpty do
        val r = rs.head
        val serviceId = r.serviceId.value.toLong
        val refineLoad = r.refineLoad
        val cur = getOrEmpty(serviceId)
        stats(serviceId) = cur.copy(
          refinementCount = cur.refinementCount + 1L,
          refinementGasUsed = cur.refinementGasUsed + refineLoad.gasUsed.toLong,
          imports = cur.imports + refineLoad.imports.toLong,
          exports = cur.exports + refineLoad.exports.toLong,
          extrinsicCount =
            cur.extrinsicCount + refineLoad.extrinsicCount.toLong,
          extrinsicSize = cur.extrinsicSize + refineLoad.extrinsicSize.toLong
        )
        rs = rs.tail
      gi = gi.tail

    var pi = preimages
    while pi.nonEmpty do
      val p = pi.head
      val serviceId = p.requester.value.toLong
      val cur = getOrEmpty(serviceId)
      stats(serviceId) = cur.copy(
        providedCount = cur.providedCount + 1,
        providedSize = cur.providedSize + p.blob.length.toLong
      )
      pi = pi.tail

    accumulationStats.foreach { case (serviceId, (gasUsed, count)) =>
      val cur = getOrEmpty(serviceId)
      stats(serviceId) = cur.copy(
        accumulateCount = cur.accumulateCount + count.toLong,
        accumulateGasUsed = cur.accumulateGasUsed + gasUsed
      )
    }

    // Sorted-by-service-id list, materialised once at the end.
    val entries =
      new Array[(Long, ReportTypes.ServiceActivityRecord)](stats.size)
    var idx = 0
    stats.foreachEntry { (id, rec) =>
      entries(idx) = (id, rec); idx += 1
    }
    java.util.Arrays.sort(
      entries,
      (
          a: (Long, ReportTypes.ServiceActivityRecord),
          b: (Long, ReportTypes.ServiceActivityRecord)
      ) => java.lang.Long.compare(a._1, b._1)
    )
    val out = scala.collection.mutable.ListBuffer
      .empty[ReportTypes.ServiceStatisticsEntry]
    var oi = 0
    while oi < entries.length do
      val (id, record) = entries(oi)
      out += ReportTypes.ServiceStatisticsEntry(id = id, record = record)
      oi += 1
    out.toList

  /** Imports a block and returns just the computed SafroleState for comparison.
    * This is useful for trace testing where we want to compare typed state.
    */
  def importBlockForSafrole(
      block: Block,
      preState: RawState
  ): (Option[SafroleState], Option[String]) =
    try
      importBlock(block, preState) match
        case ImportResult.Success(_, safroleState) => (safroleState, None)
        case ImportResult.Failure(_, message)      => (None, Some(message))
    catch
      case e: Exception =>
        (None, Some(s"Exception: ${e.getMessage}"))

  /** Validates that a block import produces the expected post-state. Used for
    * testing against trace vectors.
    */
  def validateBlockImport(
      block: Block,
      preState: RawState,
      expectedPostState: RawState
  ): Boolean =
    importBlock(block, preState) match
      case ImportResult.Success(actualRoot, _) =>
        actualRoot == expectedPostState.stateRoot
      case ImportResult.Failure(_, _) =>
        false

  def materializePostState(config: ChainConfig): RawState =
    val trie = trieStore.at(trieStore.currentRoot)
    val pairs = trie.getKeyValues(JamBytes.empty, 0)
    val kvs = pairs.map { case (k, v) => KeyValue(k, v) }
    RawState(trieStore.currentRoot, kvs)

/** Extracts STF inputs from block and state.
  */
object InputExtractor:
  /** Extract SafroleInput from block. The entropy source in the header is a VRF
    * signature from which we extract the output.
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
        if output != null && output.length == 32 then Hash(output)
        else
          throw new IllegalStateException(
            "Bandersnatch IETF VRF output extraction failed for header entropy source"
          )
      catch
        case e: IllegalStateException => throw e
        case e: Exception =>
          throw new IllegalStateException(
            s"Bandersnatch native VRF-output extraction unavailable for header entropy source: ${e.getMessage}",
            e
          )

    SafroleInput(
      slot = header.slot.value.toLong,
      entropy = vrfOutput,
      extrinsic = tickets
    )

  /** Extract DisputeInput from block.
    */
  def extractDisputeInput(block: Block): DisputeInput =
    DisputeInput(disputes = block.extrinsic.disputes)

  /** Extract AssuranceInput from block.
    */
  def extractAssuranceInput(block: Block): AssuranceInput =
    AssuranceInput(
      assurances = block.extrinsic.assurances,
      slot = block.header.slot.value.toLong,
      parent = block.header.parent
    )

  /** Extract AccumulationInput from available reports and slot.
    */
  def extractAccumulationInput(
      availableReports: List[WorkReport],
      slot: Long
  ): AccumulationInput =
    AccumulationInput(
      slot = slot,
      reports = availableReports
    )

  /** Extract HistoricalInput from block and accumulate root.
    */
  def extractHistoryInput(block: Block, accumulateRoot: Hash): HistoricalInput =
    import io.forge.jam.core.scodec.JamCodecs.encode
    import _root_.scodec.Codec
    val headerHash = Hashing.blake2b256(block.header.encode.toArray)

    val workPackages = block.extrinsic.guarantees
      .map { guarantee =>
        ReportedWorkPackage(
          hash = guarantee.report.packageSpec.hash,
          exportsRoot =
            guarantee.report.packageSpec.exportsRoot // Segment root is the exports root
        )
      }
      .sortBy(wp => JamBytes(wp.hash.bytes))

    HistoricalInput(
      headerHash = headerHash,
      parentStateRoot = block.header.parentStateRoot,
      accumulateRoot = accumulateRoot,
      workPackages = workPackages
    )

  /** Extract AuthInput from block.
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

  /** Extract PreimageInput from block.
    */
  def extractPreimageInput(block: Block, slot: Long): PreimageInput =
    PreimageInput(
      preimages = block.extrinsic.preimages,
      slot = slot
    )

  /** Extract StatInput from block.
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

/** Encoder for converting typed state structures back to raw keyvals.
  */
object StateEncoder:
  /** Encode the full FullJamState back to keyvals. This encodes all state
    * components according to the Gray Paper state layout.
    */
  def encodeFullState(
      state: FullJamState,
      config: ChainConfig
  ): List[KeyValue] =
    state.toKeyvals(config)
