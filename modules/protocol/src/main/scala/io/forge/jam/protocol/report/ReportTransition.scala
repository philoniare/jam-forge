package io.forge.jam.protocol.report

import io.forge.jam.core.{ChainConfig, Hashing, Shuffle, constants, StfResult, ValidationHelpers}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.workpackage.{WorkReport, SegmentRootLookup, AvailabilityAssignment}
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.service.ServiceAccount
import io.forge.jam.core.types.history.HistoricalBetaContainer
import io.forge.jam.protocol.report.ReportTypes.*
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.ReportBridge
import io.forge.jam.protocol.statistics.StatsAggregation
import io.forge.jam.protocol.SignatureBatch
import spire.math.ULong
import scala.util.boundary, boundary.break

/**
 * Reports State Transition Function.
 *
 * Validates work reports according to JAM protocol specifications:
 * - Validates work reports: core bounds, authorizer presence, gas limits
 * - Verifies service existence in service accounts
 * - Verifies guarantor signatures with "jam_guarantee" prefix and Ed25519
 * - Validates anchor recency and prerequisite dependencies
 * - Checks duplicate packages against recent block history
 * - Calculates core assignments via shuffle algorithm for guarantor validation
 * - Updates core and service statistics based on refinement loads
 */
object ReportTransition:

  // Type alias for validation results
  private type ValidationResult = Either[ReportErrorCode, Unit]

  // Helper to check condition and return error if false
  private def ensure(condition: Boolean, error: => ReportErrorCode): ValidationResult =
    if condition then Right(()) else Left(error)

  /**
   * Context for rotation-based validator selection.
   */
  private case class RotationContext(
    reportRotation: Long,
    currentRotation: Long,
    isEpochChanging: Boolean,
    isCurrent: Boolean
  )

  private def computeRotationContext(reportSlot: Long, currentSlot: Long, config: ChainConfig): RotationContext =
    val reportRotation = reportSlot / config.rotationPeriod
    val currentRotation = currentSlot / config.rotationPeriod
    val isEpochChanging = (currentSlot % config.epochLength) < config.rotationPeriod
    RotationContext(reportRotation, currentRotation, isEpochChanging, reportRotation == currentRotation)

  private def selectValidatorSet(
    ctx: RotationContext,
    currValidators: IndexedSeq[ValidatorKey],
    prevValidators: IndexedSeq[ValidatorKey]
  ): IndexedSeq[ValidatorKey] =
    if ctx.isCurrent then currValidators
    else if ctx.isEpochChanging then prevValidators
    else currValidators

  /**
   * Execute the Reports STF using unified JamState.
   *
   * Reads: cores.reports, validators (kappa, lambda), entropy.pool, judgements.offenders,
   *        recentHistory, authPools, accumulation.serviceAccounts
   * Writes: cores.reports
   *
   * @param input The input containing guarantees and current slot
   * @param state The unified JamState
   * @param config The chain configuration
   * @param skipAncestryValidation When true, skip anchor recency validation
   * @return Tuple of (updated JamState, ReportOutput)
   */
  def stfView(
    input: ReportInput,
    view: TrieBackedJamState,
    skipAncestryValidation: Boolean = false,
    ancestry: List[AncestorHeader] = List.empty,
    signatures: Option[SignatureBatch] = None
  ): ReportOutput =
    if input.guarantees.isEmpty then
      // With no guarantees every check passes, ρ‡ is kept and the guarantee-derived statistics are zero: what
      // stfInternal gives, without assembling its pre-state (service accounts, package-hash sets).
      if signatures.exists(!_.verify()) then StfResult.error(ReportErrorCode.BadSignature)
      else
        view.cores.statistics = StatsAggregation.coreStatsByCore(Nil, view.config.coresCount)
        view.serviceStatistics = Nil
        StfResult.success(ReportOutputMarks(reported = Nil, reporters = Nil))
    else
      val preState = ReportBridge.extract(view)
      val (postState, output) =
        stfInternal(input, preState, view.config, skipAncestryValidation, ancestry, signatures = signatures)
      ReportBridge.apply(view, postState)
      output

  /**
   * Internal Reports STF implementation using ReportState.
   *
   * @param input The input containing guarantees and current slot
   * @param preState The pre-state for the Reports STF
   * @param config The chain configuration
   * @param skipAncestryValidation When true, skip anchor recency validation (used when ancestry feature is disabled)
   */
  def stfInternal(
    input: ReportInput,
    preState: ReportState,
    config: ChainConfig,
    skipAncestryValidation: Boolean = false,
    ancestry: List[AncestorHeader] = List.empty,
    skipAnchorContext: Boolean = false,
    signatures: Option[SignatureBatch] = None
  ): (ReportState, ReportOutput) =
    val result =
      for
        _ <- validateGuaranteesOrder(input.guarantees)
        _ <- validateNoDuplicatePackages(input.guarantees, preState, input)
        processedGuarantees <- processGuarantees(
          input,
          preState,
          config,
          ancestry,
          skipAncestryValidation,
          skipAnchorContext,
          signatures.getOrElse(new SignatureBatch)
        )
      yield processedGuarantees

    result match
      case Left(err) =>
        (preState, StfResult.error(err))
      case Right((packages, guarantors)) =>
        val postState = preState.copy(
          availAssignments = updateAvailAssignments(preState.availAssignments, input.guarantees, input.slot),
          coresStatistics = StatsAggregation.coreStatsByCore(input.guarantees, config.coresCount),
          servicesStatistics = StatsAggregation.serviceStatsFromGuarantees(input.guarantees)
        )
        val outputMarks = ReportOutputMarks(
          reported = packages.sortBy(_.workPackageHash),
          reporters = guarantors.distinct.sorted
        )
        (postState, StfResult.success(outputMarks))

  private final case class RotationCache(
    validators: IndexedSeq[ValidatorKey],
    validatorsArr: Array[ValidatorKey],
    coreAssignments: Array[Int]
  )

  private def forEach[A](items: List[A])(check: A => ValidationResult): ValidationResult =
    items.foldLeft[ValidationResult](Right(()))((result, item) => result.flatMap(_ => check(item)))

  private def processGuarantees(
    input: ReportInput,
    preState: ReportState,
    config: ChainConfig,
    ancestry: List[AncestorHeader],
    skipLookupAnchor: Boolean,
    skipAnchorContext: Boolean,
    signatures: SignatureBatch
  ): Either[ReportErrorCode, (List[SegmentRootLookup], List[Hash])] =
    val offendersSet: Set[Hash] = preState.offenders.toSet
    val accountsById: Map[Long, ServiceAccount] =
      val m = scala.collection.mutable.HashMap.empty[Long, ServiceAccount]
      preState.accounts.foreach(a => if !m.contains(a.id) then m.update(a.id, a))
      m.toMap
    val rotationCache = scala.collection.mutable.HashMap.empty[Long, RotationCache]

    def cacheFor(ctx: RotationContext): RotationCache =
      val key = (ctx.reportRotation.toLong << 1) | (if ctx.isCurrent then 1L else 0L)
      rotationCache.getOrElseUpdate(key, {
        val validators = selectValidatorSet(ctx, preState.currValidators, preState.prevValidators)
        val randomness =
          if ctx.isCurrent then preState.entropy(2)
          else if ctx.isEpochChanging then preState.entropy(3)
          else preState.entropy(2)
        val slot =
          if ctx.isCurrent then input.slot
          else math.max(0, input.slot - config.rotationPeriod)
        val assignments = calculateCoreAssignmentsArr(randomness, slot, validators.size, config)
        RotationCache(validators, validators.toArray, assignments)
      })

    val batchPackages = input.guarantees.map(g => g.report.packageSpec.hash -> g.report.packageSpec.exportsRoot).toMap
    val historyReported: Map[Hash, Hash] =
      preState.recentBlocks.history.flatMap(_.reported.map(r => r.hash -> r.exportsRoot)).toMap

    for
      _ <- forEach(input.guarantees)(validateGuarantors(_, input.slot, cacheFor, signatures, config))
      _ <- ensure(signatures.verify(), ReportErrorCode.BadSignature)
      _ <- forEach(input.guarantees)(guarantee =>
        validateReport(
          guarantee,
          input,
          preState,
          accountsById,
          offendersSet,
          cacheFor,
          ancestry,
          skipLookupAnchor,
          skipAnchorContext,
          batchPackages,
          historyReported,
          config
        )
      )
    yield
      val packages = input.guarantees.map(g => SegmentRootLookup(g.report.packageSpec.hash, g.report.packageSpec.exportsRoot))
      val guarantors = input.guarantees.flatMap { guarantee =>
        val cache = cacheFor(computeRotationContext(guarantee.slot.toLong, input.slot, config))
        guarantee.signatures.map(sig => Hash(cache.validatorsArr(sig.validatorIndex.toInt).ed25519.bytes))
      }
      (packages, guarantors)

  private def validateGuarantors(
    guarantee: GuaranteeExtrinsic,
    currentSlot: Long,
    cacheFor: RotationContext => RotationCache,
    signatures: SignatureBatch,
    config: ChainConfig
  ): ValidationResult =
    val ctx = computeRotationContext(guarantee.slot.toLong, currentSlot, config)
    lazy val validators =
      val signingCtx =
        if ctx.reportRotation > ctx.currentRotation then ctx.copy(reportRotation = ctx.currentRotation, isCurrent = true)
        else ctx
      cacheFor(signingCtx).validatorsArr
    val indices = guarantee.signatures.map(_.validatorIndex.toInt)
    for
      _ <- ensure(indices.length >= 2 && indices.length <= 3, ReportErrorCode.InsufficientGuarantees)
      _ <- ensure(ctx.reportRotation >= ctx.currentRotation - 1, ReportErrorCode.ReportEpochBeforeLast)
      _ <- ensure(indices.forall(i => i >= 0 && i < validators.length), ReportErrorCode.BadValidatorIndex)
      _ <- validateGuarantorSignatureOrder(guarantee)
    yield
      val message = constants.JAM_GUARANTEE_BYTES ++ Hashing.blake2b256(guarantee.report.encode).bytes
      signatures.addAll(guarantee.signatures.map(sig => (validators(sig.validatorIndex.toInt).ed25519, message, sig.signature)))

  private def validateReport(
    guarantee: GuaranteeExtrinsic,
    input: ReportInput,
    preState: ReportState,
    accountsById: Map[Long, ServiceAccount],
    offendersSet: Set[Hash],
    cacheFor: RotationContext => RotationCache,
    ancestry: List[AncestorHeader],
    skipLookupAnchor: Boolean,
    skipAnchorContext: Boolean,
    batchPackages: Map[Hash, Hash],
    historyReported: Map[Hash, Hash],
    config: ChainConfig
  ): ValidationResult =
    val workReport = guarantee.report
    val coreIndex = workReport.coreIndex.toInt
    val activeValidatorCount = preState.currValidators.size
    for
      _ <- ensure(guarantee.slot.toLong <= input.slot, ReportErrorCode.FutureReportSlot)
      _ <- ensure(
        coreIndex < config.coresCount && coreIndex < activeValidatorCount / 3,
        ReportErrorCode.BadCoreIndex
      )
      _ <- ensure(
        workReport.packageSpec.erasureShards.toInt == activeValidatorCount,
        ReportErrorCode.BadErasureShards
      )
      _ <- ensure(
        input.slot - workReport.context.lookupAnchorSlot.toLong <= config.maxLookupAnchorAge,
        ReportErrorCode.LookupAnchorNotRecent
      )
      _ <-
        if skipAnchorContext then Right(())
        else validateAnchor(guarantee, preState.recentBlocks, ancestry, input.slot, config, skipLookupAnchor)
      _ <- ensure(workReport.results.nonEmpty, ReportErrorCode.MissingWorkResults)
      _ <- ensure(workReport.results.length <= config.maxWorkItems, ReportErrorCode.WorkReportTooBig)
      _ <- ensure(preState.availAssignments.lift(coreIndex).flatten.isEmpty, ReportErrorCode.CoreEngaged)
      _ <- validateOutputSize(workReport)
      _ <- validateWorkResults(workReport, accountsById)
      _ <- ensure(
        sumWithin(workReport.results.map(_.accumulateGas.value), config.reportAccGas),
        ReportErrorCode.WorkReportGasTooHigh
      )
      _ <- validateWorkPackageLimits(workReport, config)
      _ <- validateAuthorizer(workReport, preState.authPools)
      _ <- ensure(
        workReport.context.prerequisites.length + workReport.segmentRootLookup.length <= config.maxDependencies,
        ReportErrorCode.TooManyDependencies
      )
      _ <- validateDependencies(workReport, batchPackages, historyReported)
      _ <- validateAssignment(guarantee, input.slot, offendersSet, cacheFor, config)
    yield ()

  /** Validate guarantees are sorted by core index. */
  private def validateGuaranteesOrder(guarantees: List[GuaranteeExtrinsic]): ValidationResult =
    val isSorted = ValidationHelpers.isSortedUniqueByInt(guarantees)(_.report.coreIndex.toInt)
    ensure(isSorted, ReportErrorCode.OutOfOrderGuarantee)

  /**
   * Validate no duplicate packages in guarantees or recent history.
   */
  private def validateNoDuplicatePackages(
    guarantees: List[GuaranteeExtrinsic],
    preState: ReportState,
    input: ReportInput
  ): ValidationResult =
    val packageHashes = guarantees.map(_.report.packageSpec.hash)
    val historyHashes = preState.recentBlocks.history.flatMap(_.reported.map(_.hash)).toSet
    val availHashes = preState.availAssignments.flatten.map(_.report.packageSpec.hash).toSet
    val allPipelinedHashes = historyHashes ++
      preState.readyQueuePackageHashes ++
      preState.accumulatedPackageHashes ++
      availHashes ++
      input.knownPackages
    for
      // Check for duplicates within batch
      _ <- ensure(packageHashes.distinct.size == packageHashes.size, ReportErrorCode.DuplicatePackage)
      _ <- ensure(!packageHashes.exists(allPipelinedHashes.contains), ReportErrorCode.DuplicatePackage)
    yield ()

  /**
   * Validate anchor recency and context.
   */
  private def validateAnchor(
    guarantee: GuaranteeExtrinsic,
    recentBlocks: HistoricalBetaContainer,
    ancestry: List[AncestorHeader],
    currentSlot: Long,
    config: ChainConfig,
    skipLookupAnchor: Boolean
  ): ValidationResult =
    val context = guarantee.report.context
    val lookupAnchorPresent =
      skipLookupAnchor || {
        val lookupAnchorSlot = context.lookupAnchorSlot.toLong
        // GP 0.8.0 §reporting: ∃ h, h' ∈ A with h_t = x_t, H(h) = x_l, h'_p = H(h), h'_r = x_l's post-state
        if ancestry.nonEmpty then
          ancestry.exists(a =>
            a.headerHash == context.lookupAnchor && a.slot == lookupAnchorSlot &&
              a.postStateRoot.forall(_ == context.lookupAnchorStateRoot)
          )
        else
          recentBlocks.history.exists(_.headerHash == context.lookupAnchor) &&
            lookupAnchorSlot <= currentSlot &&
            currentSlot - lookupAnchorSlot <= config.maxLookupAnchorAge
      }
    // Find and validate anchor block (gp: within recent history β = last Crecenthistorylen blocks)
    lazy val anchorBlock = recentBlocks.history.find(_.headerHash == context.anchor)
    for
      _ <- ensure(lookupAnchorPresent, ReportErrorCode.LookupAnchorNotRecent)
      anchor <- anchorBlock.toRight(ReportErrorCode.AnchorNotRecent)
      _ <- ensure(anchor.stateRoot == context.stateRoot, ReportErrorCode.BadStateRoot)
      _ <- ensure(anchor.beefyRoot == context.beefyRoot, ReportErrorCode.BadBeefyMmrRoot)
      _ <- ensure(anchor.slot == context.anchorSlot.toLong, ReportErrorCode.BadAnchorSlot)
    yield ()

  private def validateDependencies(
    workReport: WorkReport,
    batchPackages: Map[Hash, Hash],
    historyReported: Map[Hash, Hash]
  ): ValidationResult =
    val lookupsValid = workReport.segmentRootLookup.forall { lookup =>
      batchPackages.get(lookup.workPackageHash) match
        case Some(exportsRoot) => lookup.segmentTreeRoot == exportsRoot
        case None => historyReported.get(lookup.workPackageHash).contains(lookup.segmentTreeRoot)
    }
    val prerequisitesPresent = workReport.context.prerequisites.forall(prerequisite =>
      batchPackages.contains(prerequisite) || historyReported.contains(prerequisite)
    )
    for
      _ <- ensure(lookupsValid, ReportErrorCode.SegmentRootLookupInvalid)
      _ <- ensure(prerequisitesPresent, ReportErrorCode.DependencyMissing)
    yield ()

  /** Sum of u64 values at most `limit`, summed in BigInt so it can't wrap. */
  private def sumWithin(xs: List[ULong], limit: Long): Boolean =
    xs.foldLeft(BigInt(0))(_ + _.toBigInt) <= limit

  /**
   * Work-report values within the work-package limits.
   */
  private def validateWorkPackageLimits(workReport: WorkReport, config: ChainConfig): ValidationResult =
    val loads = workReport.results.map(_.refineLoad)
    val exports = loads.map(_.exports.toLong).sum
    val refineLoadOk =
      sumWithin(loads.map(_.gasUsed.value), config.maxRefineGas) &&
        loads.map(_.imports.toLong).sum <= constants.Cmaxpackageimports &&
        exports <= constants.Cmaxpackageexports &&
        loads.map(_.extrinsicCount.toLong).sum <= constants.Cmaxpackagexts &&
        loads.map(l => l.extrinsicSize.toLong + l.imports.toLong * constants.Csegmentfootprint).sum <=
        constants.Cmaxinputsize
    for
      _ <- ensure(workReport.packageSpec.exportsCount.toLong == exports, ReportErrorCode.BadExportsCount)
      _ <- ensure(refineLoadOk, ReportErrorCode.RefineLoadTooHigh)
      _ <- ensure(workReport.packageSpec.length.toLong <= constants.Cmaxbundlesize, ReportErrorCode.BundleTooBig)
      _ <- ensure(workReport.authGasUsed.value <= ULong(constants.Cpackageauthgas), ReportErrorCode.AuthGasTooHigh)
    yield ()

  private def validateOutputSize(workReport: WorkReport): ValidationResult =
    val totalOutputSize = workReport.authOutput.length +
      workReport.results.map(_.result match
        case ExecutionResult.Ok(output) => output.length
        case ExecutionResult.OOG => 0
        case ExecutionResult.Panic => 0
        case ExecutionResult.BadExports => 0
        case ExecutionResult.Oversize => 0
        case ExecutionResult.BadCode => 0
        case ExecutionResult.CodeTooLarge => 0
      ).sum
    ensure(totalOutputSize <= constants.Cmaxreportvarsize, ReportErrorCode.WorkReportTooBig)

  private def validateAuthorizer(workReport: WorkReport, authPools: List[List[Hash]]): ValidationResult =
    val coreAuthPool = authPools.lift(workReport.coreIndex.toInt).getOrElse(List.empty)
    ensure(coreAuthPool.contains(workReport.authorizerHash), ReportErrorCode.CoreUnauthorized)

  private def validateWorkResults(workReport: WorkReport, accountsById: Map[Long, ServiceAccount]): ValidationResult =
    boundary:
      for result <- workReport.results do
        // Use toLong to preserve unsigned 32-bit service ID values
        accountsById.get(result.serviceId.toInt.toLong & 0xffffffffL) match
          case None => break(Left(ReportErrorCode.BadServiceId))
          case Some(account) =>
            if result.codeHash != account.data.service.codeHash then
              break(Left(ReportErrorCode.BadCodeHash))
            if result.accumulateGas.value < ULong(account.data.service.minItemGas) then
              break(Left(ReportErrorCode.ServiceItemGasTooLow))
      Right(())

  /** Validate guarantor signature order (must be sorted and unique by validator index). */
  private def validateGuarantorSignatureOrder(guarantee: GuaranteeExtrinsic): ValidationResult =
    val isSortedUnique = ValidationHelpers.isSortedUniqueByInt(guarantee.signatures)(_.validatorIndex.toInt)
    ensure(isSortedUnique, ReportErrorCode.NotSortedOrUniqueGuarantors)

  private def validateAssignment(
    guarantee: GuaranteeExtrinsic,
    currentSlot: Long,
    offendersSet: Set[Hash],
    cacheFor: RotationContext => RotationCache,
    config: ChainConfig
  ): ValidationResult =
    val cache = cacheFor(computeRotationContext(guarantee.slot.toLong, currentSlot, config))
    val reportedCore = guarantee.report.coreIndex.toInt
    boundary:
      for signature <- guarantee.signatures do
        val idx = signature.validatorIndex.toInt
        if offendersSet.contains(Hash(cache.validatorsArr(idx).ed25519.bytes)) then
          break(Left(ReportErrorCode.BannedValidator))
        if cache.coreAssignments(idx) != reportedCore then
          break(Left(ReportErrorCode.WrongAssignment))
      Right(())

  private def calculateCoreAssignmentsArr(
    randomness: Hash,
    slot: Long,
    validatorCount: Int,
    config: ChainConfig
  ): Array[Int] =
    val activeCores = math.max(1, validatorCount / 3)
    val source = new Array[Int](validatorCount)
    var i = 0
    while i < validatorCount do
      source(i) = i / 3
      i += 1
    val shuffledIndices = Shuffle.jamComputeShuffle(validatorCount, randomness)
    val shift = (math.floorMod(slot, config.epochLength) / config.rotationPeriod).toInt
    val out = new Array[Int](shuffledIndices.length)
    var j = 0
    val it = shuffledIndices.iterator
    while it.hasNext do
      val idx = it.next()
      out(j) = math.floorMod(source(idx) + shift, activeCores)
      j += 1
    out

  /**
   * Update availability assignments with new reports.
   */
  private def updateAvailAssignments(
    existing: List[Option[AvailabilityAssignment]],
    guarantees: List[GuaranteeExtrinsic],
    currentSlot: Long
  ): List[Option[AvailabilityAssignment]] =
    val guaranteesByCore = guarantees.map(g => g.report.coreIndex.toInt -> g).toMap
    existing.zipWithIndex.map {
      case (existing, index) =>
        guaranteesByCore.get(index).map(AvailabilityAssignment(_, currentSlot)).orElse(existing)
    }
