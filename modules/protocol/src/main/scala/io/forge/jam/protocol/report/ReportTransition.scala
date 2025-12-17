package io.forge.jam.protocol.report

import io.forge.jam.core.{ChainConfig, Hashing, Shuffle, constants, StfResult, ValidationHelpers}
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey}
import io.forge.jam.core.types.workpackage.{WorkReport, SegmentRootLookup, AvailabilityAssignment}
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.service.ServiceAccount
import io.forge.jam.core.types.history.HistoricalBetaContainer
import io.forge.jam.protocol.report.ReportTypes.*
import io.forge.jam.protocol.state.JamState
import io.forge.jam.crypto.Ed25519

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

  private val MaxOutputSize: Int = 48 * 1024 // 48 KiB

  // Type alias for validation results
  private type ValidationResult = Either[ReportErrorCode, Unit]

  // Helper to check condition and return error if false
  private def require(condition: Boolean, error: => ReportErrorCode): ValidationResult =
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
    currValidators: List[ValidatorKey],
    prevValidators: List[ValidatorKey]
  ): List[ValidatorKey] =
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
  def stf(
    input: ReportInput,
    state: JamState,
    config: ChainConfig,
    skipAncestryValidation: Boolean = false
  ): (JamState, ReportOutput) =
    // Extract ReportState using lens bundle
    val preState = JamState.ReportLenses.extract(state)

    val (postState, output) = stfInternal(input, preState, config, skipAncestryValidation)

    // Apply results back using lens bundle
    val updatedState = JamState.ReportLenses.apply(state, postState)

    (updatedState, output)

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
    skipAncestryValidation: Boolean = false
  ): (ReportState, ReportOutput) =
    val result =
      for
        _ <- validateGuaranteesOrder(input.guarantees)
        _ <- validateNoDuplicatePackages(input.guarantees, preState.recentBlocks)
        _ <- if skipAncestryValidation then Right(())
        else validateAnchor(input.guarantees, preState.recentBlocks, input.slot, config)
        processedGuarantees <- processGuarantees(input, preState, config)
      yield processedGuarantees

    result match
      case Left(err) => (preState, StfResult.error(err))
      case Right((reports, packages, guarantors)) =>
        val postState = preState.copy(
          availAssignments = updateAvailAssignments(preState.availAssignments, reports, input.slot),
          coresStatistics = updateCoreStatistics(input.guarantees, config.coresCount),
          servicesStatistics = updateServiceStatistics(input.guarantees)
        )
        val outputMarks = ReportOutputMarks(
          reported = packages.sortBy(_.workPackageHash.toHex),
          reporters = guarantors.distinct.sortBy(_.toHex)
        )
        (postState, StfResult.success(outputMarks))

  /**
   * Process all guarantees and collect results.
   */
  private def processGuarantees(
    input: ReportInput,
    preState: ReportState,
    config: ChainConfig
  ): Either[ReportErrorCode, (List[WorkReport], List[SegmentRootLookup], List[Hash])] =
    // Accumulated state during processing
    case class ProcessState(
      seenCores: Set[Int],
      reports: List[WorkReport],
      packages: List[SegmentRootLookup],
      guarantors: List[Hash]
    )

    val initial = ProcessState(Set.empty, List.empty, List.empty, List.empty)

    input.guarantees.foldLeft[Either[ReportErrorCode, ProcessState]](Right(initial)) { (accum, guarantee) =>
      accum.flatMap { state =>
        for
          _ <- validateGuarantorSignatureOrder(guarantee)
          _ <- validateWorkReport(
            guarantee.report,
            guarantee.slot.toInt.toLong,
            input.slot,
            preState.accounts,
            preState.authPools,
            preState.availAssignments,
            config
          )
          _ <- validateGuarantorSignatures(
            guarantee,
            preState.currValidators,
            preState.prevValidators,
            input.slot,
            preState.entropy,
            preState.offenders,
            config
          )
          _ <- require(!state.seenCores.contains(guarantee.report.coreIndex.toInt), ReportErrorCode.CoreEngaged)
        yield
          val ctx = computeRotationContext(guarantee.slot.toInt.toLong, input.slot, config)
          val validators = selectValidatorSet(ctx, preState.currValidators, preState.prevValidators)
          val newGuarantors = guarantee.signatures.map(sig => Hash(validators(sig.validatorIndex.toInt).ed25519.bytes))

          state.copy(
            seenCores = state.seenCores + guarantee.report.coreIndex.toInt,
            reports = state.reports :+ guarantee.report,
            packages = state.packages :+ SegmentRootLookup(
              guarantee.report.packageSpec.hash,
              guarantee.report.packageSpec.exportsRoot
            ),
            guarantors = state.guarantors ++ newGuarantors
          )
      }
    }.map(s => (s.reports, s.packages, s.guarantors))

  /** Validate guarantees are sorted by core index. */
  private def validateGuaranteesOrder(guarantees: List[GuaranteeExtrinsic]): ValidationResult =
    val isSorted = ValidationHelpers.isSortedUniqueByInt(guarantees)(_.report.coreIndex.toInt)
    require(isSorted, ReportErrorCode.OutOfOrderGuarantee)

  /**
   * Validate no duplicate packages in guarantees or recent history.
   */
  private def validateNoDuplicatePackages(
    guarantees: List[GuaranteeExtrinsic],
    recentBlocks: HistoricalBetaContainer
  ): ValidationResult =
    val packageHashes = guarantees.map(_.report.packageSpec.hash)

    // Check for duplicates within batch
    if packageHashes.distinct.size != packageHashes.size then
      return Left(ReportErrorCode.DuplicatePackage)

    // Check against history
    val historyHashes = recentBlocks.history.flatMap(_.reported.map(_.hash)).toSet
    if packageHashes.exists(historyHashes.contains) then
      return Left(ReportErrorCode.DuplicatePackage)

    // Build lookup for current batch packages
    val batchPackages = guarantees.map(g => g.report.packageSpec.hash -> g.report.packageSpec.exportsRoot).toMap

    // Validate segment root lookups
    for
      guarantee <- guarantees
      lookup <- guarantee.report.segmentRootLookup
    do
      val validLookup = batchPackages.get(lookup.workPackageHash) match
        case Some(exportsRoot) => lookup.segmentTreeRoot == exportsRoot
        case None => recentBlocks.history.exists(_.reported.exists(r =>
            r.hash == lookup.workPackageHash && r.exportsRoot == lookup.segmentTreeRoot
          ))
      if !validLookup then
        return Left(ReportErrorCode.SegmentRootLookupInvalid)

    // Validate prerequisites
    val batchHashSet = packageHashes.toSet
    for
      guarantee <- guarantees
      prerequisite <- guarantee.report.context.prerequisites
    do
      val exists = batchHashSet.contains(prerequisite) ||
        recentBlocks.history.exists(_.reported.exists(_.hash == prerequisite))
      if !exists then
        return Left(ReportErrorCode.DependencyMissing)

    Right(())

  /**
   * Validate anchor recency and context.
   */
  private def validateAnchor(
    guarantees: List[GuaranteeExtrinsic],
    recentBlocks: HistoricalBetaContainer,
    currentSlot: Long,
    config: ChainConfig
  ): ValidationResult =
    val batchPackages = guarantees.map(g => g.report.packageSpec.hash -> g.report.packageSpec.exportsRoot).toMap

    for guarantee <- guarantees do
      val context = guarantee.report.context

      // Validate lookup anchor age
      if currentSlot - context.lookupAnchorSlot.toInt > config.maxLookupAnchorAge then
        return Left(ReportErrorCode.AnchorNotRecent)

      // Find and validate lookup anchor block
      val lookupAnchorBlock = recentBlocks.history.find(_.headerHash == context.lookupAnchor)
      if lookupAnchorBlock.isEmpty then
        return Left(ReportErrorCode.AnchorNotRecent)

      // Find and validate anchor block
      val anchorBlock = recentBlocks.history.find(_.headerHash == context.anchor)
      if anchorBlock.isEmpty then
        return Left(ReportErrorCode.AnchorNotRecent)

      val anchor = anchorBlock.get
      if anchor.stateRoot != context.stateRoot then
        return Left(ReportErrorCode.BadStateRoot)
      if anchor.beefyRoot != context.beefyRoot then
        return Left(ReportErrorCode.BadBeefyMmrRoot)

      // Validate prerequisites with segment root consistency
      for prerequisite <- context.prerequisites do
        val existsInBatch = batchPackages.get(prerequisite).exists { exportsRoot =>
          guarantee.report.segmentRootLookup.forall(lookup =>
            lookup.workPackageHash != prerequisite || lookup.segmentTreeRoot == exportsRoot
          )
        }
        val existsInHistory = recentBlocks.history.exists(_.reported.exists { reported =>
          reported.hash == prerequisite &&
          guarantee.report.segmentRootLookup.forall(lookup =>
            lookup.workPackageHash != prerequisite || lookup.segmentTreeRoot == reported.exportsRoot
          )
        })
        if !existsInBatch && !existsInHistory then
          return Left(ReportErrorCode.DependencyMissing)

    Right(())

  /**
   * Validate work report.
   */
  private def validateWorkReport(
    workReport: WorkReport,
    guaranteeSlot: Long,
    currentSlot: Long,
    accounts: List[ServiceAccount],
    authPools: List[List[Hash]],
    availAssignments: List[Option[AvailabilityAssignment]],
    config: ChainConfig
  ): ValidationResult =
    for
      _ <- require(guaranteeSlot <= currentSlot, ReportErrorCode.FutureReportSlot)
      _ <- require(workReport.results.nonEmpty, ReportErrorCode.MissingWorkResults)
      _ <- require(availAssignments.lift(workReport.coreIndex.toInt).flatten.isEmpty, ReportErrorCode.CoreEngaged)
      _ <- validateOutputSize(workReport)
      _ <- require(
        workReport.results.map(_.accumulateGas.toLong).sum <= config.reportAccGas,
        ReportErrorCode.WorkReportGasTooHigh
      )
      _ <- require(workReport.coreIndex.toInt < config.coresCount, ReportErrorCode.BadCoreIndex)
      _ <- validateAuthorizer(workReport, authPools)
      _ <- validateWorkResults(workReport, accounts)
      _ <- require(
        workReport.context.prerequisites.length + workReport.segmentRootLookup.length <= config.maxDependencies,
        ReportErrorCode.TooManyDependencies
      )
    yield ()

  private def validateOutputSize(workReport: WorkReport): ValidationResult =
    val totalOutputSize = workReport.authOutput.length +
      workReport.results.map(_.result match
        case ExecutionResult.Ok(output) => output.length
        case ExecutionResult.Panic => 0
      ).sum
    require(totalOutputSize <= MaxOutputSize, ReportErrorCode.WorkReportTooBig)

  private def validateAuthorizer(workReport: WorkReport, authPools: List[List[Hash]]): ValidationResult =
    val coreAuthPool = authPools.lift(workReport.coreIndex.toInt).getOrElse(List.empty)
    require(coreAuthPool.contains(workReport.authorizerHash), ReportErrorCode.CoreUnauthorized)

  private def validateWorkResults(workReport: WorkReport, accounts: List[ServiceAccount]): ValidationResult =
    for result <- workReport.results do
      // Use toLong to preserve unsigned 32-bit service ID values
      accounts.find(_.id == (result.serviceId.toInt.toLong & 0xffffffffL)) match
        case None => return Left(ReportErrorCode.BadServiceId)
        case Some(account) =>
          if result.codeHash != account.data.service.codeHash then
            return Left(ReportErrorCode.BadCodeHash)
          if result.accumulateGas.toLong < account.data.service.minItemGas then
            return Left(ReportErrorCode.ServiceItemGasTooLow)
    Right(())

  /** Validate guarantor signature order (must be sorted and unique by validator index). */
  private def validateGuarantorSignatureOrder(guarantee: GuaranteeExtrinsic): ValidationResult =
    val isSortedUnique = ValidationHelpers.isSortedUniqueByInt(guarantee.signatures)(_.validatorIndex.toInt)
    require(isSortedUnique, ReportErrorCode.NotSortedOrUniqueGuarantors)

  /**
   * Validate guarantor signatures.
   */
  private def validateGuarantorSignatures(
    guarantee: GuaranteeExtrinsic,
    currValidators: List[ValidatorKey],
    prevValidators: List[ValidatorKey],
    currentSlot: Long,
    entropy: List[Hash],
    offenders: List[Hash],
    config: ChainConfig
  ): ValidationResult =
    val sigCount = guarantee.signatures.length
    if sigCount < 2 || sigCount > 3 then
      return Left(ReportErrorCode.InsufficientGuarantees)

    val ctx = computeRotationContext(guarantee.slot.toInt.toLong, currentSlot, config)

    if ctx.reportRotation < ctx.currentRotation - 1 then
      return Left(ReportErrorCode.ReportEpochBeforeLast)
    if ctx.reportRotation > ctx.currentRotation then
      return Left(ReportErrorCode.FutureReportSlot)

    val validatorKeys = selectValidatorSet(ctx, currValidators, prevValidators)
    val coreAssignments = calculateCoreAssignments(
      if ctx.isCurrent then entropy(2)
      else if ctx.isEpochChanging then entropy(3)
      else entropy(2),
      if ctx.isCurrent then currentSlot else math.max(0, currentSlot - config.rotationPeriod),
      config
    )

    val offendersSet = offenders.map(_.toHex).toSet
    val reportedCore = guarantee.report.coreIndex.toInt

    for signature <- guarantee.signatures do
      val idx = signature.validatorIndex.toInt
      if idx < 0 || idx >= validatorKeys.length then
        return Left(ReportErrorCode.BadValidatorIndex)

      val validatorEd25519 = validatorKeys(idx).ed25519

      if offendersSet.contains(Hash(validatorEd25519.bytes).toHex) then
        return Left(ReportErrorCode.BannedValidator)

      if !verifySignature(validatorEd25519, guarantee, signature.signature) then
        return Left(ReportErrorCode.BadSignature)

      if coreAssignments(idx) != reportedCore then
        return Left(ReportErrorCode.WrongAssignment)

    Right(())

  /**
   * Calculate core assignments using shuffle algorithm.
   */
  private def calculateCoreAssignments(randomness: Hash, slot: Long, config: ChainConfig): List[Int] =
    val source = (0 until config.validatorCount).map(i => (config.coresCount * i) / config.validatorCount).toList
    val shuffledIndices = Shuffle.jamComputeShuffle(config.validatorCount, randomness)
    val shift = (math.floorMod(slot, config.epochLength) / config.rotationPeriod).toInt
    shuffledIndices.map(idx => math.floorMod(source(idx) + shift, config.coresCount))

  /**
   * Verify Ed25519 signature.
   */
  private def verifySignature(
    validatorKey: Ed25519PublicKey,
    guarantee: GuaranteeExtrinsic,
    signature: io.forge.jam.core.primitives.Ed25519Signature
  ): Boolean =
    val reportHash = Hashing.blake2b256(guarantee.report.encode)
    val message = constants.JAM_GUARANTEE_BYTES ++ reportHash.bytes
    Ed25519.verify(validatorKey, message, signature)

  /**
   * Update availability assignments with new reports.
   */
  private def updateAvailAssignments(
    existing: List[Option[AvailabilityAssignment]],
    reports: List[WorkReport],
    currentSlot: Long
  ): List[Option[AvailabilityAssignment]] =
    val reportsByCore = reports.map(r => r.coreIndex.toInt -> r).toMap
    existing.zipWithIndex.map {
      case (existing, index) =>
        reportsByCore.get(index).map(AvailabilityAssignment(_, currentSlot)).orElse(existing)
    }

  /**
   * Update core statistics based on guarantees.
   */
  private def updateCoreStatistics(guarantees: List[GuaranteeExtrinsic], coresCount: Int): List[CoreStatisticsRecord] =
    val statsByCore = guarantees
      .groupMapReduce(_.report.coreIndex.toInt)(computeCoreStats)(mergeCoreStats)

    (0 until coresCount).map(i => statsByCore.getOrElse(i, CoreStatisticsRecord())).toList

  private def computeCoreStats(guarantee: GuaranteeExtrinsic): CoreStatisticsRecord =
    val report = guarantee.report
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
    CoreStatisticsRecord(
      imports = totals._1,
      extrinsicCount = totals._2,
      extrinsicSize = totals._3,
      exports = totals._4,
      bundleSize = report.packageSpec.length.toLong,
      gasUsed = totals._5
    )

  private def mergeCoreStats(a: CoreStatisticsRecord, b: CoreStatisticsRecord): CoreStatisticsRecord =
    CoreStatisticsRecord(
      imports = a.imports + b.imports,
      extrinsicCount = a.extrinsicCount + b.extrinsicCount,
      extrinsicSize = a.extrinsicSize + b.extrinsicSize,
      exports = a.exports + b.exports,
      bundleSize = a.bundleSize + b.bundleSize,
      gasUsed = a.gasUsed + b.gasUsed
    )

  /**
   * Update service statistics based on guarantees.
   */
  private def updateServiceStatistics(guarantees: List[GuaranteeExtrinsic]): List[ServiceStatisticsEntry] =
    if guarantees.isEmpty then return List.empty

    val allResults =
      for
        guarantee <- guarantees
        result <- guarantee.report.results
      yield result

    allResults
      // Use & 0xFFFFFFFFL to preserve unsigned 32-bit service ID values
      .groupMapReduce(r => r.serviceId.toInt.toLong & 0xffffffffL)(computeServiceStats)(mergeServiceStats)
      .map { case (id, record) => ServiceStatisticsEntry(id, record) }
      .toList
      .sortBy(_.id)

  private def computeServiceStats(result: io.forge.jam.core.types.workresult.WorkResult): ServiceActivityRecord =
    val load = result.refineLoad
    ServiceActivityRecord(
      refinementCount = 1,
      refinementGasUsed = load.gasUsed.toLong,
      extrinsicCount = load.extrinsicCount.toLong,
      extrinsicSize = load.extrinsicSize.toLong,
      imports = load.imports.toLong,
      exports = load.exports.toLong
    )

  private def mergeServiceStats(a: ServiceActivityRecord, b: ServiceActivityRecord): ServiceActivityRecord =
    ServiceActivityRecord(
      refinementCount = a.refinementCount + b.refinementCount,
      refinementGasUsed = a.refinementGasUsed + b.refinementGasUsed,
      extrinsicCount = a.extrinsicCount + b.extrinsicCount,
      extrinsicSize = a.extrinsicSize + b.extrinsicSize,
      imports = a.imports + b.imports,
      exports = a.exports + b.exports
    )
