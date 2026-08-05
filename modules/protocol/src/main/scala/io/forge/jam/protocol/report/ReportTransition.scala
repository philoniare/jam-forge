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
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.ReportBridge
import io.forge.jam.crypto.Ed25519
import spire.math.ULong

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
  def stfView(
    input: ReportInput,
    view: TrieBackedJamState,
    skipAncestryValidation: Boolean = false,
    ancestry: List[AncestorHeader] = List.empty
  ): ReportOutput =
    val preState = ReportBridge.extract(view)
    val (postState, output) =
      stfInternal(input, preState, view.config, skipAncestryValidation, ancestry)
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
    ancestry: List[AncestorHeader] = List.empty
  ): (ReportState, ReportOutput) =
    val result =
      for
        _ <- validateGuaranteesOrder(input.guarantees)
        _ <- validateNoDuplicatePackages(input.guarantees, preState, input)
        _ <- validateAnchorAge(input.guarantees, input.slot, config)
        _ <- if skipAncestryValidation then Right(())
        else validateAnchor(input.guarantees, preState.recentBlocks, ancestry, input.slot, config)
        processedGuarantees <- processGuarantees(input, preState, config)
      yield processedGuarantees

    result match
      case Left(err) =>
        (preState, StfResult.error(err))
      case Right((reports, packages, guarantors)) =>
        val postState = preState.copy(
          availAssignments = updateAvailAssignments(preState.availAssignments, reports, input.slot),
          coresStatistics = updateCoreStatistics(input.guarantees, config.coresCount),
          servicesStatistics = updateServiceStatistics(input.guarantees)
        )
        val outputMarks = ReportOutputMarks(
          reported = packages.sortBy(_.workPackageHash),
          reporters = guarantors.distinct.sorted
        )
        (postState, StfResult.success(outputMarks))

  /**
   * Process all guarantees and collect results.
   */
  private final case class RotationCache(
    validators: List[ValidatorKey],
    validatorsArr: Array[ValidatorKey],
    coreAssignments: Array[Int]
  )

  private def processGuarantees(
    input: ReportInput,
    preState: ReportState,
    config: ChainConfig
  ): Either[ReportErrorCode, (List[WorkReport], List[SegmentRootLookup], List[Hash])] =
    val offendersSet: Set[Hash] = preState.offenders.toSet
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
        val assignments = calculateCoreAssignmentsArr(randomness, slot, config)
        RotationCache(validators, validators.toArray, assignments)
      })

    val sigBuf = scala.collection.mutable.ArrayBuffer.empty[
      (Ed25519PublicKey, Array[Byte], io.forge.jam.core.primitives.Ed25519Signature)
    ]

    val seenCores      = scala.collection.mutable.HashSet.empty[Int]
    val reportsBuf     = scala.collection.mutable.ListBuffer.empty[WorkReport]
    val packagesBuf    = scala.collection.mutable.ListBuffer.empty[SegmentRootLookup]
    val guarantorsBuf  = scala.collection.mutable.ListBuffer.empty[Hash]

    var remaining = input.guarantees
    while remaining.nonEmpty do
      val guarantee = remaining.head
      remaining = remaining.tail

      validateGuarantorSignatureOrder(guarantee) match
        case Left(err) => return Left(err)
        case _         => ()

      validateWorkReport(
        guarantee.report,
        guarantee.slot.value.toLong,
        input.slot,
        preState.accounts,
        preState.authPools,
        preState.availAssignments,
        config
      ) match
        case Left(err) => return Left(err)
        case _         => ()

      validateGuarantorSignaturesCached(
        guarantee,
        input.slot,
        offendersSet,
        cacheFor,
        sigBuf,
        config
      ) match
        case Left(err) => return Left(err)
        case _         => ()

      val coreIdx = guarantee.report.coreIndex.toInt
      if seenCores.contains(coreIdx) then return Left(ReportErrorCode.CoreEngaged)
      seenCores += coreIdx

      val ctx   = computeRotationContext(guarantee.slot.value.toLong, input.slot, config)
      val cache = cacheFor(ctx)

      reportsBuf  += guarantee.report
      packagesBuf += SegmentRootLookup(
        guarantee.report.packageSpec.hash,
        guarantee.report.packageSpec.exportsRoot
      )

      var sigs = guarantee.signatures
      while sigs.nonEmpty do
        val sig = sigs.head
        sigs = sigs.tail
        guarantorsBuf += Hash(cache.validatorsArr(sig.validatorIndex.toInt).ed25519.bytes)

    val n = sigBuf.size
    if n != 0 then
      val tuples = sigBuf.toArray
      val allValid = java.util.stream.IntStream
        .range(0, n)
        .parallel()
        .allMatch { i =>
          val (pk, msg, sig) = tuples(i)
          Ed25519.verify(pk, msg, sig)
        }
      if !allValid then return Left(ReportErrorCode.BadSignature)

    Right((reportsBuf.toList, packagesBuf.toList, guarantorsBuf.toList))

  /** Validate guarantees are sorted by core index. */
  private def validateGuaranteesOrder(guarantees: List[GuaranteeExtrinsic]): ValidationResult =
    val isSorted = ValidationHelpers.isSortedUniqueByInt(guarantees)(_.report.coreIndex.toInt)
    require(isSorted, ReportErrorCode.OutOfOrderGuarantee)

  /**
   * Validate no duplicate packages in guarantees or recent history.
   */
  private def validateNoDuplicatePackages(
    guarantees: List[GuaranteeExtrinsic],
    preState: ReportState,
    input: ReportInput
  ): ValidationResult =
    val recentBlocks = preState.recentBlocks
    val packageHashes = guarantees.map(_.report.packageSpec.hash)

    // Check for duplicates within batch
    if packageHashes.distinct.size != packageHashes.size then
      return Left(ReportErrorCode.DuplicatePackage)

    val historyHashes = recentBlocks.history.flatMap(_.reported.map(_.hash)).toSet
    val availHashes = preState.availAssignments.flatten.map(_.report.packageSpec.hash).toSet
    val allPipelinedHashes = historyHashes ++
      preState.readyQueuePackageHashes ++
      preState.accumulatedPackageHashes ++
      availHashes ++
      input.knownPackages
    if packageHashes.exists(allPipelinedHashes.contains) then
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
   * Validate lookup anchor slot age.
   */
  private def validateAnchorAge(
    guarantees: List[GuaranteeExtrinsic],
    currentSlot: Long,
    config: ChainConfig
  ): ValidationResult =
    for guarantee <- guarantees do
      val lookupAnchorSlot = guarantee.report.context.lookupAnchorSlot.value.toLong
      if lookupAnchorSlot > currentSlot || currentSlot - lookupAnchorSlot > config.maxLookupAnchorAge then
        return Left(ReportErrorCode.LookupAnchorNotRecent)
    Right(())

  /**
   * Validate anchor recency and context.
   */
  private def validateAnchor(
    guarantees: List[GuaranteeExtrinsic],
    recentBlocks: HistoricalBetaContainer,
    ancestry: List[AncestorHeader],
    currentSlot: Long,
    config: ChainConfig
  ): ValidationResult =
    val batchPackages = guarantees.map(g => g.report.packageSpec.hash -> g.report.packageSpec.exportsRoot).toMap

    for guarantee <- guarantees do
      val context = guarantee.report.context

      val lookupAnchorSlot = context.lookupAnchorSlot.value.toLong
      val lookupAnchorPresent =
        if ancestry.nonEmpty then
          ancestry.exists(a => a.headerHash == context.lookupAnchor && a.slot == lookupAnchorSlot)
        else
          recentBlocks.history.exists(_.headerHash == context.lookupAnchor)
      if !lookupAnchorPresent then
        return Left(ReportErrorCode.LookupAnchorNotRecent)

      // Find and validate anchor block (gp: within recent history β = last Crecenthistorylen blocks)
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
      _ <- require(workReport.results.length <= config.maxWorkItems, ReportErrorCode.WorkReportTooBig)
      _ <- require(availAssignments.lift(workReport.coreIndex.toInt).flatten.isEmpty, ReportErrorCode.CoreEngaged)
      _ <- validateOutputSize(workReport)
      _ <- {
        // ULong sum: signed-Long sum can wrap and falsely satisfy the bound.
        val totalAccGas = workReport.results.foldLeft(ULong(0L)) { (acc, r) =>
          acc + ULong(r.accumulateGas.toLong)
        }
        require(totalAccGas <= ULong(config.reportAccGas), ReportErrorCode.WorkReportGasTooHigh)
      }
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
        case ExecutionResult.OOG => 0
        case ExecutionResult.Panic => 0
        case ExecutionResult.BadExports => 0
        case ExecutionResult.Oversize => 0
        case ExecutionResult.BadCode => 0
        case ExecutionResult.CodeTooLarge => 0
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

  private def validateGuarantorSignaturesCached(
    guarantee: GuaranteeExtrinsic,
    currentSlot: Long,
    offendersSet: Set[Hash],
    cacheFor: RotationContext => RotationCache,
    sigBuf: scala.collection.mutable.ArrayBuffer[
      (Ed25519PublicKey, Array[Byte], io.forge.jam.core.primitives.Ed25519Signature)
    ],
    config: ChainConfig
  ): ValidationResult =
    val sigCount = guarantee.signatures.length
    if sigCount < 2 || sigCount > 3 then
      return Left(ReportErrorCode.InsufficientGuarantees)

    val ctx = computeRotationContext(guarantee.slot.value.toLong, currentSlot, config)
    if ctx.reportRotation < ctx.currentRotation - 1 then
      return Left(ReportErrorCode.ReportEpochBeforeLast)
    if ctx.reportRotation > ctx.currentRotation then
      return Left(ReportErrorCode.FutureReportSlot)

    val cache = cacheFor(ctx)
    val validatorsArr = cache.validatorsArr
    val coreAssignments = cache.coreAssignments
    val reportedCore = guarantee.report.coreIndex.toInt

    val reportHash = Hashing.blake2b256(guarantee.report.encode)
    val message = constants.JAM_GUARANTEE_BYTES ++ reportHash.bytes

    var sigs = guarantee.signatures
    while sigs.nonEmpty do
      val signature = sigs.head
      sigs = sigs.tail
      val idx = signature.validatorIndex.toInt
      if idx < 0 || idx >= validatorsArr.length then
        return Left(ReportErrorCode.BadValidatorIndex)

      val validatorEd25519 = validatorsArr(idx).ed25519
      if offendersSet.contains(Hash(validatorEd25519.bytes)) then
        return Left(ReportErrorCode.BannedValidator)
      if coreAssignments(idx) != reportedCore then
        return Left(ReportErrorCode.WrongAssignment)
      sigBuf += ((validatorEd25519, message, signature.signature))

    Right(())

  private def calculateCoreAssignmentsArr(randomness: Hash, slot: Long, config: ChainConfig): Array[Int] =
    val validatorCount = config.validatorCount
    val coresCount = config.coresCount
    val source = new Array[Int](validatorCount)
    var i = 0
    while i < validatorCount do
      source(i) = (coresCount * i) / validatorCount
      i += 1
    val shuffledIndices = Shuffle.jamComputeShuffle(validatorCount, randomness)
    val shift = (math.floorMod(slot, config.epochLength) / config.rotationPeriod).toInt
    val out = new Array[Int](shuffledIndices.length)
    var j = 0
    val it = shuffledIndices.iterator
    while it.hasNext do
      val idx = it.next()
      out(j) = math.floorMod(source(idx) + shift, coresCount)
      j += 1
    out

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
