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
import io.forge.jam.protocol.statistics.StatsAggregation
import io.forge.jam.crypto.Ed25519
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
    ancestry: List[AncestorHeader] = List.empty,
    skipAnchorContext: Boolean = false
  ): (ReportState, ReportOutput) =
    val result =
      for
        _ <- validateGuaranteesOrder(input.guarantees)
        _ <- validateNoDuplicatePackages(input.guarantees, preState, input)
        _ <- validateAnchorAge(input.guarantees, input.slot, config)
        _ <- if skipAnchorContext then Right(())
        else validateAnchor(input.guarantees, preState.recentBlocks, ancestry, input.slot, config, skipLookupAnchor = skipAncestryValidation)
        processedGuarantees <- processGuarantees(input, preState, config)
      yield processedGuarantees

    result match
      case Left(err) =>
        (preState, StfResult.error(err))
      case Right((reports, packages, guarantors)) =>
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
        val assignments = calculateCoreAssignmentsArr(randomness, slot, config)
        RotationCache(validators, validators.toArray, assignments)
      })

    val sigBuf = scala.collection.mutable.ArrayBuffer.empty[
      (Ed25519PublicKey, Array[Byte], io.forge.jam.core.primitives.Ed25519Signature)
    ]

    val reportsBuf     = scala.collection.mutable.ListBuffer.empty[WorkReport]
    val packagesBuf    = scala.collection.mutable.ListBuffer.empty[SegmentRootLookup]
    val guarantorsBuf  = scala.collection.mutable.ListBuffer.empty[Hash]

    boundary:
      var remaining = input.guarantees
      while remaining.nonEmpty do
        val guarantee = remaining.head
        remaining = remaining.tail

        validateGuarantee(guarantee, input, preState, accountsById, offendersSet, cacheFor, sigBuf, config) match
          case Left(err) => break(Left(err))
          case Right(products) =>
            reportsBuf    += products.report
            packagesBuf   += products.packageLookup
            guarantorsBuf ++= products.guarantors

      verifyGuarantorSignatureBatch(sigBuf) match
        case Left(err) => break(Left(err))
        case _         => ()

      Right((reportsBuf.toList, packagesBuf.toList, guarantorsBuf.toList))

  /** Per-guarantee accumulation products: the report, its segment-root lookup, its guarantor keys. */
  private final case class GuaranteeProducts(
    report: WorkReport,
    packageLookup: SegmentRootLookup,
    guarantors: List[Hash]
  )

  /**
   * Validate one guarantee and derive its accumulation products.
   */
  private def validateGuarantee(
    guarantee: GuaranteeExtrinsic,
    input: ReportInput,
    preState: ReportState,
    accountsById: Map[Long, ServiceAccount],
    offendersSet: Set[Hash],
    cacheFor: RotationContext => RotationCache,
    sigBuf: scala.collection.mutable.ArrayBuffer[
      (Ed25519PublicKey, Array[Byte], io.forge.jam.core.primitives.Ed25519Signature)
    ],
    config: ChainConfig
  ): Either[ReportErrorCode, GuaranteeProducts] =
    for
      _ <- validateGuarantorSignatureOrder(guarantee)
      _ <- validateWorkReport(
        guarantee.report,
        guarantee.slot.value.toLong,
        input.slot,
        accountsById,
        preState.authPools,
        preState.availAssignments,
        config
      )
      _ <- validateGuarantorSignaturesCached(
        guarantee,
        input.slot,
        offendersSet,
        cacheFor,
        sigBuf,
        config
      )
    yield
      val ctx   = computeRotationContext(guarantee.slot.value.toLong, input.slot, config)
      val cache = cacheFor(ctx)

      val packageLookup = SegmentRootLookup(
        guarantee.report.packageSpec.hash,
        guarantee.report.packageSpec.exportsRoot
      )

      val guarantorsBuf = scala.collection.mutable.ListBuffer.empty[Hash]
      var sigs = guarantee.signatures
      while sigs.nonEmpty do
        val sig = sigs.head
        sigs = sigs.tail
        guarantorsBuf += Hash(cache.validatorsArr(sig.validatorIndex.toInt).ed25519.bytes)

      GuaranteeProducts(guarantee.report, packageLookup, guarantorsBuf.toList)

  /**
   * Verify every collected guarantor signature in one parallel batch.
   */
  private def verifyGuarantorSignatureBatch(
    sigBuf: scala.collection.mutable.ArrayBuffer[
      (Ed25519PublicKey, Array[Byte], io.forge.jam.core.primitives.Ed25519Signature)
    ]
  ): ValidationResult =
    val n = sigBuf.size
    if n == 0 then Right(())
    else
      val tuples = sigBuf.toArray
      val allValid = java.util.stream.IntStream
        .range(0, n)
        .parallel()
        .allMatch { i =>
          val (pk, msg, sig) = tuples(i)
          Ed25519.verify(pk, msg, sig)
        }
      if !allValid then Left(ReportErrorCode.BadSignature) else Right(())

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
    boundary:
      val recentBlocks = preState.recentBlocks
      val packageHashes = guarantees.map(_.report.packageSpec.hash)

      // Check for duplicates within batch
      if packageHashes.distinct.size != packageHashes.size then
        break(Left(ReportErrorCode.DuplicatePackage))

      val historyReported: Map[Hash, Hash] =
        recentBlocks.history.flatMap(_.reported.map(r => r.hash -> r.exportsRoot)).toMap
      val historyHashes = historyReported.keySet
      val availHashes = preState.availAssignments.flatten.map(_.report.packageSpec.hash).toSet
      val allPipelinedHashes = historyHashes ++
        preState.readyQueuePackageHashes ++
        preState.accumulatedPackageHashes ++
        availHashes ++
        input.knownPackages
      if packageHashes.exists(allPipelinedHashes.contains) then
        break(Left(ReportErrorCode.DuplicatePackage))

      // Build lookup for current batch packages
      val batchPackages = guarantees.map(g => g.report.packageSpec.hash -> g.report.packageSpec.exportsRoot).toMap

      // Validate segment root lookups
      for
        guarantee <- guarantees
        lookup <- guarantee.report.segmentRootLookup
      do
        val validLookup = batchPackages.get(lookup.workPackageHash) match
          case Some(exportsRoot) => lookup.segmentTreeRoot == exportsRoot
          case None => historyReported.get(lookup.workPackageHash).contains(lookup.segmentTreeRoot)
        if !validLookup then
          break(Left(ReportErrorCode.SegmentRootLookupInvalid))

      // Validate prerequisites
      val batchHashSet = packageHashes.toSet
      for
        guarantee <- guarantees
        prerequisite <- guarantee.report.context.prerequisites
      do
        val exists = batchHashSet.contains(prerequisite) ||
          historyReported.contains(prerequisite)
        if !exists then
          break(Left(ReportErrorCode.DependencyMissing))

      Right(())

  /**
   * Validate lookup anchor slot age.
   */
  private def validateAnchorAge(
    guarantees: List[GuaranteeExtrinsic],
    currentSlot: Long,
    config: ChainConfig
  ): ValidationResult =
    boundary:
      for guarantee <- guarantees do
        val lookupAnchorSlot = guarantee.report.context.lookupAnchorSlot.value.toLong
        if lookupAnchorSlot > currentSlot || currentSlot - lookupAnchorSlot > config.maxLookupAnchorAge then
          break(Left(ReportErrorCode.LookupAnchorNotRecent))
      Right(())

  /**
   * Validate anchor recency and context.
   */
  private def validateAnchor(
    guarantees: List[GuaranteeExtrinsic],
    recentBlocks: HistoricalBetaContainer,
    ancestry: List[AncestorHeader],
    currentSlot: Long,
    config: ChainConfig,
    skipLookupAnchor: Boolean
  ): ValidationResult =
    val batchPackages = guarantees.map(g => g.report.packageSpec.hash -> g.report.packageSpec.exportsRoot).toMap
    val historyReported: Map[Hash, Hash] =
      recentBlocks.history.flatMap(_.reported.map(r => r.hash -> r.exportsRoot)).toMap

    boundary:
      for guarantee <- guarantees do
        val context = guarantee.report.context

        val lookupAnchorPresent =
          skipLookupAnchor || {
            val lookupAnchorSlot = context.lookupAnchorSlot.value.toLong
            if ancestry.nonEmpty then
              ancestry.exists(a => a.headerHash == context.lookupAnchor && a.slot == lookupAnchorSlot)
            else
              recentBlocks.history.exists(_.headerHash == context.lookupAnchor) &&
                lookupAnchorSlot <= currentSlot &&
                currentSlot - lookupAnchorSlot <= config.maxLookupAnchorAge
          }
        if !lookupAnchorPresent then
          break(Left(ReportErrorCode.LookupAnchorNotRecent))

        // Find and validate anchor block (gp: within recent history β = last Crecenthistorylen blocks)
        val anchorBlock = recentBlocks.history.find(_.headerHash == context.anchor)
        if anchorBlock.isEmpty then
          break(Left(ReportErrorCode.AnchorNotRecent))

        val anchor = anchorBlock.get
        if anchor.stateRoot != context.stateRoot then
          break(Left(ReportErrorCode.BadStateRoot))
        if anchor.beefyRoot != context.beefyRoot then
          break(Left(ReportErrorCode.BadBeefyMmrRoot))

        // Validate prerequisites with segment root consistency
        for prerequisite <- context.prerequisites do
          val existsInBatch = batchPackages.get(prerequisite).exists { exportsRoot =>
            guarantee.report.segmentRootLookup.forall(lookup =>
              lookup.workPackageHash != prerequisite || lookup.segmentTreeRoot == exportsRoot
            )
          }
          val existsInHistory = historyReported.get(prerequisite).exists { exportsRoot =>
            guarantee.report.segmentRootLookup.forall(lookup =>
              lookup.workPackageHash != prerequisite || lookup.segmentTreeRoot == exportsRoot
            )
          }
          if !existsInBatch && !existsInHistory then
            break(Left(ReportErrorCode.DependencyMissing))

      Right(())

  /**
   * Validate work report.
   */
  private def validateWorkReport(
    workReport: WorkReport,
    guaranteeSlot: Long,
    currentSlot: Long,
    accountsById: Map[Long, ServiceAccount],
    authPools: List[List[Hash]],
    availAssignments: List[Option[AvailabilityAssignment]],
    config: ChainConfig
  ): ValidationResult =
    for
      _ <- ensure(guaranteeSlot <= currentSlot, ReportErrorCode.FutureReportSlot)
      _ <- ensure(workReport.results.nonEmpty, ReportErrorCode.MissingWorkResults)
      _ <- ensure(workReport.results.length <= config.maxWorkItems, ReportErrorCode.WorkReportTooBig)
      _ <- ensure(availAssignments.lift(workReport.coreIndex.toInt).flatten.isEmpty, ReportErrorCode.CoreEngaged)
      _ <- validateOutputSize(workReport)
      _ <- {
        // ULong sum: signed-Long sum can wrap and falsely satisfy the bound.
        val totalAccGas = workReport.results.foldLeft(ULong(0L)) { (acc, r) =>
          acc + ULong(r.accumulateGas.toLong)
        }
        ensure(totalAccGas <= ULong(config.reportAccGas), ReportErrorCode.WorkReportGasTooHigh)
      }
      _ <- ensure(workReport.coreIndex.toInt < config.coresCount, ReportErrorCode.BadCoreIndex)
      _ <- validateAuthorizer(workReport, authPools)
      _ <- validateWorkResults(workReport, accountsById)
      _ <- ensure(
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
            if result.accumulateGas.toLong < account.data.service.minItemGas then
              break(Left(ReportErrorCode.ServiceItemGasTooLow))
      Right(())

  /** Validate guarantor signature order (must be sorted and unique by validator index). */
  private def validateGuarantorSignatureOrder(guarantee: GuaranteeExtrinsic): ValidationResult =
    val isSortedUnique = ValidationHelpers.isSortedUniqueByInt(guarantee.signatures)(_.validatorIndex.toInt)
    ensure(isSortedUnique, ReportErrorCode.NotSortedOrUniqueGuarantors)

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
    boundary:
      val sigCount = guarantee.signatures.length
      if sigCount < 2 || sigCount > 3 then
        break(Left(ReportErrorCode.InsufficientGuarantees))

      val ctx = computeRotationContext(guarantee.slot.value.toLong, currentSlot, config)
      if ctx.reportRotation < ctx.currentRotation - 1 then
        break(Left(ReportErrorCode.ReportEpochBeforeLast))
      if ctx.reportRotation > ctx.currentRotation then
        break(Left(ReportErrorCode.FutureReportSlot))

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
          break(Left(ReportErrorCode.BadValidatorIndex))

        val validatorEd25519 = validatorsArr(idx).ed25519
        if offendersSet.contains(Hash(validatorEd25519.bytes)) then
          break(Left(ReportErrorCode.BannedValidator))
        if coreAssignments(idx) != reportedCore then
          break(Left(ReportErrorCode.WrongAssignment))
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
    guarantees: List[GuaranteeExtrinsic],
    currentSlot: Long
  ): List[Option[AvailabilityAssignment]] =
    val guaranteesByCore = guarantees.map(g => g.report.coreIndex.toInt -> g).toMap
    existing.zipWithIndex.map {
      case (existing, index) =>
        guaranteesByCore.get(index).map(AvailabilityAssignment(_, currentSlot)).orElse(existing)
    }
