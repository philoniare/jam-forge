package io.forge.jam.protocol.report

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{
  Hash,
  ServiceId,
  ValidatorIndex,
  Timeslot,
  Ed25519Signature,
  Gas,
  CoreIndex
}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.types.work.{ExecutionResult, PackageSpec}
import io.forge.jam.core.types.workresult.{WorkResult, RefineLoad}
import io.forge.jam.core.types.context.Context
import io.forge.jam.protocol.generators.StfGenerators.*
import io.forge.jam.protocol.report.ReportTypes.*
import io.forge.jam.protocol.report.ReportTransition
import spire.math.{UInt, UShort}

/**
 * - Work report processing validates all required fields
 * - Report accumulation follows ordering constraints
 */
class ReportsSTFSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  private val testConfig = ChainConfig.TINY

  // Override default ScalaCheck configuration for faster tests
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  private def genReportInput(config: ChainConfig): Gen[ReportInput] =
    for
      slot <- Gen.choose(1L, 1000L)
      knownPackagesCount <- Gen.choose(0, 3)
      knownPackages <- Gen.listOfN(knownPackagesCount, genHash)
    yield ReportInput(
      guarantees = List.empty, // Empty guarantees for property testing
      slot = slot,
      knownPackages = knownPackages
    )

  private def genCoreStatisticsRecord: Gen[CoreStatisticsRecord] =
    for
      daLoad <- Gen.choose(0L, 1000L)
      popularity <- Gen.choose(0L, 1000L)
      imports <- Gen.choose(0L, 100L)
      extrinsicCount <- Gen.choose(0L, 100L)
      extrinsicSize <- Gen.choose(0L, 10000L)
      exports <- Gen.choose(0L, 100L)
      bundleSize <- Gen.choose(0L, 10000L)
      gasUsed <- Gen.choose(0L, 100000L)
    yield CoreStatisticsRecord(
      daLoad,
      popularity,
      imports,
      extrinsicCount,
      extrinsicSize,
      exports,
      bundleSize,
      gasUsed
    )

  private def genReportState(config: ChainConfig): Gen[ReportState] =
    for
      availAssignments <- Gen.listOfN(config.coresCount, Gen.option(genAvailabilityAssignment(config)))
      currValidators <- genValidatorKeys(config.validatorCount)
      prevValidators <- genValidatorKeys(config.validatorCount)
      entropy <- Gen.listOfN(4, genHash)
      offendersCount <- Gen.choose(0, 3)
      offenders <- Gen.listOfN(offendersCount, genHash)
      recentBlocks <- genHistoricalBetaContainer
      authPools <- Gen.listOfN(config.coresCount, Gen.listOfN(Gen.choose(0, 5).sample.get, genHash))
      coresStatistics <- Gen.listOfN(config.coresCount, genCoreStatisticsRecord)
    yield ReportState(
      availAssignments = availAssignments,
      currValidators = currValidators,
      prevValidators = prevValidators,
      entropy = entropy,
      offenders = offenders,
      recentBlocks = recentBlocks,
      authPools = authPools,
      accounts = List.empty, // Empty accounts for basic property testing
      coresStatistics = coresStatistics,
      servicesStatistics = List.empty
    )

  private val zeroHash: Hash = Hash(Array.fill(32)(0.toByte))

  private def hashOf(b: Int): Hash = Hash(Array.fill(32)(b.toByte))

  /** A well-formed-enough work report; individual fields are overridden per test. */
  private def buildWorkReport(
    coreIndex: Int = 0,
    packageHash: Hash = hashOf(0x11),
    lookupAnchorSlot: Int = 1,
    results: List[WorkResult] = List(
      WorkResult(
        serviceId = ServiceId(42),
        codeHash = hashOf(0x09),
        payloadHash = hashOf(0x0a),
        accumulateGas = Gas(1000),
        result = ExecutionResult.Ok(JamBytes(Array.emptyByteArray)),
        refineLoad = RefineLoad(Gas(100), UShort(0), UShort(0), UInt(0), UShort(0))
      )
    ),
    prerequisites: List[Hash] = List.empty,
    segmentRootLookup: List[io.forge.jam.core.types.workpackage.SegmentRootLookup] = List.empty
  ): WorkReport =
    WorkReport(
      packageSpec = PackageSpec(
        hash = packageHash,
        length = UInt(1000),
        erasureRoot = hashOf(0x02),
        exportsRoot = hashOf(0x03),
        exportsCount = UShort(1)
      ),
      context = Context(
        anchor = hashOf(0x04),
        stateRoot = hashOf(0x05),
        beefyRoot = hashOf(0x06),
        lookupAnchor = hashOf(0x07),
        lookupAnchorSlot = Timeslot(lookupAnchorSlot),
        prerequisites = prerequisites
      ),
      coreIndex = CoreIndex(coreIndex),
      authorizerHash = hashOf(0x08),
      authGasUsed = Gas(0),
      authOutput = JamBytes(Array.emptyByteArray),
      segmentRootLookup = segmentRootLookup,
      results = results
    )

  /** A guarantee with `sigCount` credentials over the given report. */
  private def buildGuarantee(report: WorkReport, slot: Int, sigCount: Int): GuaranteeExtrinsic =
    GuaranteeExtrinsic(
      report = report,
      slot = Timeslot(slot),
      signatures = (0 until sigCount).toList.map(i =>
        GuaranteeSignature(ValidatorIndex(i), Ed25519Signature(Array.fill(64)(0.toByte)))
      )
    )

  private val reportAuthorizerHash: Hash = hashOf(0x08)
  private val reportServiceId: Long = 42L
  private val reportCodeHash: Hash = hashOf(0x09)

  private def minimalState(config: ChainConfig): ReportState =
    val emptyValidatorKey = io.forge.jam.core.types.epoch.ValidatorKey(
      bandersnatch = io.forge.jam.core.primitives.BandersnatchPublicKey(Array.fill(32)(0.toByte)),
      ed25519 = io.forge.jam.core.primitives.Ed25519PublicKey(Array.fill(32)(0.toByte)),
      bls = io.forge.jam.core.primitives.BlsPublicKey(Array.fill(144)(0.toByte)),
      metadata = JamBytes.zeros(128)
    )
    val account = io.forge.jam.core.types.service.ServiceAccount(
      id = reportServiceId,
      data = io.forge.jam.core.types.service.ServiceData(
        io.forge.jam.core.types.service.ServiceInfo(
          codeHash = reportCodeHash,
          balance = 0L,
          minItemGas = 0L,
          minMemoGas = 0L,
          bytesUsed = 0L,
          items = 0
        )
      )
    )
    ReportState(
      availAssignments = List.fill(config.coresCount)(None),
      currValidators = List.fill(config.validatorCount)(emptyValidatorKey),
      prevValidators = List.fill(config.validatorCount)(emptyValidatorKey),
      entropy = List.fill(4)(zeroHash),
      offenders = List.empty,
      recentBlocks =
        io.forge.jam.core.types.history.HistoricalBetaContainer(
          List.empty,
          io.forge.jam.core.types.history.HistoricalMmr(List.empty)
        ),
      authPools = List.fill(config.coresCount)(List(reportAuthorizerHash)),
      accounts = List(account),
      coresStatistics = List.fill(config.coresCount)(CoreStatisticsRecord()),
      servicesStatistics = List.empty
    )

  /**
   * Add the given package hashes to recent history as "reported" packages so
   * they count as satisfied prerequisites (otherwise validateNoDuplicatePackages
   * rejects unknown prerequisites with DependencyMissing before the per-report
   * dependency-count check is reached).
   */
  private def withHistoryReported(state: ReportState, packages: List[Hash]): ReportState =
    val reported = packages.map(h =>
      io.forge.jam.core.types.history.ReportedWorkPackage(hash = h, exportsRoot = zeroHash)
    )
    val beta = io.forge.jam.core.types.history.HistoricalBeta(
      headerHash = hashOf(0x55),
      beefyRoot = zeroHash,
      stateRoot = zeroHash,
      reported = reported
    )
    state.copy(recentBlocks =
      state.recentBlocks.copy(history = List(beta))
    )

  private def runStf(input: ReportInput, state: ReportState): ReportOutput =
    ReportTransition.stfInternal(input, state, testConfig, skipAncestryValidation = true)._2

  test("property: availAssignments size matches cores count") {
    forAll(genReportState(testConfig)) { state =>
      // Property: availAssignments list size should equal cores count
      state.availAssignments.size shouldBe testConfig.coresCount
    }
  }

  test("property: currValidators size matches validator count") {
    forAll(genReportState(testConfig)) { state =>
      // Property: currValidators list size should equal validator count
      state.currValidators.size shouldBe testConfig.validatorCount
    }
  }

  test("property: prevValidators size matches validator count") {
    forAll(genReportState(testConfig)) { state =>
      // Property: prevValidators list size should equal validator count
      state.prevValidators.size shouldBe testConfig.validatorCount
    }
  }

  test("property: entropy list has exactly 4 elements") {
    forAll(genReportState(testConfig)) { state =>
      // Property: entropy should have exactly 4 hashes (eta[0..3])
      state.entropy.size shouldBe 4
    }
  }

  test("property: authPools size matches cores count") {
    forAll(genReportState(testConfig)) { state =>
      // Property: authPools list size should equal cores count
      state.authPools.size shouldBe testConfig.coresCount
    }
  }

  test("property: coresStatistics size matches cores count") {
    forAll(genReportState(testConfig)) { state =>
      // Property: coresStatistics list size should equal cores count
      state.coresStatistics.size shouldBe testConfig.coresCount
    }
  }

  test("property: generated work reports have valid core indices") {
    forAll(genWorkReport(testConfig)) { report =>
      // Property: core index should be within valid range
      report.coreIndex.toInt should be < testConfig.coresCount
      report.coreIndex.toInt should be >= 0
    }
  }

  test("property: work report package spec has valid hash size") {
    forAll(genWorkReport(testConfig)) { report =>
      // Property: package hash should be 32 bytes
      report.packageSpec.hash.size shouldBe Hash.Size
      report.packageSpec.erasureRoot.size shouldBe Hash.Size
      report.packageSpec.exportsRoot.size shouldBe Hash.Size
    }
  }

  test("property: work report context has valid hash sizes") {
    forAll(genWorkReport(testConfig)) { report =>
      // Property: context hashes should be 32 bytes
      report.context.anchor.size shouldBe Hash.Size
      report.context.stateRoot.size shouldBe Hash.Size
      report.context.beefyRoot.size shouldBe Hash.Size
      report.context.lookupAnchor.size shouldBe Hash.Size
    }
  }

  test("property: work report has at least one result") {
    forAll(genWorkReport(testConfig)) { report =>
      // Property: work report should have at least one work result
      report.results.nonEmpty shouldBe true
    }
  }

  test("property: empty guarantees input leaves availability assignments unchanged") {
    forAll(genReportState(testConfig)) { preState =>
      val input = ReportInput(
        guarantees = List.empty,
        slot = 100L,
        knownPackages = List.empty
      )

      val (postState, output) = ReportTransition.stfInternal(input, preState, testConfig)

      output.isRight shouldBe true
      postState.availAssignments shouldBe preState.availAssignments
    }
  }

  test("property: known packages list contains valid hashes") {
    forAll(genReportInput(testConfig)) { input =>
      input.knownPackages.foreach { hash =>
        // Property: each known package hash should be 32 bytes
        hash.size shouldBe Hash.Size
      }
    }
  }

  test("property: core statistics record has non-negative values") {
    forAll(genCoreStatisticsRecord) { stats =>
      // Property: all statistics values should be non-negative
      stats.daLoad should be >= 0L
      stats.popularity should be >= 0L
      stats.imports should be >= 0L
      stats.extrinsicCount should be >= 0L
      stats.extrinsicSize should be >= 0L
      stats.exports should be >= 0L
      stats.bundleSize should be >= 0L
      stats.gasUsed should be >= 0L
    }
  }

  test("property: zero core statistics record is valid") {
    val zeroStats = CoreStatisticsRecord.zero

    // Property: zero record should have all zeros
    zeroStats.daLoad shouldBe 0L
    zeroStats.popularity shouldBe 0L
    zeroStats.imports shouldBe 0L
    zeroStats.extrinsicCount shouldBe 0L
    zeroStats.extrinsicSize shouldBe 0L
    zeroStats.exports shouldBe 0L
    zeroStats.bundleSize shouldBe 0L
    zeroStats.gasUsed shouldBe 0L
  }

  test("property: availability assignments have valid timeout") {
    forAll(genAvailabilityAssignment(testConfig)) { assignment =>
      // Property: timeout should be a positive value
      assignment.timeout should be > 0L
    }
  }

  test("property: availability assignment contains valid work report") {
    forAll(genAvailabilityAssignment(testConfig)) { assignment =>
      // Property: work report core index should be valid
      assignment.report.coreIndex.toInt should be < testConfig.coresCount
      assignment.report.coreIndex.toInt should be >= 0

      // Property: work report should have results
      assignment.report.results.nonEmpty shouldBe true
    }
  }

  test("property: all report error codes are defined") {
    // Property: verify error codes exist
    val errorCodes = List(
      ReportErrorCode.BadCoreIndex,
      ReportErrorCode.FutureReportSlot,
      ReportErrorCode.ReportEpochBeforeLast,
      ReportErrorCode.InsufficientGuarantees,
      ReportErrorCode.OutOfOrderGuarantee,
      ReportErrorCode.NotSortedOrUniqueGuarantors,
      ReportErrorCode.WrongAssignment,
      ReportErrorCode.CoreEngaged,
      ReportErrorCode.AnchorNotRecent,
      ReportErrorCode.BadServiceId,
      ReportErrorCode.BadCodeHash,
      ReportErrorCode.DependencyMissing,
      ReportErrorCode.DuplicatePackage,
      ReportErrorCode.BadStateRoot,
      ReportErrorCode.BadBeefyMmrRoot,
      ReportErrorCode.CoreUnauthorized,
      ReportErrorCode.BadValidatorIndex,
      ReportErrorCode.WorkReportGasTooHigh,
      ReportErrorCode.ServiceItemGasTooLow,
      ReportErrorCode.TooManyDependencies,
      ReportErrorCode.SegmentRootLookupInvalid,
      ReportErrorCode.BadSignature,
      ReportErrorCode.WorkReportTooBig,
      ReportErrorCode.BannedValidator,
      ReportErrorCode.LookupAnchorNotRecent,
      ReportErrorCode.MissingWorkResults,
      ReportErrorCode.DuplicateGuarantors
    )

    errorCodes.size shouldBe 27
  }

  test("GP: stfInternal with empty guarantees returns success") {
    forAll(genReportState(testConfig)) { preState =>
      val input = ReportInput(
        guarantees = List.empty,
        slot = 100L,
        knownPackages = List.empty
      )

      val (postState, output) = ReportTransition.stfInternal(input, preState, testConfig)

      // GP: Empty guarantees should succeed
      output.isRight shouldBe true

      // GP: No reports should be processed
      output.toOption.get.reported shouldBe empty
      output.toOption.get.reporters shouldBe empty

      // GP: availAssignments size preserved
      postState.availAssignments.size shouldBe testConfig.coresCount
    }
  }

  test("GP: stfInternal is deterministic (same input produces same output)") {
    forAll(genReportState(testConfig)) { preState =>
      val input = ReportInput(
        guarantees = List.empty,
        slot = 100L,
        knownPackages = List.empty
      )

      val (postState1, output1) = ReportTransition.stfInternal(input, preState, testConfig)
      val (postState2, output2) = ReportTransition.stfInternal(input, preState, testConfig)

      // GP: Same inputs produce identical outputs
      output1 shouldBe output2
      postState1.availAssignments.size shouldBe postState2.availAssignments.size
      postState1.currValidators.size shouldBe postState2.currValidators.size
    }
  }

  test("GP: stfInternal preserves validator sets") {
    forAll(genReportState(testConfig)) { preState =>
      val input = ReportInput(
        guarantees = List.empty,
        slot = 100L,
        knownPackages = List.empty
      )

      val (postState, output) = ReportTransition.stfInternal(input, preState, testConfig)

      whenever(output.isRight) {
        // GP: currValidators must be preserved
        postState.currValidators.size shouldBe preState.currValidators.size
        postState.currValidators.zip(preState.currValidators).foreach { case (post, pre) =>
          post.bandersnatch.bytes shouldBe pre.bandersnatch.bytes
          post.ed25519.bytes shouldBe pre.ed25519.bytes
        }

        // GP: prevValidators must be preserved
        postState.prevValidators.size shouldBe preState.prevValidators.size
      }
    }
  }

  test("GP: unsorted guarantor credentials are rejected (NotSortedOrUniqueGuarantors)") {
    val report = buildWorkReport(coreIndex = 0)
    val guarantee = GuaranteeExtrinsic(
      report = report,
      slot = Timeslot(10),
      signatures = List(
        GuaranteeSignature(ValidatorIndex(1), Ed25519Signature(Array.fill(64)(0.toByte))),
        GuaranteeSignature(ValidatorIndex(0), Ed25519Signature(Array.fill(64)(0.toByte)))
      )
    )
    val input = ReportInput(List(guarantee), slot = 10L)
    runStf(input, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.NotSortedOrUniqueGuarantors)
  }

  test("GP: report accumulate gas above limit is rejected (WorkReportGasTooHigh)") {
    val report = buildWorkReport(
      coreIndex = 0,
      results = List(
        WorkResult(
          serviceId = ServiceId(42),
          codeHash = hashOf(0x09),
          payloadHash = hashOf(0x0a),
          accumulateGas = Gas(testConfig.reportAccGas + 1L),
          result = ExecutionResult.Ok(JamBytes(Array.emptyByteArray)),
          refineLoad = RefineLoad(Gas(100), UShort(0), UShort(0), UInt(0), UShort(0))
        )
      )
    )
    val input = ReportInput(List(buildGuarantee(report, slot = 10, sigCount = 2)), slot = 10L)
    runStf(input, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.WorkReportGasTooHigh)
  }

  test("GP: dependency count at and above the limit (TooManyDependencies boundary)") {
    // GP: |prerequisites| + |segmentRootLookup| <= maxDependencies.
    // Seed every prerequisite into recent history so it is a satisfied dependency;
    // the only thing that varies between the two cases is the dependency *count*.
    val maxDeps = testConfig.maxDependencies
    val overPrereqs = (0 to maxDeps).toList.map(hashOf) // maxDeps + 1 entries
    val seeded = withHistoryReported(minimalState(testConfig), overPrereqs)

    // Exactly maxDeps prerequisites passes the dependency-count gate (it fails
    // later on signature verification, never TooManyDependencies).
    val atLimit = buildWorkReport(prerequisites = overPrereqs.take(maxDeps))
    val atLimitOut = runStf(ReportInput(List(buildGuarantee(atLimit, 10, 2)), 10L), seeded)
    atLimitOut.swap.toOption should not be Some(ReportErrorCode.TooManyDependencies)

    // One over the limit is rejected with TooManyDependencies.
    val overLimit = buildWorkReport(prerequisites = overPrereqs)
    val overLimitOut = runStf(ReportInput(List(buildGuarantee(overLimit, 10, 2)), 10L), seeded)
    overLimitOut.swap.toOption shouldBe Some(ReportErrorCode.TooManyDependencies)
  }

  test("GP: report from before the previous rotation is rejected (ReportEpochBeforeLast)") {
    val rp = testConfig.rotationPeriod
    val currentSlot = 100L
    val staleSlot = currentSlot - (rp.toLong * 2L) // two rotations earlier
    // Keep the lookup anchor inside Cmaxlookupanchorage of the current slot so
    // validateAnchorAge passes and the rotation-window check is what fires.
    val report = buildWorkReport(coreIndex = 0, lookupAnchorSlot = (currentSlot - 1L).toInt)
    val input = ReportInput(List(buildGuarantee(report, staleSlot.toInt, sigCount = 2)), currentSlot)
    runStf(input, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.ReportEpochBeforeLast)
  }

  test("GP: credential count outside 2-3 is rejected (InsufficientGuarantees)") {
    // GP: each guarantee must carry between 2 and 3 credentials.
    val report = buildWorkReport(coreIndex = 0)

    // 1 credential -> InsufficientGuarantees
    val one = ReportInput(List(buildGuarantee(report, 10, sigCount = 1)), 10L)
    runStf(one, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.InsufficientGuarantees)

    // 4 credentials -> InsufficientGuarantees
    val four = ReportInput(List(buildGuarantee(report, 10, sigCount = 4)), 10L)
    runStf(four, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.InsufficientGuarantees)

    val two = ReportInput(List(buildGuarantee(report, 10, sigCount = 2)), 10L)
    runStf(two, minimalState(testConfig)).swap.toOption should not be
      Some(ReportErrorCode.InsufficientGuarantees)
  }

  test("GP: a rejected guarantee leaves the pre-state unchanged (atomicity)") {
    val pre = minimalState(testConfig).copy(
      coresStatistics = List(
        CoreStatisticsRecord(daLoad = 7, gasUsed = 123),
        CoreStatisticsRecord(daLoad = 9, gasUsed = 456)
      )
    )
    val report = buildWorkReport(coreIndex = testConfig.coresCount) // out of range
    val input = ReportInput(List(buildGuarantee(report, 10, 2)), 10L)
    val (postState, output) =
      ReportTransition.stfInternal(input, pre, testConfig, skipAncestryValidation = true)
    output.isLeft shouldBe true
    postState shouldBe pre
    postState.coresStatistics shouldBe pre.coresStatistics
  }

  test("GP: out-of-order guarantees are rejected (OutOfOrderGuarantee)") {
    // GP: guarantees must be strictly ascending by core index. Two guarantees
    // for cores [1, 0] violate the ordering.
    val r0 = buildWorkReport(coreIndex = 0, packageHash = hashOf(0x21))
    val r1 = buildWorkReport(coreIndex = 1, packageHash = hashOf(0x22))
    val input = ReportInput(
      List(buildGuarantee(r1, 10, 2), buildGuarantee(r0, 10, 2)),
      slot = 10L
    )
    runStf(input, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.OutOfOrderGuarantee)
  }

  test("GP: core index >= cores count is rejected (BadCoreIndex)") {
    // GP: core index must be < C. TINY has 2 cores, so core index 2 is invalid.
    testConfig.coresCount shouldBe 2
    val report = buildWorkReport(coreIndex = testConfig.coresCount)
    val input = ReportInput(List(buildGuarantee(report, 10, 2)), 10L)
    runStf(input, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.BadCoreIndex)
  }

  test("GP: guarantee slot in the future is rejected (FutureReportSlot)") {
    // GP: a work report's guarantee slot must be <= the current block slot.
    val report = buildWorkReport(coreIndex = 0)
    val input = ReportInput(List(buildGuarantee(report, slot = 11, sigCount = 2)), slot = 10L)
    runStf(input, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.FutureReportSlot)
  }

  test("GP: a report with no work results is rejected (MissingWorkResults)") {
    // GP: a work report must carry at least one result.
    val report = buildWorkReport(coreIndex = 0, results = List.empty)
    val input = ReportInput(List(buildGuarantee(report, 10, 2)), 10L)
    runStf(input, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.MissingWorkResults)
  }

  test("GP: a report on an engaged core is rejected (CoreEngaged)") {
    // GP: no report may be placed on a core that already has a report pending
    // availability. Occupy core 0 in the pre-state, then submit a report for it.
    val base = minimalState(testConfig)
    val occupant = io.forge.jam.core.types.workpackage.AvailabilityAssignment(
      buildWorkReport(coreIndex = 0, packageHash = hashOf(0x99)),
      timeout = 10L
    )
    val engaged = base.copy(availAssignments = Some(occupant) :: base.availAssignments.tail)
    val report = buildWorkReport(coreIndex = 0)
    val input = ReportInput(List(buildGuarantee(report, 10, 2)), 10L)
    runStf(input, engaged).swap.toOption shouldBe Some(ReportErrorCode.CoreEngaged)
  }

  test("GP: a duplicate package hash is rejected (DuplicatePackage)") {
    // GP: a work package hash already present in known-packages (the derived
    // ρ/ξ/φ/recent union) must be rejected.
    val pkg = hashOf(0x77)
    val report = buildWorkReport(coreIndex = 0, packageHash = pkg)
    val input = ReportInput(List(buildGuarantee(report, 10, 2)), slot = 10L, knownPackages = List(pkg))
    runStf(input, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.DuplicatePackage)
  }

  test("GP: an authorizer absent from the pool is rejected (CoreUnauthorized)") {
    // GP: the report's authorizer hash must be present in the core's auth pool.
    // Use a pre-state whose pools do NOT contain the report's authorizer.
    val state = minimalState(testConfig).copy(
      authPools = List.fill(testConfig.coresCount)(List(hashOf(0xCC)))
    )
    val report = buildWorkReport(coreIndex = 0) // authorizerHash = 0x08, not in pool
    val input = ReportInput(List(buildGuarantee(report, 10, 2)), 10L)
    runStf(input, state).swap.toOption shouldBe Some(ReportErrorCode.CoreUnauthorized)
  }

  test("GP: a result below the service's min item gas is rejected (ServiceItemGasTooLow)") {
    // GP: each result's accumulate gas must be >= the service's min item gas.
    val highMinGasAccount = io.forge.jam.core.types.service.ServiceAccount(
      id = reportServiceId,
      data = io.forge.jam.core.types.service.ServiceData(
        io.forge.jam.core.types.service.ServiceInfo(
          codeHash = reportCodeHash,
          balance = 0L,
          minItemGas = 1_000_000L, // higher than the report's 1000
          minMemoGas = 0L,
          bytesUsed = 0L,
          items = 0
        )
      )
    )
    val state = minimalState(testConfig).copy(accounts = List(highMinGasAccount))
    val report = buildWorkReport(coreIndex = 0) // result accumulateGas = 1000
    val input = ReportInput(List(buildGuarantee(report, 10, 2)), 10L)
    runStf(input, state).swap.toOption shouldBe Some(ReportErrorCode.ServiceItemGasTooLow)
  }

  test("GP: an unknown service id is rejected (BadServiceId)") {
    // GP: every result's service id must exist in the accounts set.
    val state = minimalState(testConfig).copy(accounts = List.empty)
    val report = buildWorkReport(coreIndex = 0) // serviceId 42 not in empty accounts
    val input = ReportInput(List(buildGuarantee(report, 10, 2)), 10L)
    runStf(input, state).swap.toOption shouldBe Some(ReportErrorCode.BadServiceId)
  }

  test("GP: a mismatched code hash is rejected (BadCodeHash)") {
    // GP: a result's code hash must equal the service's stored code hash.
    val wrongCodeHashAccount = io.forge.jam.core.types.service.ServiceAccount(
      id = reportServiceId,
      data = io.forge.jam.core.types.service.ServiceData(
        io.forge.jam.core.types.service.ServiceInfo(
          codeHash = hashOf(0xDD), // report uses hashOf(0x09)
          balance = 0L,
          minItemGas = 0L,
          minMemoGas = 0L,
          bytesUsed = 0L,
          items = 0
        )
      )
    )
    val state = minimalState(testConfig).copy(accounts = List(wrongCodeHashAccount))
    val report = buildWorkReport(coreIndex = 0)
    val input = ReportInput(List(buildGuarantee(report, 10, 2)), 10L)
    runStf(input, state).swap.toOption shouldBe Some(ReportErrorCode.BadCodeHash)
  }

  test("GP: a report clears every structural check and reaches the assignment check (WrongAssignment)") {
    val report = buildWorkReport(coreIndex = 0)
    val input = ReportInput(List(buildGuarantee(report, 10, sigCount = 2)), 10L)
    runStf(input, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.WrongAssignment)
  }

  test("GP: lookup anchor too old is rejected (LookupAnchorNotRecent)") {
    // GP: lookup_anchor_slot >= current_slot - maxLookupAnchorage.
    val maxAnchorage = testConfig.maxLookupAnchorAge
    maxAnchorage should be > 0L
    // currentSlot - lookupAnchorSlot = 100 - 1 = 99 > 24 -> rejected.
    val report = buildWorkReport(coreIndex = 0, lookupAnchorSlot = 1)
    val input = ReportInput(List(buildGuarantee(report, slot = 100, sigCount = 2)), slot = 100L)
    runStf(input, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.LookupAnchorNotRecent)
  }
