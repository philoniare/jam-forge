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
 * Reports STF behaviour not exercised by the stf/reports vectors (ReportTest runs
 * them all): empty guarantees over random pre-states, the 4-credential upper bound,
 * and the β anchor-context checks with the ancestry feature off.
 */
class ReportsSTFSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  private val testConfig = ChainConfig.TINY

  // Override default ScalaCheck configuration for faster tests
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

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
        erasureShards = UShort(testConfig.validatorCount),
        exportsRoot = hashOf(0x03),
        // Segment count is the sum of the digest exports
        exportsCount = UShort(results.map(_.refineLoad.exports.toInt).sum)
      ),
      context = Context(
        anchor = hashOf(0x04),
        anchorSlot = Timeslot(0),
        stateRoot = hashOf(0x05),
        beefyRoot = hashOf(0x06),
        lookupAnchor = hashOf(0x07),
        lookupAnchorSlot = Timeslot(lookupAnchorSlot),
        lookupAnchorStateRoot = Hash.zero,
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
      currValidators = IndexedSeq.fill(config.validatorCount)(emptyValidatorKey),
      prevValidators = IndexedSeq.fill(config.validatorCount)(emptyValidatorKey),
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

  /** A pre-state whose β recent-history holds a single block usable as a report anchor. */
  private def stateWithAnchor(config: ChainConfig, anchor: Hash, stateRoot: Hash, beefyRoot: Hash): ReportState =
    val beta = io.forge.jam.core.types.history.HistoricalBeta(
      headerHash = anchor,
      beefyRoot = beefyRoot,
      stateRoot = stateRoot,
      slot = 0L,
      reported = List.empty
    )
    val s = minimalState(config)
    s.copy(recentBlocks = s.recentBlocks.copy(history = List(beta)))

  private def runStf(input: ReportInput, state: ReportState): ReportOutput =
    ReportTransition.stfInternal(input, state, testConfig, skipAncestryValidation = true, skipAnchorContext = true)._2

  test("GP: empty guarantees succeed with nothing reported and leave assignments and validator sets unchanged") {
    forAll(genReportState(testConfig)) { preState =>
      val input = ReportInput(
        guarantees = List.empty,
        slot = 100L,
        knownPackages = List.empty
      )

      val (postState, output) = ReportTransition.stfInternal(input, preState, testConfig)

      output.isRight shouldBe true
      output.toOption.get.reported shouldBe empty
      output.toOption.get.reporters shouldBe empty

      postState.availAssignments shouldBe preState.availAssignments

      postState.currValidators.size shouldBe preState.currValidators.size
      postState.currValidators.zip(preState.currValidators).foreach { case (post, pre) =>
        post.bandersnatch.bytes shouldBe pre.bandersnatch.bytes
        post.ed25519.bytes shouldBe pre.ed25519.bytes
      }
      postState.prevValidators.size shouldBe preState.prevValidators.size
    }
  }

  test("GP: more than 3 credentials is rejected (InsufficientGuarantees)") {
    // GP: each guarantee carries 2-3 credentials. The 1-credential case is
    // no_enough_guarantees-1; no vector exceeds the upper bound.
    val report = buildWorkReport(coreIndex = 0)

    val four = ReportInput(List(buildGuarantee(report, 10, sigCount = 4)), 10L)
    runStf(four, minimalState(testConfig)).swap.toOption shouldBe
      Some(ReportErrorCode.InsufficientGuarantees)

    val two = ReportInput(List(buildGuarantee(report, 10, sigCount = 2)), 10L)
    runStf(two, minimalState(testConfig)).swap.toOption should not be
      Some(ReportErrorCode.InsufficientGuarantees)
  }

  test("WR-10: β anchor-context checks run even when the ancestry feature is off") {
    val report = buildWorkReport(coreIndex = 0)
    val input = ReportInput(List(buildGuarantee(report, 10, 2)), 10L)

    def err(state: ReportState, skipCtx: Boolean): Option[ReportErrorCode] =
      ReportTransition
        .stfInternal(input, state, testConfig, skipAncestryValidation = true, skipAnchorContext = skipCtx)
        ._2
        .swap
        .toOption

    // (a) anchor absent from β -> AnchorNotRecent.
    err(minimalState(testConfig), skipCtx = false) shouldBe Some(ReportErrorCode.AnchorNotRecent)
    // (b) anchor present, wrong state root -> BadStateRoot.
    err(stateWithAnchor(testConfig, hashOf(0x04), hashOf(0xEE), hashOf(0x06)), skipCtx = false) shouldBe
      Some(ReportErrorCode.BadStateRoot)
    // (c) anchor present, wrong beefy root -> BadBeefyMmrRoot.
    err(stateWithAnchor(testConfig, hashOf(0x04), hashOf(0x05), hashOf(0xEE)), skipCtx = false) shouldBe
      Some(ReportErrorCode.BadBeefyMmrRoot)
    // (d) a fully valid β anchor clears the anchor checks (fails later, never on anchor).
    val good = err(stateWithAnchor(testConfig, hashOf(0x04), hashOf(0x05), hashOf(0x06)), skipCtx = false)
    good should not be Some(ReportErrorCode.AnchorNotRecent)
    good should not be Some(ReportErrorCode.BadStateRoot)
    good should not be Some(ReportErrorCode.BadBeefyMmrRoot)
    // (e) the test-only bypass still skips anchor-context entirely (guards runStf).
    err(minimalState(testConfig), skipCtx = true) should not be Some(ReportErrorCode.AnchorNotRecent)
  }
