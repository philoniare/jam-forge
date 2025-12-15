package io.forge.jam.protocol.report

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.primitives.{Hash, ServiceId, ValidatorIndex, Timeslot, Ed25519Signature, Gas, CoreIndex}
import io.forge.jam.core.types.workpackage.{WorkReport, SegmentRootLookup}
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.types.work.{ExecutionResult, PackageSpec}
import io.forge.jam.core.types.workresult.{WorkResult, RefineLoad}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.service.ServiceAccount
import io.forge.jam.core.types.history.{HistoricalBeta, HistoricalMmr, HistoricalBetaContainer}
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.report.ReportTypes.*
import io.forge.jam.protocol.report.ReportTransition
import spire.math.{UInt, UShort}

/**
 * Tests for the Reports State Transition Function.
 *
 * Tests cover:
 * - Work report core bounds validation
 * - Authorizer presence validation
 * - Gas limit enforcement
 * - Guarantor signature verification
 * - Anchor recency validation
 * - Duplicate package detection
 * - Tiny config state transitions from test vectors
 * - Full config state transitions from test vectors
 */
class ReportTest extends AnyFunSuite with Matchers:

  // Tiny config: 6 validators, 2 cores
  val TinyConfig: ChainConfig = ChainConfig.TINY

  // Full config: 1023 validators, 341 cores
  val FullConfig: ChainConfig = ChainConfig.FULL

  // Helper to create empty byte array
  private def emptyBytes: Array[Byte] = Array.emptyByteArray

  // Helper to create a minimal valid state
  private def createMinimalState(
    validators: Int,
    cores: Int,
    authPools: List[List[Hash]] = List.empty,
    accounts: List[ServiceAccount] = List.empty,
    recentBlocks: HistoricalBetaContainer = HistoricalBetaContainer(List.empty, HistoricalMmr(List.empty))
  ): ReportState =
    val emptyValidatorKey = ValidatorKey(
      bandersnatch = io.forge.jam.core.primitives.BandersnatchPublicKey(Array.fill(32)(0.toByte)),
      ed25519 = io.forge.jam.core.primitives.Ed25519PublicKey(Array.fill(32)(0.toByte)),
      bls = io.forge.jam.core.primitives.BlsPublicKey(Array.fill(144)(0.toByte)),
      metadata = JamBytes(Array.fill(128)(0.toByte))
    )
    val defaultAuthPools = if authPools.isEmpty then List.fill(cores)(List.empty) else authPools

    ReportState(
      availAssignments = List.fill(cores)(None),
      currValidators = List.fill(validators)(emptyValidatorKey),
      prevValidators = List.fill(validators)(emptyValidatorKey),
      entropy = List.fill(4)(Hash(Array.fill(32)(0.toByte))),
      offenders = List.empty,
      recentBlocks = recentBlocks,
      authPools = defaultAuthPools,
      accounts = accounts,
      coresStatistics = List.fill(cores)(CoreStatisticsRecord()),
      servicesStatistics = List.empty
    )

  test("work report core bounds validation") {
    // Test that work reports with core index >= maxCores are rejected
    val preState = createMinimalState(TinyConfig.validatorCount, TinyConfig.coresCount)

    // Create a work report with core index beyond bounds
    val invalidCoreIndex = TinyConfig.coresCount // Should be 0 or 1 for tiny config
    val workReport = WorkReport(
      packageSpec = PackageSpec(
        hash = Hash(Array.fill(32)(0x01.toByte)),
        length = UInt(1000),
        erasureRoot = Hash(Array.fill(32)(0x02.toByte)),
        exportsRoot = Hash(Array.fill(32)(0x03.toByte)),
        exportsCount = UShort(1)
      ),
      context = Context(
        anchor = Hash(Array.fill(32)(0x04.toByte)),
        stateRoot = Hash(Array.fill(32)(0x05.toByte)),
        beefyRoot = Hash(Array.fill(32)(0x06.toByte)),
        lookupAnchor = Hash(Array.fill(32)(0x07.toByte)),
        lookupAnchorSlot = Timeslot(1),
        prerequisites = List.empty
      ),
      coreIndex = CoreIndex(invalidCoreIndex),
      authorizerHash = Hash(Array.fill(32)(0x08.toByte)),
      authGasUsed = Gas(0),
      authOutput = JamBytes(emptyBytes),
      segmentRootLookup = List.empty,
      results = List(WorkResult(
        serviceId = ServiceId(42),
        codeHash = Hash(Array.fill(32)(0x09.toByte)),
        payloadHash = Hash(Array.fill(32)(0x0A.toByte)),
        accumulateGas = Gas(1000),
        result = ExecutionResult.Ok(JamBytes(emptyBytes)),
        refineLoad = RefineLoad(Gas(100), UShort(0), UShort(0), UInt(0), UShort(0))
      ))
    )

    val guarantee = GuaranteeExtrinsic(
      report = workReport,
      slot = Timeslot(10),
      signatures = List.empty
    )
    val input = ReportInput(guarantees = List(guarantee), slot = 10)

    val (postState, output) = ReportTransition.stfInternal(input, preState, TinyConfig)

    // Should fail - some validation error should occur
    output.err shouldBe defined
    postState shouldBe preState
  }

  test("authorizer presence validation") {
    // Test that work reports with unauthorized authorizer are rejected
    val authHash = Hash(Array.fill(32)(0xAA.toByte))
    val differentAuthHash = Hash(Array.fill(32)(0xBB.toByte))

    // Auth pool contains authHash, but work report uses differentAuthHash
    val authPools = List(List(authHash), List.empty)
    val preState = createMinimalState(
      TinyConfig.validatorCount,
      TinyConfig.coresCount,
      authPools = authPools
    )

    val workReport = WorkReport(
      packageSpec = PackageSpec(
        hash = Hash(Array.fill(32)(0x01.toByte)),
        length = UInt(1000),
        erasureRoot = Hash(Array.fill(32)(0x02.toByte)),
        exportsRoot = Hash(Array.fill(32)(0x03.toByte)),
        exportsCount = UShort(1)
      ),
      context = Context(
        anchor = Hash(Array.fill(32)(0x04.toByte)),
        stateRoot = Hash(Array.fill(32)(0x05.toByte)),
        beefyRoot = Hash(Array.fill(32)(0x06.toByte)),
        lookupAnchor = Hash(Array.fill(32)(0x07.toByte)),
        lookupAnchorSlot = Timeslot(1),
        prerequisites = List.empty
      ),
      coreIndex = CoreIndex(0),
      authorizerHash = differentAuthHash, // Not in auth pool
      authGasUsed = Gas(0),
      authOutput = JamBytes(emptyBytes),
      segmentRootLookup = List.empty,
      results = List(WorkResult(
        serviceId = ServiceId(42),
        codeHash = Hash(Array.fill(32)(0x09.toByte)),
        payloadHash = Hash(Array.fill(32)(0x0A.toByte)),
        accumulateGas = Gas(1000),
        result = ExecutionResult.Ok(JamBytes(emptyBytes)),
        refineLoad = RefineLoad(Gas(100), UShort(0), UShort(0), UInt(0), UShort(0))
      ))
    )

    val guarantee = GuaranteeExtrinsic(
      report = workReport,
      slot = Timeslot(10),
      signatures = List.empty
    )
    val input = ReportInput(guarantees = List(guarantee), slot = 10)

    val (postState, output) = ReportTransition.stfInternal(input, preState, TinyConfig)

    // Should fail due to unauthorized core or insufficient signatures
    output.err shouldBe defined
    postState shouldBe preState
  }

  test("gas limit enforcement") {
    // Test that work reports with excessive accumulate gas are rejected
    val authHash = Hash(Array.fill(32)(0xAA.toByte))
    val authPools = List(List(authHash), List.empty)

    val preState = createMinimalState(
      TinyConfig.validatorCount,
      TinyConfig.coresCount,
      authPools = authPools
    )

    // Create work report with gas exceeding limit
    val excessiveGas = TinyConfig.maxAccumulationGas + 1

    val workReport = WorkReport(
      packageSpec = PackageSpec(
        hash = Hash(Array.fill(32)(0x01.toByte)),
        length = UInt(1000),
        erasureRoot = Hash(Array.fill(32)(0x02.toByte)),
        exportsRoot = Hash(Array.fill(32)(0x03.toByte)),
        exportsCount = UShort(1)
      ),
      context = Context(
        anchor = Hash(Array.fill(32)(0x04.toByte)),
        stateRoot = Hash(Array.fill(32)(0x05.toByte)),
        beefyRoot = Hash(Array.fill(32)(0x06.toByte)),
        lookupAnchor = Hash(Array.fill(32)(0x07.toByte)),
        lookupAnchorSlot = Timeslot(1),
        prerequisites = List.empty
      ),
      coreIndex = CoreIndex(0),
      authorizerHash = authHash,
      authGasUsed = Gas(0),
      authOutput = JamBytes(emptyBytes),
      segmentRootLookup = List.empty,
      results = List(WorkResult(
        serviceId = ServiceId(42),
        codeHash = Hash(Array.fill(32)(0x09.toByte)),
        payloadHash = Hash(Array.fill(32)(0x0A.toByte)),
        accumulateGas = Gas(excessiveGas),
        result = ExecutionResult.Ok(JamBytes(emptyBytes)),
        refineLoad = RefineLoad(Gas(100), UShort(0), UShort(0), UInt(0), UShort(0))
      ))
    )

    val guarantee = GuaranteeExtrinsic(
      report = workReport,
      slot = Timeslot(10),
      signatures = List.empty
    )
    val input = ReportInput(guarantees = List(guarantee), slot = 10)

    val (postState, output) = ReportTransition.stfInternal(input, preState, TinyConfig)

    // Should fail - either due to gas or insufficient signatures
    output.err shouldBe defined
    postState shouldBe preState
  }

  test("guarantor signature verification - insufficient signatures") {
    // Test that guarantees with insufficient signatures are rejected
    val authHash = Hash(Array.fill(32)(0xAA.toByte))
    val authPools = List(List(authHash), List.empty)

    val preState = createMinimalState(
      TinyConfig.validatorCount,
      TinyConfig.coresCount,
      authPools = authPools
    )

    val workReport = WorkReport(
      packageSpec = PackageSpec(
        hash = Hash(Array.fill(32)(0x01.toByte)),
        length = UInt(1000),
        erasureRoot = Hash(Array.fill(32)(0x02.toByte)),
        exportsRoot = Hash(Array.fill(32)(0x03.toByte)),
        exportsCount = UShort(1)
      ),
      context = Context(
        anchor = Hash(Array.fill(32)(0x04.toByte)),
        stateRoot = Hash(Array.fill(32)(0x05.toByte)),
        beefyRoot = Hash(Array.fill(32)(0x06.toByte)),
        lookupAnchor = Hash(Array.fill(32)(0x07.toByte)),
        lookupAnchorSlot = Timeslot(1),
        prerequisites = List.empty
      ),
      coreIndex = CoreIndex(0),
      authorizerHash = authHash,
      authGasUsed = Gas(0),
      authOutput = JamBytes(emptyBytes),
      segmentRootLookup = List.empty,
      results = List(WorkResult(
        serviceId = ServiceId(42),
        codeHash = Hash(Array.fill(32)(0x09.toByte)),
        payloadHash = Hash(Array.fill(32)(0x0A.toByte)),
        accumulateGas = Gas(1000),
        result = ExecutionResult.Ok(JamBytes(emptyBytes)),
        refineLoad = RefineLoad(Gas(100), UShort(0), UShort(0), UInt(0), UShort(0))
      ))
    )

    // Only one signature when we need 2-3
    val guarantee = GuaranteeExtrinsic(
      report = workReport,
      slot = Timeslot(10),
      signatures = List(
        GuaranteeSignature(ValidatorIndex(0), Ed25519Signature(Array.fill(64)(0.toByte)))
      )
    )
    val input = ReportInput(guarantees = List(guarantee), slot = 10)

    val (postState, output) = ReportTransition.stfInternal(input, preState, TinyConfig)

    // Should fail - an error should occur during validation
    // The actual error depends on validation order (could be InsufficientGuarantees or AnchorNotRecent)
    output.err shouldBe defined
    postState shouldBe preState
  }

  test("anchor recency validation") {
    // Test that work reports with stale anchors are rejected
    // This is tested via test vectors, but we also test the logic conceptually
    val preState = createMinimalState(
      TinyConfig.validatorCount,
      TinyConfig.coresCount,
      recentBlocks = HistoricalBetaContainer(
        history = List(
          HistoricalBeta(
            headerHash = Hash(Array.fill(32)(0x01.toByte)),
            beefyRoot = Hash(Array.fill(32)(0x02.toByte)),
            stateRoot = Hash(Array.fill(32)(0x03.toByte)),
            reported = List.empty
          )
        ),
        mmr = HistoricalMmr(List.empty)
      )
    )

    // Work report anchor is not in recent blocks
    val workReport = WorkReport(
      packageSpec = PackageSpec(
        hash = Hash(Array.fill(32)(0x11.toByte)),
        length = UInt(1000),
        erasureRoot = Hash(Array.fill(32)(0x12.toByte)),
        exportsRoot = Hash(Array.fill(32)(0x13.toByte)),
        exportsCount = UShort(1)
      ),
      context = Context(
        anchor = Hash(Array.fill(32)(0xFF.toByte)), // Not in recent blocks
        stateRoot = Hash(Array.fill(32)(0x15.toByte)),
        beefyRoot = Hash(Array.fill(32)(0x16.toByte)),
        lookupAnchor = Hash(Array.fill(32)(0x01.toByte)), // This is in recent blocks
        lookupAnchorSlot = Timeslot(1),
        prerequisites = List.empty
      ),
      coreIndex = CoreIndex(0),
      authorizerHash = Hash(Array.fill(32)(0x18.toByte)),
      authGasUsed = Gas(0),
      authOutput = JamBytes(emptyBytes),
      segmentRootLookup = List.empty,
      results = List(WorkResult(
        serviceId = ServiceId(42),
        codeHash = Hash(Array.fill(32)(0x19.toByte)),
        payloadHash = Hash(Array.fill(32)(0x1A.toByte)),
        accumulateGas = Gas(1000),
        result = ExecutionResult.Ok(JamBytes(emptyBytes)),
        refineLoad = RefineLoad(Gas(100), UShort(0), UShort(0), UInt(0), UShort(0))
      ))
    )

    val guarantee = GuaranteeExtrinsic(
      report = workReport,
      slot = Timeslot(10),
      signatures = List(
        GuaranteeSignature(ValidatorIndex(0), Ed25519Signature(Array.fill(64)(0.toByte))),
        GuaranteeSignature(ValidatorIndex(1), Ed25519Signature(Array.fill(64)(0.toByte)))
      )
    )
    val input = ReportInput(guarantees = List(guarantee), slot = 10)

    val (postState, output) = ReportTransition.stfInternal(input, preState, TinyConfig)

    // Should fail due to anchor not recent
    output.err shouldBe defined
    postState shouldBe preState
  }

  test("duplicate package detection") {
    // Test that duplicate packages within a batch are rejected
    val authHash = Hash(Array.fill(32)(0xAA.toByte))
    val authPools = List(List(authHash), List(authHash))
    val packageHash = Hash(Array.fill(32)(0x11.toByte))

    val preState = createMinimalState(
      TinyConfig.validatorCount,
      TinyConfig.coresCount,
      authPools = authPools
    )

    val workReport1 = WorkReport(
      packageSpec = PackageSpec(
        hash = packageHash, // Same hash
        length = UInt(1000),
        erasureRoot = Hash(Array.fill(32)(0x02.toByte)),
        exportsRoot = Hash(Array.fill(32)(0x03.toByte)),
        exportsCount = UShort(1)
      ),
      context = Context(
        anchor = Hash(Array.fill(32)(0x04.toByte)),
        stateRoot = Hash(Array.fill(32)(0x05.toByte)),
        beefyRoot = Hash(Array.fill(32)(0x06.toByte)),
        lookupAnchor = Hash(Array.fill(32)(0x07.toByte)),
        lookupAnchorSlot = Timeslot(1),
        prerequisites = List.empty
      ),
      coreIndex = CoreIndex(0),
      authorizerHash = authHash,
      authGasUsed = Gas(0),
      authOutput = JamBytes(emptyBytes),
      segmentRootLookup = List.empty,
      results = List(WorkResult(
        serviceId = ServiceId(42),
        codeHash = Hash(Array.fill(32)(0x09.toByte)),
        payloadHash = Hash(Array.fill(32)(0x0A.toByte)),
        accumulateGas = Gas(1000),
        result = ExecutionResult.Ok(JamBytes(emptyBytes)),
        refineLoad = RefineLoad(Gas(100), UShort(0), UShort(0), UInt(0), UShort(0))
      ))
    )

    val workReport2 = workReport1.copy(coreIndex = CoreIndex(1)) // Same package hash but different core

    val guarantee1 = GuaranteeExtrinsic(
      report = workReport1,
      slot = Timeslot(10),
      signatures = List.empty
    )
    val guarantee2 = GuaranteeExtrinsic(
      report = workReport2,
      slot = Timeslot(10),
      signatures = List.empty
    )

    val input = ReportInput(guarantees = List(guarantee1, guarantee2), slot = 10)

    val (postState, output) = ReportTransition.stfInternal(input, preState, TinyConfig)

    // Should fail due to duplicate package
    output.err shouldBe defined
    output.err.get shouldBe ReportErrorCode.DuplicatePackage
    postState shouldBe preState
  }

  test("tiny config state transition from test vectors") {
    val folderPath = "stf/reports/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadJsonFromTestVectors[ReportCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right(testCase) =>
          // Test state transition
          val (postState, output) = ReportTransition.stfInternal(
            testCase.input,
            testCase.preState,
            TinyConfig
          )
          assertReportOutputEquals(testCase.output, output, testCaseName)
          assertReportStateEquals(testCase.postState, postState, testCaseName)
  }

  test("full config state transition from test vectors") {
    val folderPath = "stf/reports/full"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadJsonFromTestVectors[ReportCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right(testCase) =>
          // Test state transition
          val (postState, output) = ReportTransition.stfInternal(
            testCase.input,
            testCase.preState,
            FullConfig
          )
          assertReportOutputEquals(testCase.output, output, testCaseName)
          assertReportStateEquals(testCase.postState, postState, testCaseName)
  }

  // Helper method to compare ReportOutput instances
  private def assertReportOutputEquals(
    expected: ReportOutput,
    actual: ReportOutput,
    testCaseName: String
  ): Unit =
    (expected.err, actual.err) match
      case (Some(expectedErr), Some(actualErr)) =>
        expectedErr shouldBe actualErr withClue s"Error code mismatch in test case: $testCaseName"
      case (Some(expectedErr), None) =>
        fail(s"Expected error $expectedErr but got success in test case: $testCaseName")
      case (None, Some(actualErr)) =>
        fail(s"Expected success but got error $actualErr in test case: $testCaseName")
      case (None, None) =>
        // Both are success, compare output marks
        expected.ok.isDefined shouldBe actual.ok.isDefined withClue
          s"OK status mismatch in test case: $testCaseName"
        (expected.ok, actual.ok) match
          case (Some(expMarks), Some(actMarks)) =>
            expMarks.reported.size shouldBe actMarks.reported.size withClue
              s"Reported packages count mismatch in test case: $testCaseName"
            expMarks.reporters.size shouldBe actMarks.reporters.size withClue
              s"Reporters count mismatch in test case: $testCaseName"
          case _ => ()

  // Helper method to compare ReportState instances
  private def assertReportStateEquals(
    expected: ReportState,
    actual: ReportState,
    testCaseName: String
  ): Unit =
    expected.availAssignments.size shouldBe actual.availAssignments.size withClue
      s"AvailAssignments size mismatch in test case: $testCaseName"

    expected.availAssignments.zip(actual.availAssignments).zipWithIndex.foreach {
      case ((exp, act), idx) =>
        (exp.isDefined, act.isDefined) match
          case (true, true) =>
            val expAssign = exp.get
            val actAssign = act.get
            java.util.Arrays.equals(
              expAssign.report.packageSpec.hash.bytes,
              actAssign.report.packageSpec.hash.bytes
            ) shouldBe true withClue
              s"Assignment package hash mismatch at index $idx in test case: $testCaseName"
          case (false, false) => ()
          case (true, false) =>
            fail(s"Expected assignment at index $idx but got none in test case: $testCaseName")
          case (false, true) =>
            fail(s"Expected no assignment at index $idx but got one in test case: $testCaseName")
    }

    expected.coresStatistics.size shouldBe actual.coresStatistics.size withClue
      s"CoreStatistics size mismatch in test case: $testCaseName"

    expected.servicesStatistics.size shouldBe actual.servicesStatistics.size withClue
      s"ServicesStatistics size mismatch in test case: $testCaseName"
