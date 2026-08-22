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
import scodec.bits.ByteVector

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
      metadata = JamBytes.zeros(128)
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
    output.isLeft shouldBe true
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
    output.isLeft shouldBe true
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
    val excessiveGas = TinyConfig.reportAccGas + 1

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
    output.isLeft shouldBe true
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
    output.isLeft shouldBe true
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
    output.isLeft shouldBe true
    postState shouldBe preState
  }

  // ---------------------------------------------------------------------------
  // Lookup-anchor vs anchor validation
  // ---------------------------------------------------------------------------

  private val betaWithBlock01: HistoricalBetaContainer =
    HistoricalBetaContainer(
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

  private def anchorGuaranteeInput(
    anchor: Hash,
    lookupAnchor: Hash,
    lookupAnchorSlot: Int,
    blockSlot: Int
  ): ReportInput =
    val workReport = WorkReport(
      packageSpec = PackageSpec(
        hash = Hash(Array.fill(32)(0x11.toByte)),
        length = UInt(1000),
        erasureRoot = Hash(Array.fill(32)(0x12.toByte)),
        exportsRoot = Hash(Array.fill(32)(0x13.toByte)),
        exportsCount = UShort(1)
      ),
      context = Context(
        anchor = anchor,
        stateRoot = Hash(Array.fill(32)(0x15.toByte)),
        beefyRoot = Hash(Array.fill(32)(0x16.toByte)),
        lookupAnchor = lookupAnchor,
        lookupAnchorSlot = Timeslot(lookupAnchorSlot),
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
      slot = Timeslot(blockSlot),
      signatures = List(
        GuaranteeSignature(ValidatorIndex(0), Ed25519Signature(Array.fill(64)(0.toByte))),
        GuaranteeSignature(ValidatorIndex(1), Ed25519Signature(Array.fill(64)(0.toByte)))
      )
    )
    ReportInput(guarantees = List(guarantee), slot = blockSlot)

  test("lookup anchor too old yields LookupAnchorNotRecent (not AnchorNotRecent)") {
    // blockSlot - lookupAnchorSlot = 30 - 1 = 29 > maxLookupAnchorAge(TINY) = 24
    val preState = createMinimalState(
      TinyConfig.validatorCount, TinyConfig.coresCount, recentBlocks = betaWithBlock01)
    val input = anchorGuaranteeInput(
      anchor = Hash(Array.fill(32)(0x01.toByte)),
      lookupAnchor = Hash(Array.fill(32)(0x01.toByte)),
      lookupAnchorSlot = 1,
      blockSlot = 30)
    val (postState, output) = ReportTransition.stfInternal(input, preState, TinyConfig)
    output.swap.toOption shouldBe Some(ReportErrorCode.LookupAnchorNotRecent)
    postState shouldBe preState
  }

  test("lookup anchor absent from beta with no ancestor set yields LookupAnchorNotRecent") {
    val preState = createMinimalState(
      TinyConfig.validatorCount, TinyConfig.coresCount, recentBlocks = betaWithBlock01)
    val input = anchorGuaranteeInput(
      anchor = Hash(Array.fill(32)(0x01.toByte)),        // present in β
      lookupAnchor = Hash(Array.fill(32)(0xFF.toByte)),  // absent from β
      lookupAnchorSlot = 5,
      blockSlot = 10)
    // No ancestor set -> β-membership fallback -> lookup anchor missing.
    val (postState, output) = ReportTransition.stfInternal(input, preState, TinyConfig)
    output.swap.toOption shouldBe Some(ReportErrorCode.LookupAnchorNotRecent)
    postState shouldBe preState
  }

  test("lookup anchor found in ancestor set (deeper than beta) passes the lookup-anchor check") {
    val preState = createMinimalState(
      TinyConfig.validatorCount, TinyConfig.coresCount, recentBlocks = betaWithBlock01)
    val lookupAnchor = Hash(Array.fill(32)(0xFF.toByte)) // absent from β
    val input = anchorGuaranteeInput(
      anchor = Hash(Array.fill(32)(0xEE.toByte)),        // absent from β -> AnchorNotRecent once lookup passes
      lookupAnchor = lookupAnchor,
      lookupAnchorSlot = 5,
      blockSlot = 10)
    // Ancestor set records the lookup anchor by (slot, hash): the lookup-anchor check passes,
    // so the failure surfaces on the *anchor* — proving the ancestor set was consulted.
    val ancestry = List(AncestorHeader(5L, lookupAnchor))
    val (postState, output) =
      ReportTransition.stfInternal(input, preState, TinyConfig, skipAncestryValidation = false, ancestry)
    output.swap.toOption shouldBe Some(ReportErrorCode.AnchorNotRecent)
    postState shouldBe preState
  }

  test("ancestor-set lookup anchor with mismatched timeslot is rejected (LookupAnchorNotRecent)") {
    val preState = createMinimalState(
      TinyConfig.validatorCount, TinyConfig.coresCount, recentBlocks = betaWithBlock01)
    val lookupAnchor = Hash(Array.fill(32)(0xFF.toByte))
    val input = anchorGuaranteeInput(
      anchor = Hash(Array.fill(32)(0x01.toByte)),
      lookupAnchor = lookupAnchor,
      lookupAnchorSlot = 5,
      blockSlot = 10)
    // Same header hash but a different recorded slot: the spec requires both to match.
    val ancestry = List(AncestorHeader(6L, lookupAnchor))
    val (postState, output) =
      ReportTransition.stfInternal(input, preState, TinyConfig, skipAncestryValidation = false, ancestry)
    output.swap.toOption shouldBe Some(ReportErrorCode.LookupAnchorNotRecent)
    postState shouldBe preState
  }

  test("anchor absent from beta still yields AnchorNotRecent (anchor path unchanged)") {
    val preState = createMinimalState(
      TinyConfig.validatorCount, TinyConfig.coresCount, recentBlocks = betaWithBlock01)
    val input = anchorGuaranteeInput(
      anchor = Hash(Array.fill(32)(0xFF.toByte)),        // absent from β
      lookupAnchor = Hash(Array.fill(32)(0x01.toByte)),  // present in β -> lookup passes via fallback
      lookupAnchorSlot = 1,
      blockSlot = 10)
    val (postState, output) = ReportTransition.stfInternal(input, preState, TinyConfig)
    output.swap.toOption shouldBe Some(ReportErrorCode.AnchorNotRecent)
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
    output.isLeft shouldBe true
    output.left.toOption.get shouldBe ReportErrorCode.DuplicatePackage
    postState shouldBe preState
  }

  test("known-package duplicate detection (WR-1)") {
    val authHash = Hash(Array.fill(32)(0xAA.toByte))
    val authPools = List(List(authHash), List(authHash))
    val packageHash = Hash(Array.fill(32)(0x11.toByte))

    val preState = createMinimalState(
      TinyConfig.validatorCount,
      TinyConfig.coresCount,
      authPools = authPools
    )

    val workReport = WorkReport(
      packageSpec = PackageSpec(
        hash = packageHash,
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

    val guarantee = GuaranteeExtrinsic(
      report = workReport,
      slot = Timeslot(10),
      signatures = List.empty
    )

    val input = ReportInput(
      guarantees = List(guarantee),
      slot = 10,
      knownPackages = List(packageHash)
    )

    val (postState, output) = ReportTransition.stfInternal(input, preState, TinyConfig)

    output.isLeft shouldBe true
    output.left.toOption.get shouldBe ReportErrorCode.DuplicatePackage
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
    (expected, actual) match
      case (Left(expectedErr), Left(actualErr)) =>
        expectedErr shouldBe actualErr withClue s"Error code mismatch in test case: $testCaseName"
      case (Left(expectedErr), Right(_)) =>
        fail(s"Expected error $expectedErr but got success in test case: $testCaseName")
      case (Right(_), Left(actualErr)) =>
        fail(s"Expected success but got error $actualErr in test case: $testCaseName")
      case (Right(expMarks), Right(actMarks)) =>
        actMarks.reported shouldBe expMarks.reported withClue
          s"Reported packages mismatch in test case: $testCaseName"
        actMarks.reporters shouldBe expMarks.reporters withClue
          s"Reporters mismatch in test case: $testCaseName"

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
            actAssign.report shouldBe expAssign.report withClue
              s"Assignment work report mismatch at index $idx in test case: $testCaseName"
            actAssign.timeout shouldBe expAssign.timeout withClue
              s"Assignment timeout mismatch at index $idx in test case: $testCaseName"
          case (false, false) => ()
          case (true, false) =>
            fail(s"Expected assignment at index $idx but got none in test case: $testCaseName")
          case (false, true) =>
            fail(s"Expected no assignment at index $idx but got one in test case: $testCaseName")
    }

    actual.coresStatistics.size shouldBe expected.coresStatistics.size withClue
      s"CoreStatistics size mismatch in test case: $testCaseName"
    expected.coresStatistics.zip(actual.coresStatistics).zipWithIndex.foreach {
      case ((exp, act), idx) =>
        val where = s"core $idx in test case: $testCaseName"
        act.daLoad shouldBe exp.daLoad withClue s"coresStatistics.daLoad mismatch at $where"
        act.popularity shouldBe exp.popularity withClue s"coresStatistics.popularity mismatch at $where"
        act.imports shouldBe exp.imports withClue s"coresStatistics.imports mismatch at $where"
        act.extrinsicCount shouldBe exp.extrinsicCount withClue s"coresStatistics.extrinsicCount mismatch at $where"
        act.extrinsicSize shouldBe exp.extrinsicSize withClue s"coresStatistics.extrinsicSize mismatch at $where"
        act.exports shouldBe exp.exports withClue s"coresStatistics.exports mismatch at $where"
        act.bundleSize shouldBe exp.bundleSize withClue s"coresStatistics.bundleSize mismatch at $where"
        act.gasUsed shouldBe exp.gasUsed withClue s"coresStatistics.gasUsed mismatch at $where"
    }

    actual.servicesStatistics.size shouldBe expected.servicesStatistics.size withClue
      s"ServicesStatistics size mismatch in test case: $testCaseName"
    expected.servicesStatistics.zip(actual.servicesStatistics).foreach {
      case (exp, act) =>
        act.id shouldBe exp.id withClue
          s"servicesStatistics.id mismatch in test case: $testCaseName"
        val where = s"service ${exp.id} in test case: $testCaseName"
        val e = exp.record
        val a = act.record
        a.providedCount shouldBe e.providedCount withClue s"servicesStatistics.providedCount mismatch at $where"
        a.providedSize shouldBe e.providedSize withClue s"servicesStatistics.providedSize mismatch at $where"
        a.refinementCount shouldBe e.refinementCount withClue s"servicesStatistics.refinementCount mismatch at $where"
        a.refinementGasUsed shouldBe e.refinementGasUsed withClue
          s"servicesStatistics.refinementGasUsed mismatch at $where"
        a.extrinsicCount shouldBe e.extrinsicCount withClue s"servicesStatistics.extrinsicCount mismatch at $where"
        a.extrinsicSize shouldBe e.extrinsicSize withClue s"servicesStatistics.extrinsicSize mismatch at $where"
        a.imports shouldBe e.imports withClue s"servicesStatistics.imports mismatch at $where"
        a.exports shouldBe e.exports withClue s"servicesStatistics.exports mismatch at $where"
        a.accumulateCount shouldBe e.accumulateCount withClue s"servicesStatistics.accumulateCount mismatch at $where"
        a.accumulateGasUsed shouldBe e.accumulateGasUsed withClue
          s"servicesStatistics.accumulateGasUsed mismatch at $where"
    }

  test("ReportErrorCode codec fails cleanly on an out-of-range ordinal") {
    val codec = summon[_root_.scodec.Codec[ReportErrorCode]]
    // All valid ordinals round-trip through encode/decode.
    ReportErrorCode.values.foreach { e =>
      val bits = codec.encode(e).require
      codec.decode(bits).require.value shouldBe e
    }
    // values.length (27) is out of range: decode must return a failed Attempt
    // (no thrown exception).
    val invalid = _root_.scodec.bits.BitVector(Array[Byte](ReportErrorCode.values.length.toByte))
    codec.decode(invalid).isFailure shouldBe true
  }
