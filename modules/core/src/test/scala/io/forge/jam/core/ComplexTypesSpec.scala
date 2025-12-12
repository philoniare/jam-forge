package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.{UByte, UShort, UInt}
import java.nio.file.{Files, Paths}

import codec.*
import codec.{encode, decodeAs}
import primitives.*
import types.context.*
import types.workitem.*
import types.workresult.*
import types.workpackage.{SegmentRootLookup, WorkPackage, WorkReport as WReport}
import types.extrinsic.*
import types.header.*
import types.block.{Block as JamBlock, Extrinsic as JamExtrinsic}
import types.work.*
import types.epoch.*
import types.tickets.*
import types.dispute.*

class ComplexTypesSpec extends AnyFlatSpec with Matchers:

  // ============================================================================
  // Test 1: Context encode/decode (with prerequisites list)
  // ============================================================================

  "Context" should "encode without prerequisites to 133 bytes" in {
    val ctx = Context(
      anchor = Hash(Array.fill(32)(0x11.toByte)),
      stateRoot = Hash(Array.fill(32)(0x22.toByte)),
      beefyRoot = Hash(Array.fill(32)(0x33.toByte)),
      lookupAnchor = Hash(Array.fill(32)(0x44.toByte)),
      lookupAnchorSlot = Timeslot(42),
      prerequisites = List.empty
    )
    val encoded = ctx.encode
    // 4 * 32 + 4 + 1 (compact 0) = 133 bytes
    encoded.length shouldBe 133
  }

  it should "encode prerequisites with compact length prefix" in {
    val prereq1 = Hash(Array.fill(32)(0xAA.toByte))
    val prereq2 = Hash(Array.fill(32)(0xBB.toByte))
    val ctx = Context(
      anchor = Hash.zero,
      stateRoot = Hash.zero,
      beefyRoot = Hash.zero,
      lookupAnchor = Hash.zero,
      lookupAnchorSlot = Timeslot(0),
      prerequisites = List(prereq1, prereq2)
    )
    val encoded = ctx.encode
    // 4 * 32 + 4 + 1 (compact 2) + 2 * 32 = 197 bytes
    encoded.length shouldBe 197
  }

  it should "round-trip correctly" in {
    val prereq = Hash(Array.tabulate(32)(i => i.toByte))
    val ctx = Context(
      anchor = Hash(Array.tabulate(32)(i => (i + 10).toByte)),
      stateRoot = Hash(Array.tabulate(32)(i => (i + 20).toByte)),
      beefyRoot = Hash(Array.tabulate(32)(i => (i + 30).toByte)),
      lookupAnchor = Hash(Array.tabulate(32)(i => (i + 40).toByte)),
      lookupAnchorSlot = Timeslot(12345),
      prerequisites = List(prereq)
    )
    val encoded = ctx.encode
    val (decoded, consumed) = encoded.decodeAs[Context](0)
    consumed shouldBe encoded.length
    decoded.anchor.toHex shouldBe ctx.anchor.toHex
    decoded.stateRoot.toHex shouldBe ctx.stateRoot.toHex
    decoded.beefyRoot.toHex shouldBe ctx.beefyRoot.toHex
    decoded.lookupAnchor.toHex shouldBe ctx.lookupAnchor.toHex
    decoded.lookupAnchorSlot.toInt shouldBe ctx.lookupAnchorSlot.toInt
    decoded.prerequisites.length shouldBe 1
    decoded.prerequisites.head.toHex shouldBe prereq.toHex
  }

  // ============================================================================
  // Test 2: WorkItem encode/decode (variable length)
  // ============================================================================

  "WorkItem" should "encode service as first 4 bytes" in {
    val item = WorkItem(
      service = ServiceId(0x12345678),
      codeHash = Hash.zero,
      payload = JamBytes.empty,
      refineGasLimit = Gas(0L),
      accumulateGasLimit = Gas(0L),
      importSegments = List.empty,
      extrinsic = List.empty,
      exportCount = UShort(0)
    )
    val encoded = item.encode
    // service is little-endian
    encoded(0) shouldBe UByte(0x78)
    encoded(1) shouldBe UByte(0x56)
    encoded(2) shouldBe UByte(0x34)
    encoded(3) shouldBe UByte(0x12)
  }

  it should "round-trip correctly with import segments and extrinsics" in {
    val item = WorkItem(
      service = ServiceId(42),
      codeHash = Hash(Array.fill(32)(0xAA.toByte)),
      payload = JamBytes(Array[Byte](1, 2, 3, 4, 5)),
      refineGasLimit = Gas(1000L),
      accumulateGasLimit = Gas(2000L),
      importSegments = List(
        WorkItemImportSegment(Hash(Array.fill(32)(0x11.toByte)), UShort(0)),
        WorkItemImportSegment(Hash(Array.fill(32)(0x22.toByte)), UShort(1))
      ),
      extrinsic = List(
        WorkItemExtrinsic(Hash(Array.fill(32)(0x33.toByte)), UInt(100))
      ),
      exportCount = UShort(5)
    )
    val encoded = item.encode
    val (decoded, consumed) = encoded.decodeAs[WorkItem](0)
    consumed shouldBe encoded.length
    decoded.service.toInt shouldBe item.service.toInt
    decoded.payload shouldBe item.payload
    decoded.importSegments.length shouldBe 2
    decoded.extrinsic.length shouldBe 1
    decoded.exportCount.toInt shouldBe item.exportCount.toInt
  }

  // ============================================================================
  // Test 3: WorkResult encode/decode (with ExecutionResult enum)
  // ============================================================================

  "WorkResult" should "encode Ok result correctly" in {
    val result = WorkResult(
      serviceId = ServiceId(16909060),
      codeHash = Hash(Array.fill(32)(0x70.toByte)),
      payloadHash = Hash(Array.fill(32)(0xFA.toByte)),
      accumulateGas = Gas(42L),
      result = ExecutionResult.Ok(JamBytes(Array[Byte](0xAA.toByte, 0xBB.toByte, 0xCC.toByte))),
      refineLoad = RefineLoad(Gas(0L), UShort(0), UShort(0), UInt(0), UShort(0))
    )
    val encoded = result.encode
    // Fixed part: 4 + 32 + 32 + 8 = 76 bytes
    // Then result tag (0x00) + compact length (3) + 3 bytes = 5 bytes
    // Then refineLoad: 5 compact zeros = 5 bytes
    // Total: 76 + 5 + 5 = 86 bytes
    encoded.length shouldBe 86
  }

  it should "encode Panic result correctly" in {
    val result = WorkResult(
      serviceId = ServiceId(84281096),
      codeHash = Hash(Array.fill(32)(0xFC.toByte)),
      payloadHash = Hash(Array.fill(32)(0xD5.toByte)),
      accumulateGas = Gas(33L),
      result = ExecutionResult.Panic,
      refineLoad = RefineLoad(Gas(0L), UShort(0), UShort(0), UInt(0), UShort(0))
    )
    val encoded = result.encode
    val (decoded, consumed) = encoded.decodeAs[WorkResult](0)
    decoded.result shouldBe ExecutionResult.Panic
  }

  it should "round-trip correctly" in {
    val result = WorkResult(
      serviceId = ServiceId(999),
      codeHash = Hash(Array.tabulate(32)(i => i.toByte)),
      payloadHash = Hash(Array.tabulate(32)(i => (i + 100).toByte)),
      accumulateGas = Gas(12345L),
      result = ExecutionResult.Ok(JamBytes(Array.tabulate(50)(i => (i * 2).toByte))),
      refineLoad = RefineLoad(Gas(100L), UShort(5), UShort(3), UInt(1000), UShort(2))
    )
    val encoded = result.encode
    val (decoded, consumed) = encoded.decodeAs[WorkResult](0)
    consumed shouldBe encoded.length
    decoded.serviceId.toInt shouldBe result.serviceId.toInt
    decoded.result match
      case ExecutionResult.Ok(output) => output.length shouldBe 50
      case _ => fail("Expected Ok")
  }

  // ============================================================================
  // Test 4: Extrinsic structure encoding
  // ============================================================================

  "JamExtrinsic" should "encode empty extrinsic correctly" in {
    val ext = JamExtrinsic(
      tickets = List.empty,
      preimages = List.empty,
      guarantees = List.empty,
      assurances = List.empty,
      disputes = Dispute(List.empty, List.empty, List.empty)
    )
    val encoded = ext.encode
    // 4 compact zeros for the 4 lists (tickets, preimages, guarantees, assurances)
    // + 3 compact zeros for dispute (verdicts, culprits, faults)
    // = 7 bytes total
    encoded.length shouldBe 7
  }

  it should "round-trip empty extrinsic correctly" in {
    val ext = JamExtrinsic(
      tickets = List.empty,
      preimages = List.empty,
      guarantees = List.empty,
      assurances = List.empty,
      disputes = Dispute(List.empty, List.empty, List.empty)
    )
    val encoded = ext.encode
    val config = ChainConfig.TINY
    val (decoded, consumed) = types.block.Extrinsic.decoder(config).decode(encoded, 0)
    consumed shouldBe encoded.length
    decoded.tickets shouldBe empty
    decoded.preimages shouldBe empty
    decoded.guarantees shouldBe empty
    decoded.assurances shouldBe empty
    decoded.disputes.verdicts shouldBe empty
  }

  // ============================================================================
  // Test 5: Block structure encoding
  // ============================================================================

  "JamBlock" should "encode header and extrinsic in sequence" in {
    val header = Header(
      parent = Hash.zero,
      parentStateRoot = Hash.zero,
      extrinsicHash = Hash.zero,
      slot = Timeslot(1),
      epochMark = None,
      ticketsMark = None,
      authorIndex = ValidatorIndex(0),
      entropySource = JamBytes.zeros(96),
      offendersMark = List.empty,
      seal = JamBytes.zeros(96)
    )

    val extrinsic = JamExtrinsic(
      tickets = List.empty,
      preimages = List.empty,
      guarantees = List.empty,
      assurances = List.empty,
      disputes = Dispute(List.empty, List.empty, List.empty)
    )

    val block = JamBlock(header, extrinsic)
    val encoded = block.encode

    // Header: 32*3 (hashes) + 4 (slot) + 1 (no epoch) + 1 (no tickets) + 2 (author) + 96 (entropy) + 1 (no offenders) + 96 (seal) = 297
    // Extrinsic: 7 bytes (4 empty lists + 3 empty dispute lists)
    // Total: 304 bytes
    encoded.length shouldBe 304
  }

  it should "round-trip correctly" in {
    val header = Header(
      parent = Hash(Array.tabulate(32)(_.toByte)),
      parentStateRoot = Hash(Array.tabulate(32)(i => (i + 50).toByte)),
      extrinsicHash = Hash(Array.tabulate(32)(i => (i + 100).toByte)),
      slot = Timeslot(999),
      epochMark = None,
      ticketsMark = None,
      authorIndex = ValidatorIndex(5),
      entropySource = JamBytes.fill(96)(0x42.toByte),
      offendersMark = List.empty,
      seal = JamBytes.fill(96)(0x99.toByte)
    )

    val extrinsic = JamExtrinsic(
      tickets = List.empty,
      preimages = List.empty,
      guarantees = List.empty,
      assurances = List.empty,
      disputes = Dispute(List.empty, List.empty, List.empty)
    )

    val block = JamBlock(header, extrinsic)
    val encoded = block.encode
    val config = ChainConfig.TINY
    val (decoded, consumed) = types.block.Block.decoder(config).decode(encoded, 0)
    consumed shouldBe encoded.length
    decoded.header.slot.toInt shouldBe 999
    decoded.header.authorIndex.toInt shouldBe 5
  }

  // ============================================================================
  // Test 6: WorkPackage encode/decode
  // ============================================================================

  "WorkPackage" should "round-trip correctly" in {
    val ctx = Context(
      anchor = Hash.zero,
      stateRoot = Hash.zero,
      beefyRoot = Hash.zero,
      lookupAnchor = Hash.zero,
      lookupAnchorSlot = Timeslot(0),
      prerequisites = List.empty
    )

    val item = WorkItem(
      service = ServiceId(1),
      codeHash = Hash(Array.fill(32)(0xAB.toByte)),
      payload = JamBytes(Array[Byte](1, 2, 3)),
      refineGasLimit = Gas(100L),
      accumulateGasLimit = Gas(200L),
      importSegments = List.empty,
      extrinsic = List.empty,
      exportCount = UShort(1)
    )

    val pkg = WorkPackage(
      authCodeHost = ServiceId(42),
      authCodeHash = Hash(Array.fill(32)(0xCC.toByte)),
      context = ctx,
      authorization = JamBytes(Array[Byte](0xAA.toByte)),
      authorizerConfig = JamBytes(Array[Byte](0xBB.toByte)),
      items = List(item)
    )

    val encoded = pkg.encode
    val (decoded, consumed) = encoded.decodeAs[WorkPackage](0)
    consumed shouldBe encoded.length
    decoded.authCodeHost.toInt shouldBe 42
    decoded.items.length shouldBe 1
    decoded.items.head.service.toInt shouldBe 1
  }

  // ============================================================================
  // Test 7: WorkReport encode/decode
  // ============================================================================

  "WReport" should "round-trip correctly" in {
    val ctx = Context(
      anchor = Hash.zero,
      stateRoot = Hash.zero,
      beefyRoot = Hash.zero,
      lookupAnchor = Hash.zero,
      lookupAnchorSlot = Timeslot(0),
      prerequisites = List.empty
    )

    val spec = PackageSpec(
      hash = Hash(Array.fill(32)(0x11.toByte)),
      length = UInt(1000),
      erasureRoot = Hash(Array.fill(32)(0x22.toByte)),
      exportsRoot = Hash(Array.fill(32)(0x33.toByte)),
      exportsCount = UShort(10)
    )

    val result = WorkResult(
      serviceId = ServiceId(1),
      codeHash = Hash(Array.fill(32)(0x44.toByte)),
      payloadHash = Hash(Array.fill(32)(0x55.toByte)),
      accumulateGas = Gas(100L),
      result = ExecutionResult.Ok(JamBytes(Array[Byte](1, 2, 3))),
      refineLoad = RefineLoad(Gas(10L), UShort(1), UShort(1), UInt(100), UShort(1))
    )

    val report = WReport(
      packageSpec = spec,
      context = ctx,
      coreIndex = CoreIndex(2),
      authorizerHash = Hash(Array.fill(32)(0x66.toByte)),
      authGasUsed = Gas(50L),
      authOutput = JamBytes(Array[Byte](0xAA.toByte)),
      segmentRootLookup = List.empty,
      results = List(result)
    )

    val encoded = report.encode
    val (decoded, consumed) = encoded.decodeAs[WReport](0)
    consumed shouldBe encoded.length
    decoded.coreIndex.toInt shouldBe 2
    decoded.results.length shouldBe 1
  }
