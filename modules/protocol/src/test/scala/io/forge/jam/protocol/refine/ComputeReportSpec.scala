package io.forge.jam.protocol.refine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.merkle.BinaryMerkle
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.types.workitem.{WorkItem, WorkItemExtrinsic, WorkItemImportSegment}
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.crypto.ErasureCoding
import spire.math.{UInt, UShort}

class ComputeReportSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private val serviceCodeHash = Hash(Array.fill[Byte](32)(0x42))
  private val authCodeHash = Hash(Array.fill[Byte](32)(0x21))

  private def deblob(code: Array[Byte], bitmask: Array[Byte]): Array[Byte] =
    Array[Byte](0, 0, code.length.toByte) ++ code ++ bitmask

  private def preimageOf(code: Array[Byte], bitmask: Array[Byte]): Array[Byte] =
    Array[Byte](0) ++ deblob(code, bitmask)

  // JumpIndirect r0 + 0 → halt with the args echoed as output.
  private val haltEcho = preimageOf(Array[Byte](50, 0), Array[Byte](1))
  // ecalli 7 (EXPORT of the args buffer), then halt.
  private val exportThenHalt =
    preimageOf(Array[Byte](10, 7, 50, 0), Array[Byte](5))

  private class TwoServiceLookup(
      services: Map[Long, Map[Hash, Array[Byte]]]
  ) extends HistoricalLookupService:
    def serviceExists(id: Long): Boolean = services.contains(id)
    def historicalLookup(id: Long, t: Long, h: Hash): Option[Array[Byte]] =
      services.get(id).flatMap(_.get(h))

  private def accountsWith(serviceCode: Array[Byte]): HistoricalLookupService =
    new TwoServiceLookup(
      Map(
        5L -> Map(authCodeHash -> haltEcho),
        42L -> Map(serviceCodeHash -> serviceCode)
      )
    )

  private def workPackageWith(
      exportCount: Int,
      serviceCode: Array[Byte] = haltEcho
  ): WorkPackage =
    WorkPackage(
      authCodeHost = ServiceId(5),
      authCodeHash = authCodeHash,
      context = Context(
        anchor = Hash(Array.fill[Byte](32)(1)),
        stateRoot = Hash(Array.fill[Byte](32)(2)),
        beefyRoot = Hash(Array.fill[Byte](32)(3)),
        lookupAnchor = Hash(Array.fill[Byte](32)(4)),
        lookupAnchorSlot = Timeslot(100),
        prerequisites = List.empty
      ),
      authorization = JamBytes(Array[Byte](0x0a)),
      authorizerConfig = JamBytes(Array[Byte](0x0d)),
      items = List(
        WorkItem(
          service = ServiceId(42),
          codeHash = serviceCodeHash,
          payload = JamBytes(Array[Byte](0x11, 0x22)),
          refineGasLimit = Gas(1_000_000L),
          accumulateGasLimit = Gas(10_000L),
          importSegments = List.empty,
          extrinsic = List.empty,
          exportCount = UShort(exportCount)
        )
      )
    )

  private def compute(
      wp: WorkPackage,
      accounts: HistoricalLookupService
  ): Either[ComputeReportError, ComputedReport] =
    new ComputeReport(config).compute(
      workPackage = wp,
      coreIndex = 1,
      segmentRootLookup = Map.empty,
      importSegments = IndexedSeq(IndexedSeq.empty),
      extrinsicData = IndexedSeq(IndexedSeq.empty),
      justifications = IndexedSeq(IndexedSeq.empty),
      accounts = accounts
    )

  test("computes a full report for a successful single-item package") {
    assume(ErasureCoding.isAvailable)
    val wp = workPackageWith(exportCount = 0)
    val result = compute(wp, accountsWith(haltEcho))

    val computed = result.toOption.getOrElse(fail(s"compute failed: $result"))
    val report = computed.report

    // Authorization: the halt-echo authorizer echoes encode[2](core) = [1, 0].
    report.authOutput.toArray shouldBe Array[Byte](1, 0)
    report.authGasUsed.toLong should be > 0L
    report.authorizerHash shouldBe io.forge.jam.core.Hashing.blake2b256(
      authCodeHash.bytes.toArray ++ Array[Byte](0x0d)
    )

    // Package spec commitments.
    report.packageSpec.hash shouldBe RefineFetch.workPackageHash(wp)
    report.packageSpec.length.toLong shouldBe computed.bundleBytes.length.toLong
    report.packageSpec.exportsCount.toInt shouldBe 0
    // No exports: segments-root is the zero hash.
    report.packageSpec.exportsRoot.bytes.toArray shouldBe BinaryMerkle.ZeroHash
    report.packageSpec.erasureRoot.bytes.toArray should not be BinaryMerkle.ZeroHash

    // Digest for the item: Ok result with the refine args echoed.
    report.results.size shouldBe 1
    val digest = report.results.head
    digest.serviceId shouldBe ServiceId(42)
    digest.codeHash shouldBe serviceCodeHash
    digest.result match
      case ExecutionResult.Ok(out) => out.length should be > 0
      case other                   => fail(s"expected Ok, got $other")
    digest.refineLoad.gasUsed.toLong should be > 0L

    // Context and core carried through.
    report.context shouldBe wp.context
    report.coreIndex.toInt shouldBe 1
  }

  test("bundle round-trips through encode/decode") {
    assume(ErasureCoding.isAvailable)
    val wp = workPackageWith(exportCount = 0)
    val computed = compute(wp, accountsWith(haltEcho)).toOption.get

    val decoded = WorkPackageBundle.decode(computed.bundleBytes)
    decoded.isRight shouldBe true
    val bundle = decoded.toOption.get
    RefineFetch.workPackageHash(bundle.workPackage) shouldBe
      RefineFetch.workPackageHash(wp)
    bundle.extrinsicData.map(_.size) shouldBe IndexedSeq(0)
    bundle.importSegments.map(_.size) shouldBe IndexedSeq(0)
  }

  test("a declared-but-not-produced export count yields BadExports with zero segments") {
    assume(ErasureCoding.isAvailable)
    // Service halts without exporting, but the item declares 2 exports.
    val wp = workPackageWith(exportCount = 2)
    val computed = compute(wp, accountsWith(haltEcho)).toOption.get

    computed.report.results.head.result shouldBe ExecutionResult.BadExports
    // Zero segments substituted and committed.
    computed.exportedSegments.size shouldBe 2
    computed.exportedSegments.foreach { seg =>
      seg.length shouldBe Csegmentsize.toInt
      seg.forall(_ == 0) shouldBe true
    }
    computed.report.packageSpec.exportsCount.toInt shouldBe 2
    computed.report.packageSpec.exportsRoot.bytes.toArray should not be
      BinaryMerkle.ZeroHash
  }

  test("a service that exports its declared segment count reports Ok with real exports") {
    assume(ErasureCoding.isAvailable)
    val wp = workPackageWith(exportCount = 1, serviceCode = exportThenHalt)
    val accounts = new TwoServiceLookup(
      Map(
        5L -> Map(authCodeHash -> haltEcho),
        42L -> Map(serviceCodeHash -> exportThenHalt)
      )
    )
    val computed = compute(wp, accounts).toOption.get

    computed.report.results.head.result match
      case ExecutionResult.Ok(_) => ()
      case other                 => fail(s"expected Ok, got $other")
    computed.exportedSegments.size shouldBe 1
    // The export is the zero-padded refine argument buffer — not all zeros.
    computed.exportedSegments.head.exists(_ != 0) shouldBe true
    computed.pagedProofs.size shouldBe 1
    computed.pagedProofs.head.length shouldBe Csegmentsize.toInt
  }

  test("fails with AuthorizationFailed when the auth code is unavailable") {
    val wp = workPackageWith(exportCount = 0)
    val noAuth = new TwoServiceLookup(
      Map(42L -> Map(serviceCodeHash -> haltEcho))
    )
    compute(wp, noAuth) match
      case Left(ComputeReportError.AuthorizationFailed(ExecutionResult.BadCode)) => ()
      case other => fail(s"expected AuthorizationFailed(BadCode), got $other")
  }

  test("erasure root changes when the bundle changes") {
    assume(ErasureCoding.isAvailable)
    val wp1 = workPackageWith(exportCount = 0)
    val r1 = compute(wp1, accountsWith(haltEcho)).toOption.get

    val wp2 = wp1.copy(authorization = JamBytes(Array[Byte](0x0b)))
    val r2 = compute(wp2, accountsWith(haltEcho)).toOption.get

    r1.report.packageSpec.erasureRoot should not be r2.report.packageSpec.erasureRoot
  }

  test("bundle can be reconstructed from a third of the erasure chunks") {
    assume(ErasureCoding.isAvailable)
    val wp = workPackageWith(exportCount = 0)
    val computed = compute(wp, accountsWith(haltEcho)).toOption.get
    val bundle = computed.bundleBytes

    val shards = ErasureCoding
      .chunk(bundle, config.ecPieceSize, config.validatorCount)
      .toOption
      .get

    // Reconstruct from the RECOVERY shards only (indices ≥ originalCount).
    val originalCount = config.ecPieceSize / 2
    val subset = (originalCount until originalCount * 2).map { i =>
      ErasureCoding.Shard(shards(i), i)
    }.toArray

    val recovered = ErasureCoding
      .reconstruct(subset, config.ecPieceSize, config.validatorCount)
      .toOption
      .get

    recovered.take(bundle.length) shouldBe bundle
  }
