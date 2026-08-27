package io.forge.jam.protocol.refine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.core.Hashing
import spire.math.UShort

class RefineExecutorSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY

  /** Wrap raw (code, bitmask) into the deblob format ServiceCode.parseBlob
    * accepts: varint(0 jump-table entries), entry size 0, varint(code length),
    * code, bitmask.
    */
  private def deblob(code: Array[Byte], bitmask: Array[Byte]): Array[Byte] =
    require(code.length < 128 && bitmask.length == (code.length + 7) / 8)
    Array[Byte](0, 0, code.length.toByte) ++ code ++ bitmask

  /** Service code preimage: zero metadata prefix + deblob code. */
  private def preimageOf(code: Array[Byte], bitmask: Array[Byte]): Array[Byte] =
    Array[Byte](0) ++ deblob(code, bitmask)

  // JumpIndirect r0 + 0 → djump(0xffff0000) → halt.
  private val haltCode = Array[Byte](50, 0)
  private val haltBitmask = Array[Byte](1)

  // Panic opcode.
  private val panicCode = Array[Byte](0)
  private val panicBitmask = Array[Byte](1)

  // ecalli 7 (EXPORT with r7 = input address, r8 = input length), then halt.
  private val exportThenHaltCode = Array[Byte](10, 7, 50, 0)
  private val exportThenHaltBitmask = Array[Byte](5) // instr at offsets 0 and 2

  private val codeHash = Hash(Array.fill[Byte](32)(0x42))

  private class SingleServiceLookup(
      serviceId: Long,
      preimage: Option[Array[Byte]]
  ) extends HistoricalLookupService:
    def serviceExists(id: Long): Boolean = id == serviceId
    def historicalLookup(id: Long, t: Long, h: Hash): Option[Array[Byte]] =
      if id == serviceId && h == codeHash then preimage else None

  private def workPackageFor(
      serviceId: Long,
      payload: Array[Byte],
      refineGas: Long = 1_000_000L
  ): WorkPackage =
    WorkPackage(
      authCodeHost = ServiceId(1),
      authCodeHash = Hash(Array.fill[Byte](32)(9)),
      context = Context(
        anchor = Hash(Array.fill[Byte](32)(1)),
        stateRoot = Hash(Array.fill[Byte](32)(2)),
        beefyRoot = Hash(Array.fill[Byte](32)(3)),
        lookupAnchor = Hash(Array.fill[Byte](32)(4)),
        lookupAnchorSlot = Timeslot(100),
        prerequisites = List.empty
      ),
      authorization = JamBytes(Array[Byte](0x0a)),
      authorizerConfig = JamBytes.empty,
      items = List(
        WorkItem(
          service = ServiceId(serviceId.toInt),
          codeHash = codeHash,
          payload = JamBytes(payload),
          refineGasLimit = Gas(refineGas),
          accumulateGasLimit = Gas(10_000L),
          importSegments = List.empty,
          extrinsic = List.empty,
          exportCount = UShort(0)
        )
      )
    )

  private def run(
      wp: WorkPackage,
      accounts: HistoricalLookupService
  ): RefineResult =
    new RefineExecutor(config).executeRefine(
      coreIndex = 1,
      workItemIndex = 0,
      workPackage = wp,
      authorizerTrace = Array[Byte](0x55),
      importSegments = IndexedSeq(IndexedSeq.empty),
      extrinsicData = IndexedSeq(IndexedSeq.empty),
      exportSegmentOffset = 0L,
      accounts = accounts
    )

  test("BAD when the service does not exist") {
    val wp = workPackageFor(42L, Array[Byte](1))
    val result = run(wp, new SingleServiceLookup(99L, None))
    result.result shouldBe ExecutionResult.BadCode
    result.gasUsed shouldBe 0L
  }

  test("BAD when the code preimage is not available at the lookup anchor") {
    val wp = workPackageFor(42L, Array[Byte](1))
    val result = run(wp, new SingleServiceLookup(42L, None))
    result.result shouldBe ExecutionResult.BadCode
  }

  test("BIG when the code preimage exceeds Cmaxservicecodesize") {
    val wp = workPackageFor(42L, Array[Byte](1))
    val big = new Array[Byte](4_000_001)
    val result = run(wp, new SingleServiceLookup(42L, Some(big)))
    result.result shouldBe ExecutionResult.CodeTooLarge
  }

  test("a halting service returns Ok with the refine argument encoding as output") {
    val payload = Array[Byte](0x11, 0x22, 0x33)
    val wp = workPackageFor(42L, payload)
    val accounts =
      new SingleServiceLookup(42L, Some(preimageOf(haltCode, haltBitmask)))

    val result = run(wp, accounts)

    // The program halts immediately with r7/r8 still pointing at the argument
    // buffer, so the output must be a = encode(c, i, s, var(payload), blake(p)).
    val expectedArgs =
      JamCodecs.encodeCompactInteger(1L) ++
        JamCodecs.encodeCompactInteger(0L) ++
        JamCodecs.encodeCompactInteger(42L) ++
        JamCodecs.encodeCompactInteger(payload.length.toLong) ++
        payload ++
        RefineFetch.workPackageHash(wp).bytes.toArray

    result.result match
      case ExecutionResult.Ok(output) =>
        output.toArray shouldBe expectedArgs
      case other => fail(s"expected Ok, got $other")
    result.gasUsed should be > 0L
    result.exports shouldBe empty
  }

  test("a panicking service returns Panic with no exports") {
    val wp = workPackageFor(42L, Array[Byte](1))
    val accounts =
      new SingleServiceLookup(42L, Some(preimageOf(panicCode, panicBitmask)))
    val result = run(wp, accounts)
    result.result shouldBe ExecutionResult.Panic
    result.exports shouldBe empty
    result.gasUsed should be > 0L
  }

  test("running out of gas returns OOG") {
    val wp = workPackageFor(42L, Array[Byte](1), refineGas = 0L)
    val accounts =
      new SingleServiceLookup(42L, Some(preimageOf(haltCode, haltBitmask)))
    val result = run(wp, accounts)
    result.result shouldBe ExecutionResult.OOG
  }

  test("EXPORT host call during refine surfaces the exported segment") {
    val payload = Array[Byte](0x7e)
    val wp = workPackageFor(42L, payload)
    val accounts = new SingleServiceLookup(
      42L,
      Some(preimageOf(exportThenHaltCode, exportThenHaltBitmask))
    )

    val result = run(wp, accounts)

    result.result match
      case ExecutionResult.Ok(_) => ()
      case other                 => fail(s"expected Ok, got $other")
    result.exports.size shouldBe 1
    result.exports.head.length shouldBe Csegmentsize.toInt

    // The exported segment is the zero-padded argument buffer (the program
    // passed r7 = input address, r8 = input length straight to EXPORT).
    val expectedArgs =
      JamCodecs.encodeCompactInteger(1L) ++
        JamCodecs.encodeCompactInteger(0L) ++
        JamCodecs.encodeCompactInteger(42L) ++
        JamCodecs.encodeCompactInteger(payload.length.toLong) ++
        payload ++
        RefineFetch.workPackageHash(wp).bytes.toArray
    result.exports.head.take(expectedArgs.length) shouldBe expectedArgs
    result.exports.head.drop(expectedArgs.length).forall(_ == 0) shouldBe true
  }
