package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.{UShort, UInt}
import io.forge.jam.core.codec.{Codec, codecs}

import primitives.*
import types.context.*
import types.workresult.*
import types.workpackage.WorkReport
import types.work.*
import types.dispute.Fault

/**
 * Decoders must reject values outside the target field's range instead of truncating them, so a decoded value
 * always re-encodes to the bytes it came from (each value has exactly one encoding).
 */
class StrictDecodingSpec extends AnyFlatSpec with Matchers:

  private def compact(v: Long): Array[Byte] = codecs.compactLong.encode(v)

  private val ctx = Context(
    anchor = Hash(Array.fill(32)(0x11.toByte)),
    anchorSlot = Timeslot(0),
    stateRoot = Hash(Array.fill(32)(0x22.toByte)),
    beefyRoot = Hash(Array.fill(32)(0x33.toByte)),
    lookupAnchor = Hash(Array.fill(32)(0x44.toByte)),
    lookupAnchorSlot = Timeslot(7),
    lookupAnchorStateRoot = Hash.zero,
    prerequisites = List.empty
  )

  private val spec = PackageSpec(
    hash = Hash(Array.fill(32)(0x55.toByte)),
    length = UInt(1000),
    erasureRoot = Hash(Array.fill(32)(0x66.toByte)),
    erasureShards = UShort(6),
    exportsRoot = Hash(Array.fill(32)(0x77.toByte)),
    exportsCount = UShort(1)
  )

  private def report(core: Int, load: RefineLoad): WorkReport = WorkReport(
    packageSpec = spec,
    context = ctx,
    coreIndex = CoreIndex(core),
    authorizerHash = Hash(Array.fill(32)(0x88.toByte)),
    authGasUsed = Gas(50L),
    authOutput = JamBytes(Array[Byte](1)),
    segmentRootLookup = List.empty,
    results = List(
      WorkResult(
        serviceId = ServiceId(1),
        codeHash = Hash(Array.fill(32)(0x99.toByte)),
        payloadHash = Hash(Array.fill(32)(0xAA.toByte)),
        accumulateGas = Gas(100L),
        result = ExecutionResult.Ok(JamBytes(Array[Byte](1, 2, 3))),
        refineLoad = load
      )
    )
  )

  private val load = RefineLoad(Gas(10L), UShort(1), UShort(2), UInt(300), UShort(4))

  // Offset of the compact core-index field in an encoded WorkReport.
  private def coreOffset: Int = Codec[PackageSpec].encode(spec).length + Codec[Context].encode(ctx).length

  private def withCore(core: Long): Array[Byte] =
    val valid = Codec[WorkReport].encode(report(3, load))
    valid.take(coreOffset) ++ compact(core) ++ valid.drop(coreOffset + 1) // core 3 encodes as one byte

  private def loadBytes(gas: Long, imports: Long, xtCount: Long, xtSize: Long, exports: Long): Array[Byte] =
    compact(gas) ++ compact(imports) ++ compact(xtCount) ++ compact(xtSize) ++ compact(exports)

  "WorkReport decoding" should "round-trip a valid core index unchanged" in {
    for core <- Seq(0, 3, 341, 65535) do
      val bytes = Codec[WorkReport].encode(report(core, load))
      val decoded = Codec[WorkReport].decodeExact(bytes)
      decoded.map(_.coreIndex.toInt) shouldBe Right(core)
      Codec[WorkReport].encode(decoded.toOption.get) shouldBe bytes
  }

  it should "reject a core index of 2^16 or more instead of masking it" in {
    Codec[WorkReport].decodeExact(withCore(3)).map(_.coreIndex.toInt) shouldBe Right(3)
    Codec[WorkReport].decodeExact(withCore(65536)).isLeft shouldBe true
    Codec[WorkReport].decodeExact(withCore(65539)).isLeft shouldBe true
    Codec[WorkReport].decodeExact(withCore(Int.MaxValue.toLong)).isLeft shouldBe true
  }

  "RefineLoad decoding" should "round-trip valid values including the field maxima" in {
    val max = RefineLoad(Gas(-1L), UShort(0xffff), UShort(0xffff), UInt(0xffffffffL), UShort(0xffff))
    for l <- Seq(load, max) do
      val bytes = Codec[RefineLoad].encode(l)
      val decoded = Codec[RefineLoad].decodeExact(bytes)
      decoded shouldBe Right(l)
      Codec[RefineLoad].encode(decoded.toOption.get) shouldBe bytes
  }

  it should "reject over-range digest counts instead of truncating them" in {
    Codec[RefineLoad].decodeExact(loadBytes(10, 65536 + 1, 2, 300, 4)).isLeft shouldBe true
    Codec[RefineLoad].decodeExact(loadBytes(10, 1, 65536, 300, 4)).isLeft shouldBe true
    Codec[RefineLoad].decodeExact(loadBytes(10, 1, 2, 1L << 32, 4)).isLeft shouldBe true
    Codec[RefineLoad].decodeExact(loadBytes(10, 1, 2, 300, 65536)).isLeft shouldBe true
    Codec[RefineLoad].decodeExact(loadBytes(10, 1, 2, 300, -1L)).isLeft shouldBe true // 2^64-1
  }

  it should "make a WorkReport with an over-range digest count fail to decode" in {
    val valid = Codec[WorkReport].encode(report(3, load))
    val validLoad = Codec[RefineLoad].encode(load)
    valid.takeRight(validLoad.length) shouldBe validLoad // RefineLoad is the last field
    val bad = valid.dropRight(validLoad.length) ++ loadBytes(10, 65536 + 1, 2, 300, 4)
    Codec[WorkReport].decodeExact(bad).isLeft shouldBe true
  }

  private val sig = Ed25519Signature(Array.fill(64)(0x5A.toByte))

  "Vote decoding" should "round-trip valid votes" in {
    for v <- Seq(true, false) do
      val vote = Vote(v, ValidatorIndex(5), sig)
      val bytes = Codec[Vote].encode(vote)
      bytes(0) shouldBe (if v then 1 else 0).toByte
      Codec[Vote].decodeExact(bytes) shouldBe Right(vote)
  }

  it should "reject a validity byte other than 0 or 1" in {
    val bytes = Codec[Vote].encode(Vote(true, ValidatorIndex(5), sig))
    for b <- Seq(2, 0x80, 0xff) do
      Codec[Vote].decodeExact(bytes.updated(0, b.toByte)).isLeft shouldBe true
  }

  "Fault decoding" should "round-trip valid faults and reject a validity byte other than 0 or 1" in {
    for v <- Seq(true, false) do
      val fault = Fault(Hash(Array.fill(32)(0x01.toByte)), v, Ed25519PublicKey(Array.fill(32)(0x02.toByte)), sig)
      val bytes = Codec[Fault].encode(fault)
      Codec[Fault].decodeExact(bytes) shouldBe Right(fault)
      Codec[Fault].decodeExact(bytes.updated(Hash.Size, 2.toByte)).isLeft shouldBe true
  }
