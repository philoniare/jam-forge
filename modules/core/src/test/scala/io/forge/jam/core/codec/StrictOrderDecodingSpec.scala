package io.forge.jam.core.codec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{CoreIndex, Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.codec.JamCodecs.{compactInt, compactInteger, compactPrefixedList, hashCodec}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.work.{ExecutionResult, PackageSpec}
import io.forge.jam.core.types.workpackage.{SegmentRootLookup, WorkReport}
import io.forge.jam.core.types.workresult.{RefineLoad, WorkResult}

/** The segment-root lookup `l` is a dictionary (encoded ordered by key) and the context
  * prerequisites `p` are a set (encoded `a < b < …`). Decoding must reject unsorted or repeated keys.
  */
class StrictOrderDecodingSpec extends AnyFlatSpec with Matchers:

  private def h(b: Int): Hash = Hash(Array.fill(32)(b.toByte))

  private def ctx(prereqs: List[Hash]): Context =
    Context(h(1), Timeslot(7), h(2), h(3), h(4), Timeslot(8), h(5), prereqs)

  private val spec = PackageSpec(h(0x11), UInt(1000), h(0x22), UShort(6), h(0x33), UShort(0))

  private val result = WorkResult(
    serviceId = ServiceId(1),
    codeHash = h(0x44),
    payloadHash = h(0x55),
    accumulateGas = Gas(100L),
    result = ExecutionResult.Ok(JamBytes(Array[Byte](1, 2, 3))),
    refineLoad = RefineLoad(Gas(10L), UShort(1), UShort(1), UInt(100), UShort(1))
  )

  private def report(lookup: List[SegmentRootLookup], prereqs: List[Hash] = Nil): WorkReport =
    WorkReport(spec, ctx(prereqs), CoreIndex(1), h(0x66), Gas(5L), JamBytes(Array[Byte](9)), lookup, List(result))

  /** Wire bytes of a report with `lookup` written in the given order (the report encoder would sort it). */
  private def rawReportBytes(lookup: List[SegmentRootLookup]): Array[Byte] =
    val r = report(Nil)
    Array.concat(
      Codec[PackageSpec].encode(r.packageSpec),
      Codec[Context].encode(r.context),
      compactInt.encode(r.coreIndex.toInt),
      hashCodec.encode(r.authorizerHash),
      compactInteger.encode(r.authGasUsed.toLong),
      PrimitiveCodecs.compactBytes.encode(r.authOutput),
      compactPrefixedList(Codec[SegmentRootLookup]).encode(lookup),
      compactPrefixedList(Codec[WorkResult]).encode(r.results)
    )

  private val lowLookup = SegmentRootLookup(h(0x0a), h(0xa0))
  private val highLookup = SegmentRootLookup(h(0xf0), h(0x0f))

  "WorkReport decode" should "accept a sorted segment-root lookup and re-encode it byte-for-byte" in {
    val wire = rawReportBytes(List(lowLookup, highLookup))
    val decoded = Codec[WorkReport].decodeExact(wire)
    decoded.map(_.segmentRootLookup) shouldBe Right(List(lowLookup, highLookup))
    decoded.map(r => Codec[WorkReport].encode(r).toSeq) shouldBe Right(wire.toSeq)
  }

  it should "reject an unsorted segment-root lookup" in {
    Codec[WorkReport].decodeExact(rawReportBytes(List(highLookup, lowLookup))).isLeft shouldBe true
  }

  it should "reject a segment-root lookup with a duplicate work-package hash" in {
    val dupSameRoot = List(lowLookup, lowLookup)
    val dupOtherRoot = List(lowLookup, SegmentRootLookup(lowLookup.workPackageHash, h(0xbb)))
    Codec[WorkReport].decodeExact(rawReportBytes(dupSameRoot)).isLeft shouldBe true
    Codec[WorkReport].decodeExact(rawReportBytes(dupOtherRoot)).isLeft shouldBe true
  }

  it should "compare work-package hashes as unsigned bytes" in {
    // 0x7f.. < 0x80.. unsigned; a signed comparison would order these the other way.
    val sorted = List(SegmentRootLookup(h(0x7f), h(1)), SegmentRootLookup(h(0x80), h(2)))
    Codec[WorkReport].decodeExact(rawReportBytes(sorted)).isRight shouldBe true
    Codec[WorkReport].decodeExact(rawReportBytes(sorted.reverse)).isLeft shouldBe true
  }

  "Context decode" should "accept strictly increasing prerequisites and round-trip them" in {
    val c = ctx(List(h(0x01), h(0x7f), h(0x80), h(0xff)))
    val wire = Codec[Context].encode(c)
    Codec[Context].decodeExact(wire) shouldBe Right(c)
  }

  it should "reject unsorted prerequisites" in {
    Codec[Context].decodeExact(Codec[Context].encode(ctx(List(h(0xbb), h(0xaa))))).isLeft shouldBe true
  }

  it should "reject duplicate prerequisites" in {
    Codec[Context].decodeExact(Codec[Context].encode(ctx(List(h(0xaa), h(0xaa))))).isLeft shouldBe true
  }

  it should "reject unsorted prerequisites inside a work report" in {
    val wire = Codec[WorkReport].encode(report(Nil, List(h(0xbb), h(0xaa))))
    Codec[WorkReport].decodeExact(wire).isLeft shouldBe true
  }
