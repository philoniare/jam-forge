package io.forge.jam.core.scodec

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Ed25519Signature, Timeslot, ValidatorIndex}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scodec.Codec
import scodec.bits.BitVector

class PrimitiveCodecsSpec extends AnyFunSuite with Matchers:

  test("ed25519Signature round-trips and is 64 bytes on the wire") {
    val sig = Ed25519Signature(Array.tabulate[Byte](64)(_.toByte))
    val bits = PrimitiveCodecs.ed25519Signature.encode(sig).require
    bits.bytes.size shouldBe 64
    PrimitiveCodecs.ed25519Signature.decode(bits).require.value shouldBe sig
  }

  test("timeslot encodes as u32 little-endian") {
    val ts = Timeslot(0x01020304)
    val bits = PrimitiveCodecs.timeslot.encode(ts).require
    bits.bytes.toArray shouldBe Array[Byte](0x04, 0x03, 0x02, 0x01)
    PrimitiveCodecs.timeslot.decode(bits).require.value shouldBe ts
  }

  test("validatorIndex encodes as u16 little-endian") {
    val vi = ValidatorIndex(0x0102)
    val bits = PrimitiveCodecs.validatorIndex.encode(vi).require
    bits.bytes.toArray shouldBe Array[Byte](0x02, 0x01)
    PrimitiveCodecs.validatorIndex.decode(bits).require.value shouldBe vi
  }

  test("compactBytes matches both legacy variants byte-for-byte") {
    import scodec.codecs.{variableSizeBytes, variableSizeBytesLong, bytes}
    val legacyInt = variableSizeBytes(JamCodecs.compactInt, bytes)
    val legacyLong = variableSizeBytesLong(JamCodecs.compactInteger, bytes)
    for len <- List(0, 1, 63, 64, 127, 128, 300, 70000) do
      val payload =
        scodec.bits.ByteVector(Array.tabulate[Byte](len)(i => (i % 251).toByte))
      val canonical =
        PrimitiveCodecs.compactBytes.encode(JamBytes.fromByteVector(payload)).require
      withClue(s"len=$len: ") {
        legacyInt.encode(payload).require shouldBe canonical
        legacyLong.encode(payload).require shouldBe canonical
        PrimitiveCodecs.compactBytes
          .decode(canonical)
          .require
          .value
          .toByteVector shouldBe payload
      }
  }

  test(
    "JamCodecs optionCodec/compactPrefixedList are wire-identical to history's local versions"
  ) {
    import scodec.codecs.{byte, discriminated, listOfN, provide, uint8}

    // Verbatim copies of history.scala's private local implementations.
    def historyOptionCodec[A](codec: Codec[A]): Codec[Option[A]] =
      discriminated[Option[A]]
        .by(byte)
        .subcaseP(0) { case None => None }(provide(None))
        .subcaseP(1) { case Some(v) => Some(v) }(codec.xmap(Some(_), _.get))

    def historyCompactPrefixedList[A](codec: Codec[A]): Codec[List[A]] =
      listOfN(JamCodecs.compactInt, codec)

    val elem: Codec[Int] = uint8

    val localOpt = historyOptionCodec(elem)
    val canonicalOpt = JamCodecs.optionCodec(elem)
    for v <- List(None, Some(0), Some(1), Some(127), Some(255)) do
      withClue(s"option=$v: ") {
        localOpt.encode(v).require shouldBe canonicalOpt.encode(v).require
        canonicalOpt
          .decode(localOpt.encode(v).require)
          .require
          .value shouldBe v
      }

    val localList = historyCompactPrefixedList(elem)
    val canonicalList = JamCodecs.compactPrefixedList(elem)
    val cases: List[List[Int]] =
      List(Nil, List(1), List.tabulate(200)(i => i % 256))
    for v <- cases do
      withClue(s"list.size=${v.size}: ") {
        localList.encode(v).require shouldBe canonicalList.encode(v).require
        canonicalList
          .decode(localList.encode(v).require)
          .require
          .value shouldBe v
      }
  }
