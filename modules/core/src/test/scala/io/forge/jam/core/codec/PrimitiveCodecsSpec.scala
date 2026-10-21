package io.forge.jam.core.codec

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Ed25519Signature, Timeslot, ValidatorIndex}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PrimitiveCodecsSpec extends AnyFunSuite with Matchers:

  test("ed25519Signature round-trips and is 64 bytes on the wire") {
    val sig = Ed25519Signature(Array.tabulate[Byte](64)(_.toByte))
    val bytes = PrimitiveCodecs.ed25519Signature.encode(sig)
    bytes.length shouldBe 64
    PrimitiveCodecs.ed25519Signature.decodeExact(bytes) shouldBe Right(sig)
  }

  test("timeslot encodes as u32 little-endian") {
    val ts = Timeslot(0x01020304)
    val bytes = PrimitiveCodecs.timeslot.encode(ts)
    bytes shouldBe Array[Byte](0x04, 0x03, 0x02, 0x01)
    PrimitiveCodecs.timeslot.decodeExact(bytes) shouldBe Right(ts)
  }

  test("validatorIndex encodes as u16 little-endian") {
    val vi = ValidatorIndex(0x0102)
    val bytes = PrimitiveCodecs.validatorIndex.encode(vi)
    bytes shouldBe Array[Byte](0x02, 0x01)
    PrimitiveCodecs.validatorIndex.decodeExact(bytes) shouldBe Right(vi)
  }

  test("GP: compactBytes is the compact length followed by the payload (eq. C.8)") {
    for len <- List(0, 1, 63, 64, 127, 128, 300, 70000) do
      val payload = Array.tabulate[Byte](len)(i => (i % 251).toByte)
      val encoded = PrimitiveCodecs.compactBytes.encode(JamBytes(payload))
      withClue(s"len=$len: ") {
        encoded shouldBe (JamCodecs.encodeCompactInteger(len.toLong) ++ payload)
        PrimitiveCodecs.compactBytes.decodeExact(encoded) shouldBe Right(JamBytes(payload))
      }
  }

  test("compactBytes rejects a length beyond the input") {
    PrimitiveCodecs.compactBytes.decode(Array[Byte](5, 1, 2)).isLeft shouldBe true
  }

  test("GP: optionCodec is a 0/1 discriminator then the value (eq. C.11)") {
    val opt = JamCodecs.optionCodec(codecs.uint8)
    opt.encode(None) shouldBe Array[Byte](0)
    opt.encode(Some(127)) shouldBe Array[Byte](1, 127)
    for v <- List(None, Some(0), Some(1), Some(127), Some(255)) do
      withClue(s"option=$v: ") {
        opt.decodeExact(opt.encode(v)) shouldBe Right(v)
      }
    opt.decode(Array[Byte](2, 0)).isLeft shouldBe true
  }

  test("GP: compactPrefixedList is a compact count then the elements (eq. C.8)") {
    val list = JamCodecs.compactPrefixedList(codecs.uint8)
    val cases: List[List[Int]] = List(Nil, List(1), List.tabulate(200)(i => i % 256))
    for v <- cases do
      withClue(s"list.size=${v.size}: ") {
        val encoded = list.encode(v)
        encoded shouldBe (JamCodecs.encodeCompactInteger(v.size.toLong) ++ v.map(_.toByte))
        list.decodeExact(encoded) shouldBe Right(v)
      }
  }

  test("tuple codecs encode fields in order and decode back") {
    val c = codecs.uint8 :: codecs.uint16L :: codecs.uint32L
    val encoded = c.encode((1, 0x0302, 0x07060504L))
    encoded shouldBe Array[Byte](1, 2, 3, 4, 5, 6, 7)
    c.decodeExact(encoded) shouldBe Right((1, 0x0302, 0x07060504L))
  }

  test("decodeExact rejects trailing bytes and decodeAt reports bytes consumed") {
    codecs.uint16L.decodeExact(Array[Byte](1, 2, 3)).isLeft shouldBe true
    codecs.uint16L.decodeAt(Array[Byte](9, 1, 2, 3), 1) shouldBe Right((0x0201, 2))
  }
