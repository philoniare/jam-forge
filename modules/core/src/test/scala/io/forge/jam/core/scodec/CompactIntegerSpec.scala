package io.forge.jam.core.scodec

import scodec.bits.BitVector
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompactIntegerSpec extends AnyFlatSpec with Matchers:

  import JamCodecs.compactInteger
  "JamCodecs.compactInteger" should "round-trip canonical values across class boundaries" in {
    val values = Seq(
      0L, 1L, 127L, 128L, 255L, 256L, 16383L, 16384L, 65535L, 65536L,
      (1L << 20), (1L << 32), (1L << 48), (1L << 56) - 1, (1L << 56)
    )
    for v <- values do
      val enc = compactInteger.encode(v).require
      withClue(s"value=$v: ") {
        compactInteger.decode(enc).require.value shouldBe v
      }
  }

  it should "use the minimal byte length on encode at class boundaries" in {
    JamCodecs.encodeCompactInteger(0L).length shouldBe 1
    JamCodecs.encodeCompactInteger(127L).length shouldBe 1   // top of the l=0 class
    JamCodecs.encodeCompactInteger(128L).length shouldBe 2   // first l=1 value (2^7)
  }

  it should "reject a non-minimal encoding (SER-09)" in {
    // [0x80, 0x05] encodes value 5 in the l=1 (2-byte) class; 5 fits the 1-byte
    // class, so this is non-minimal and must be rejected.
    compactInteger.decode(BitVector(Array[Byte](0x80.toByte, 0x05.toByte))).isFailure shouldBe true
    // The minimal form decodes fine.
    compactInteger.decode(BitVector(Array[Byte](0x05.toByte))).require.value shouldBe 5L
  }
