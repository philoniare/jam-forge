package io.forge.jam.core.codec
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
      val enc = compactInteger.encode(v)
      withClue(s"value=$v: ") {
        compactInteger.decodeExact(enc) shouldBe Right(v)
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
    compactInteger.decode(Array[Byte](0x80.toByte, 0x05.toByte)).isLeft shouldBe true
    // The minimal form decodes fine.
    compactInteger.decode(Array[Byte](0x05.toByte)) shouldBe Right(5L)
  }

  it should "encode GP eq. (C.6) byte patterns" in {
    compactInteger.encode(0L) shouldBe Array[Byte](0)
    compactInteger.encode(128L) shouldBe Array[Byte](0x80.toByte, 0x80.toByte)
    compactInteger.encode(16384L) shouldBe Array[Byte](0xc0.toByte, 0x00, 0x40)
    compactInteger.encode(-1L) shouldBe (0xff.toByte +: Array.fill[Byte](8)(0xff.toByte))
    compactInteger.decode(0xff.toByte +: Array.fill[Byte](8)(0xff.toByte)) shouldBe Right(-1L)
  }

  it should "reject truncated input" in {
    compactInteger.decode(Array[Byte](0xc0.toByte, 0x00)).isLeft shouldBe true
    compactInteger.decode(Array.emptyByteArray).isLeft shouldBe true
  }
