package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JamBytesSpec extends AnyFlatSpec with Matchers:

  "JamBytes.empty" should "create an empty JamBytes" in {
    val empty = JamBytes.empty
    empty.length shouldBe 0
    empty.isEmpty shouldBe true
  }

  "JamBytes.apply(Array[Byte])" should "create from byte array" in {
    val bytes = JamBytes(Array[Byte](1, 2, 3))
    bytes.length shouldBe 3
    bytes(0) shouldBe 1
  }

  "JamBytes.apply(Byte*)" should "create from varargs" in {
    val bytes = JamBytes(1.toByte, 2.toByte, 3.toByte)
    bytes.length shouldBe 3
  }

  "JamBytes.fromSeq" should "create from Seq[Byte]" in {
    val bytes = JamBytes.fromSeq(Seq(1.toByte, 2.toByte))
    bytes.length shouldBe 2
  }

  "JamBytes.zeros" should "create zero-filled bytes" in {
    val zeros = JamBytes.zeros(5)
    zeros.length shouldBe 5
    zeros.toArray.forall(_ == 0) shouldBe true
  }

  "JamBytes.fill" should "create filled bytes" in {
    val filled = JamBytes.fill(3)(0x42.toByte)
    filled.length shouldBe 3
    filled.toArray.forall(_ == 0x42.toByte) shouldBe true
  }

  "JamBytes.fromHex" should "parse valid hex string" in {
    JamBytes.fromHex("0102ff") shouldBe Right(JamBytes(Array[Byte](1, 2, -1)))
  }

  it should "handle 0x prefix" in {
    JamBytes.fromHex("0x0102") shouldBe Right(JamBytes(Array[Byte](1, 2)))
  }

  it should "return Left for invalid hex" in {
    JamBytes.fromHex("ghij").isLeft shouldBe true
  }

  "JamBytes.fromHexUnsafe" should "parse valid hex" in {
    val bytes = JamBytes.fromHexUnsafe("0102")
    bytes.toArray shouldBe Array[Byte](1, 2)
  }

  it should "throw for invalid hex" in {
    assertThrows[IllegalArgumentException] {
      JamBytes.fromHexUnsafe("invalid")
    }
  }

  "JamBytes.concat" should "concatenate multiple JamBytes" in {
    val a = JamBytes(Array[Byte](1))
    val b = JamBytes(Array[Byte](2))
    val c = JamBytes(Array[Byte](3))
    JamBytes.concat(a, b, c).toArray shouldBe Array[Byte](1, 2, 3)
  }

  it should "handle empty parts" in {
    JamBytes.concat().isEmpty shouldBe true
  }

  "length and size" should "return correct values" in {
    val bytes = JamBytes(Array[Byte](1, 2, 3))
    bytes.length shouldBe 3
    bytes.size shouldBe 3L
  }

  "isEmpty and nonEmpty" should "correctly report state" in {
    JamBytes.empty.isEmpty shouldBe true
    JamBytes.empty.nonEmpty shouldBe false
    JamBytes(Array[Byte](1)).isEmpty shouldBe false
    JamBytes(Array[Byte](1)).nonEmpty shouldBe true
  }

  "apply(Long)" should "access byte at index" in {
    val bytes = JamBytes(Array[Byte](10, 20, 30))
    bytes(0L) shouldBe 10
    bytes(2L) shouldBe 30
  }

  "apply(Int)" should "access byte at index" in {
    val bytes = JamBytes(Array[Byte](10, 20, 30))
    bytes(1) shouldBe 20
  }

  "signedAt" should "return signed byte value" in {
    val bytes = JamBytes(Array[Byte](-1))
    bytes.signedAt(0) shouldBe -1
  }

  "toArray" should "convert to Array[Byte]" in {
    val bytes = JamBytes(Array[Byte](1, 2, 3))
    bytes.toArray shouldBe Array[Byte](1, 2, 3)
  }

  "toSeq" should "convert to Seq[Byte]" in {
    val bytes = JamBytes(Array[Byte](1, 2))
    bytes.toSeq shouldBe Seq(1.toByte, 2.toByte)
  }

  "toHex" should "return hex string without prefix" in {
    JamBytes(Array[Byte](0x0a, 0x0b)).toHex shouldBe "0a0b"
  }

  "toHexWithPrefix" should "return hex string with 0x prefix" in {
    JamBytes(Array[Byte](0x0a, 0x0b)).toHexWithPrefix shouldBe "0x0a0b"
  }

  "toByteVector" should "return underlying ByteVector" in {
    val bytes = JamBytes(Array[Byte](1, 2))
    bytes.toByteVector.toArray shouldBe Array[Byte](1, 2)
  }

  "bytes" should "return underlying ByteVector" in {
    val bytes = JamBytes(Array[Byte](1, 2))
    bytes.bytes.toArray shouldBe Array[Byte](1, 2)
  }

  "++" should "concatenate two JamBytes" in {
    val a = JamBytes(Array[Byte](1, 2))
    val b = JamBytes(Array[Byte](3, 4))
    (a ++ b).toArray shouldBe Array[Byte](1, 2, 3, 4)
  }

  ":+" should "append a byte" in {
    val bytes = JamBytes(Array[Byte](1, 2))
    (bytes :+ 3.toByte).toArray shouldBe Array[Byte](1, 2, 3)
  }

  "+:" should "prepend a byte" in {
    val bytes = JamBytes(Array[Byte](2, 3))
    (1.toByte +: bytes).toArray shouldBe Array[Byte](1, 2, 3)
  }

  "take" should "take first n bytes" in {
    val bytes = JamBytes(Array[Byte](1, 2, 3, 4))
    bytes.take(2).toArray shouldBe Array[Byte](1, 2)
  }

  "drop" should "drop first n bytes" in {
    val bytes = JamBytes(Array[Byte](1, 2, 3, 4))
    bytes.drop(2).toArray shouldBe Array[Byte](3, 4)
  }

  "slice" should "extract a range" in {
    val bytes = JamBytes(Array[Byte](1, 2, 3, 4, 5))
    bytes.slice(1, 4).toArray shouldBe Array[Byte](2, 3, 4)
  }

  "reverse" should "reverse byte order" in {
    val bytes = JamBytes(Array[Byte](1, 2, 3))
    bytes.reverse.toArray shouldBe Array[Byte](3, 2, 1)
  }

  "copyToArray" should "copy bytes to destination array" in {
    val src = JamBytes(Array[Byte](1, 2, 3, 4))
    val dest = new Array[Byte](10)
    src.copyToArray(dest, 2, 1, 2)
    dest(2) shouldBe 2
    dest(3) shouldBe 3
  }

  "foldLeft" should "accumulate values" in {
    val bytes = JamBytes(Array[Byte](1, 2, 3))
    val sum = bytes.foldLeft(0)((acc, b) => acc + b.toInt)
    sum shouldBe 6
  }

  "toString" should "provide readable representation" in {
    val bytes = JamBytes(Array[Byte](0xab.toByte, 0xcd.toByte))
    bytes.toString should include("JamBytes")
    bytes.toString should include("abcd")
  }

  "compareUnsigned" should "compare byte arrays lexicographically" in {
    JamBytes.compareUnsigned(Array(1, 2), Array(1, 2)) shouldBe 0
    JamBytes.compareUnsigned(Array(1, 2), Array(1, 3)) should be < 0
    JamBytes.compareUnsigned(Array(1, 3), Array(1, 2)) should be > 0
  }

  it should "handle different lengths" in {
    JamBytes.compareUnsigned(Array(1, 2), Array(1, 2, 3)) should be < 0
    JamBytes.compareUnsigned(Array(1, 2, 3), Array(1, 2)) should be > 0
  }

  it should "use unsigned comparison for high bytes" in {
    // 0xFF as unsigned is 255, greater than 0x01
    JamBytes.compareUnsigned(Array(0xff.toByte), Array(0x01.toByte)) should be > 0
  }

  "Builder" should "construct JamBytes incrementally" in {
    val builder = JamBytes.newBuilder
    builder += 1.toByte
    builder ++= Array[Byte](2, 3)
    builder ++= JamBytes(Array[Byte](4, 5))
    val result = builder.result()
    result.toArray shouldBe Array[Byte](1, 2, 3, 4, 5)
  }

  "Builder.size" should "report current size" in {
    val builder = JamBytes.newBuilder
    builder.size shouldBe 0
    builder += 1.toByte
    builder.size shouldBe 1
    builder ++= Array[Byte](2, 3)
    builder.size shouldBe 3
  }

  "Builder.clear" should "reset the builder" in {
    val builder = JamBytes.newBuilder
    builder ++= Array[Byte](1, 2, 3)
    builder.clear()
    builder.size shouldBe 0
    builder.result().isEmpty shouldBe true
  }

  "Ordering[JamBytes]" should "compare lexicographically" in {
    val ordering = summon[Ordering[JamBytes]]
    val a = JamBytes(Array[Byte](1, 2))
    val b = JamBytes(Array[Byte](1, 3))
    ordering.compare(a, b) should be < 0
    ordering.compare(b, a) should be > 0
    ordering.compare(a, a) shouldBe 0
  }

  it should "support sorting" in {
    val list = List(
      JamBytes(Array[Byte](3)),
      JamBytes(Array[Byte](1)),
      JamBytes(Array[Byte](2))
    )
    val sorted = list.sorted
    sorted(0).toArray shouldBe Array[Byte](1)
    sorted(1).toArray shouldBe Array[Byte](2)
    sorted(2).toArray shouldBe Array[Byte](3)
  }
