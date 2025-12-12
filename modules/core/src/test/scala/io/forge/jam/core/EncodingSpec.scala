package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, ULong, UByte, UShort}
import codec.*

class EncodingSpec extends AnyFlatSpec with Matchers:

  "encodeVarint" should "encode small values in 1 byte" in {
    encodeVarint(UInt(0)) shouldBe Array(0.toByte)
    encodeVarint(UInt(1)) shouldBe Array(1.toByte)
    encodeVarint(UInt(127)) shouldBe Array(127.toByte)
  }
  
  it should "encode larger values in multiple bytes" in {
    encodeVarint(UInt(128)) shouldBe Array(0x80.toByte, 0x01.toByte)
    encodeVarint(UInt(16383)) shouldBe Array(0xFF.toByte, 0x7F.toByte)
  }
  
  "decodeVarint" should "round-trip correctly" in {
    val values = Seq(UInt(0), UInt(1), UInt(127), UInt(128), UInt(16383), UInt(-1))
    for v <- values do
      val encoded = encodeVarint(v)
      val (decoded, consumed) = decodeVarint(encoded)
      decoded shouldBe v
      consumed shouldBe encoded.length
  }

  "encodeU32LE" should "produce little-endian bytes" in {
    encodeU32LE(UInt(0x12345678)) shouldBe Array(0x78, 0x56, 0x34, 0x12).map(_.toByte)
  }

  "decodeU32LE" should "decode little-endian bytes" in {
    val bytes = Array(0x78, 0x56, 0x34, 0x12).map(_.toByte)
    decodeU32LE(bytes) shouldBe UInt(0x12345678)
  }

  "encodeU64LE/decodeU64LE" should "round-trip correctly" in {
    val values = Seq(
      ULong(0),
      ULong(1),
      ULong(0x123456789ABCDEFL),
      ULong(-1L)
    )
    for v <- values do
      val encoded = encodeU64LE(v)
      encoded.length shouldBe 8
      decodeU64LE(encoded) shouldBe v
  }
