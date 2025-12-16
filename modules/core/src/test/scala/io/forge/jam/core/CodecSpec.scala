package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.{UByte, UShort, UInt, ULong}
import codec.*
import primitives.*

class CodecSpec extends AnyFlatSpec with Matchers:

  // ============================================================================
  // Test 1: JamEncoder for primitive unsigned types
  // ============================================================================

  "JamEncoder[UByte]" should "encode single byte correctly" in {
    val values = Seq(UByte(0), UByte(1), UByte(127), UByte(255))
    for v <- values do
      val encoded = v.encode
      encoded.length shouldBe 1
      encoded(0) shouldBe v.toByte
  }

  "JamEncoder[UShort]" should "encode 2 bytes little-endian" in {
    val v = UShort(0x1234)
    val encoded = v.encode
    encoded.length shouldBe 2
    encoded(0) shouldBe 0x34.toByte
    encoded(1) shouldBe 0x12.toByte
  }

  "JamEncoder[UInt]" should "encode 4 bytes little-endian" in {
    val v = UInt(0x12345678)
    val encoded = v.encode
    encoded.length shouldBe 4
    encoded(0) shouldBe 0x78.toByte
    encoded(1) shouldBe 0x56.toByte
    encoded(2) shouldBe 0x34.toByte
    encoded(3) shouldBe 0x12.toByte
  }

  "JamEncoder[ULong]" should "encode 8 bytes little-endian" in {
    val v = ULong(0x123456789ABCDEFL)
    val encoded = v.encode
    encoded.length shouldBe 8
    encoded(0) shouldBe 0xEF.toByte
    encoded(1) shouldBe 0xCD.toByte
    encoded(2) shouldBe 0xAB.toByte
    encoded(3) shouldBe 0x89.toByte
    encoded(4) shouldBe 0x67.toByte
    encoded(5) shouldBe 0x45.toByte
    encoded(6) shouldBe 0x23.toByte
    encoded(7) shouldBe 0x01.toByte
  }

  // ============================================================================
  // Test 2: JamDecoder round-trip for primitives
  // ============================================================================

  "JamDecoder[UByte]" should "round-trip correctly" in {
    val values = Seq(UByte(0), UByte(1), UByte(127), UByte(255))
    for v <- values do
      val encoded = v.encode
      val (decoded, consumed) = encoded.decodeAs[UByte]()
      decoded shouldBe v
      consumed shouldBe 1
  }

  "JamDecoder[UShort]" should "round-trip correctly" in {
    val values = Seq(UShort(0), UShort(1), UShort(0x1234), UShort(0xFFFF))
    for v <- values do
      val encoded = v.encode
      val (decoded, consumed) = encoded.decodeAs[UShort]()
      decoded shouldBe v
      consumed shouldBe 2
  }

  "JamDecoder[UInt]" should "round-trip correctly" in {
    val values = Seq(UInt(0), UInt(1), UInt(0x12345678), UInt(-1))
    for v <- values do
      val encoded = v.encode
      val (decoded, consumed) = encoded.decodeAs[UInt]()
      decoded shouldBe v
      consumed shouldBe 4
  }

  "JamDecoder[ULong]" should "round-trip correctly" in {
    val values = Seq(ULong(0), ULong(1), ULong(0x123456789ABCDEFL), ULong(-1L))
    for v <- values do
      val encoded = v.encode
      val (decoded, consumed) = encoded.decodeAs[ULong]()
      decoded shouldBe v
      consumed shouldBe 8
  }

  // ============================================================================
  // Test 3: JamEncoder/JamDecoder for JamBytes (variable-length encoding)
  // ============================================================================

  "JamEncoder[JamBytes]" should "encode with compact length prefix" in {
    val empty = JamBytes.empty
    val emptyEncoded = empty.encode
    emptyEncoded.length shouldBe 1
    emptyEncoded(0) shouldBe 0.toByte

    val small = JamBytes(Array[Byte](1, 2, 3, 4, 5))
    val smallEncoded = small.encode
    smallEncoded.length shouldBe 6
    smallEncoded(0) shouldBe 5.toByte
    smallEncoded.slice(1, 6) shouldBe small

    val medium = JamBytes.fill(200)(0x42.toByte)
    val mediumEncoded = medium.encode
    mediumEncoded.length shouldBe 202
  }

  "JamDecoder[JamBytes]" should "round-trip correctly" in {
    val testCases = Seq(
      JamBytes.empty,
      JamBytes(Array[Byte](1, 2, 3)),
      JamBytes.fill(127)(0x42.toByte),
      JamBytes.fill(200)(0xAB.toByte)
    )
    for v <- testCases do
      val encoded = v.encode
      val (decoded, consumed) = encoded.decodeAs[JamBytes]()
      decoded shouldBe v
      consumed shouldBe encoded.length
  }

  // ============================================================================
  // Test 4: CompactInt codec
  // ============================================================================

  "JamEncoder[CompactInt]" should "encode compact integers correctly" in {
    val zero = CompactInt(0)
    zero.encode.length shouldBe 1
    zero.encode(0) shouldBe 0.toByte

    val small = CompactInt(42)
    small.encode.length shouldBe 1
    small.encode(0) shouldBe 42.toByte

    val medium = CompactInt(128)
    medium.encode.length shouldBe 2
  }

  "JamDecoder[CompactInt]" should "round-trip correctly" in {
    val values = Seq(
      CompactInt(0),
      CompactInt(1),
      CompactInt(127),
      CompactInt(128),
      CompactInt(255),
      CompactInt(16383),
      CompactInt(16384),
      CompactInt(1000000)
    )
    for v <- values do
      val encoded = v.encode
      val (decoded, consumed) = encoded.decodeAs[CompactInt]()
      decoded.value shouldBe v.value
      consumed shouldBe encoded.length
  }

  // ============================================================================
  // Test 5: Option codec
  // ============================================================================

  "JamEncoder[Option]" should "encode None as 0 byte" in {
    val none: Option[UInt] = None
    val encoded = none.encode
    encoded.length shouldBe 1
    encoded(0) shouldBe 0.toByte
  }

  it should "encode Some as 1 byte prefix followed by value" in {
    val some: Option[UInt] = Some(UInt(0x12345678))
    val encoded = some.encode
    encoded.length shouldBe 5
    encoded(0) shouldBe 1.toByte
    encoded.slice(1, 5) shouldBe UInt(0x12345678).encode
  }

  "JamDecoder[Option]" should "round-trip correctly" in {
    val testCases: Seq[Option[UShort]] = Seq(None, Some(UShort(0)), Some(UShort(0x1234)))
    for v <- testCases do
      val encoded = v.encode
      val (decoded, consumed) = encoded.decodeAs[Option[UShort]]()
      decoded shouldBe v
      consumed shouldBe encoded.length
  }

  // ============================================================================
  // Test 6: List codec
  // ============================================================================

  "JamEncoder[List]" should "encode empty list" in {
    val empty: List[UByte] = Nil
    val encoded = empty.encode
    encoded.length shouldBe 1
    encoded(0) shouldBe 0.toByte
  }

  it should "encode non-empty list with compact length prefix" in {
    val list: List[UShort] = List(UShort(1), UShort(2), UShort(3))
    val encoded = list.encode
    encoded.length shouldBe 7
    encoded(0) shouldBe 3.toByte
  }

  "JamDecoder[List]" should "round-trip correctly" in {
    val testCases: Seq[List[UInt]] = Seq(
      Nil,
      List(UInt(1)),
      List(UInt(1), UInt(2), UInt(3)),
      (0 until 100).map(i => UInt(i)).toList
    )
    for v <- testCases do
      val encoded = v.encode
      val (decoded, consumed) = encoded.decodeAs[List[UInt]]()
      decoded shouldBe v
      consumed shouldBe encoded.length
  }

  // ============================================================================
  // Test 7: Hashing
  // ============================================================================

  "Hashing.blake2b256" should "produce 32-byte hash" in {
    val data = JamBytes(Array[Byte](1, 2, 3, 4, 5))
    val hash = Hashing.blake2b256(data)
    hash.size shouldBe 32
  }

  it should "produce consistent hashes for same input" in {
    val data = JamBytes.fromHexUnsafe("deadbeef")
    val hash1 = Hashing.blake2b256(data)
    val hash2 = Hashing.blake2b256(data)
    hash1.toHex shouldBe hash2.toHex
  }

  it should "produce expected hash for empty input" in {
    val hash = Hashing.blake2b256(JamBytes.empty)
    hash.size shouldBe 32
    hash.toHex shouldBe "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
  }

  "Hashing.keccak256" should "produce 32-byte hash" in {
    val data = JamBytes(Array[Byte](1, 2, 3, 4, 5))
    val hash = Hashing.keccak256(data)
    hash.size shouldBe 32
  }

  it should "produce expected hash for empty input" in {
    val hash = Hashing.keccak256(JamBytes.empty)
    hash.size shouldBe 32
    hash.toHex shouldBe "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
  }

  // ============================================================================
  // Test 8: ChainConfig
  // ============================================================================

  "ChainConfig.TINY" should "have correct values" in {
    ChainConfig.TINY.validatorCount shouldBe 6
    ChainConfig.TINY.coresCount shouldBe 2
    ChainConfig.TINY.epochLength shouldBe 12
    ChainConfig.TINY.votesPerVerdict shouldBe 5
  }

  "ChainConfig.FULL" should "have correct values" in {
    ChainConfig.FULL.validatorCount shouldBe 1023
    ChainConfig.FULL.coresCount shouldBe 341
    ChainConfig.FULL.epochLength shouldBe 600
    ChainConfig.FULL.votesPerVerdict shouldBe 683
  }

  "ChainConfig.votesPerVerdict" should "be computed correctly" in {
    val config = ChainConfig(
      validatorCount = 9,
      coresCount = 3,
      preimageExpungePeriod = 32,
      slotDuration = 6,
      epochLength = 12,
      contestDuration = 10,
      ticketsPerValidator = 3,
      maxTicketsPerExtrinsic = 3,
      rotationPeriod = 4,
      numEcPiecesPerSegment = 1026,
      maxBlockGas = 20_000_000L,
      maxRefineGas = 1_000_000_000L
    )
    config.votesPerVerdict shouldBe 7
  }
