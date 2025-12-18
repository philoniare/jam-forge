package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.{UByte, UShort, UInt}
import _root_.scodec.{Codec, Attempt, DecodeResult}
import _root_.scodec.bits.BitVector

import primitives.*
import types.tickets.*
import types.epoch.*
import types.work.*
import types.dispute.*

class SimpleTypesSpec extends AnyFlatSpec with Matchers:

  // Helper method to encode using scodec Codec
  private def encode[A](value: A)(using codec: Codec[A]): JamBytes =
    JamBytes.fromByteVector(codec.encode(value).require.bytes)

  // Helper method to decode using scodec Codec
  private def decode[A](bytes: JamBytes)(using codec: Codec[A]): (A, Long) =
    codec.decode(BitVector(bytes.toByteVector)) match
      case Attempt.Successful(DecodeResult(value, remainder)) =>
        (value, bytes.length - remainder.bytes.size)
      case Attempt.Failure(err) =>
        throw new RuntimeException(s"Decode failed: $err")

  // ============================================================================
  // Test 1: TicketEnvelope encode/decode (785 bytes fixed)
  // ============================================================================

  "TicketEnvelope" should "encode to exactly 785 bytes" in {
    val signature = JamBytes.fill(RingVrfSignatureSize)(0x42.toByte)
    val envelope = TicketEnvelope(UByte(3), signature)
    val encoded = encode(envelope)
    encoded.length shouldBe TicketEnvelope.Size
    encoded.length shouldBe 785
  }

  it should "encode attempt as first byte" in {
    val signature = JamBytes.fill(RingVrfSignatureSize)(0x00.toByte)
    val envelope = TicketEnvelope(UByte(42), signature)
    val encoded = encode(envelope)
    encoded(0) shouldBe 42.toByte
  }

  it should "round-trip correctly" in {
    val signature = JamBytes.fromHexUnsafe("ab" * RingVrfSignatureSize)
    val envelope = TicketEnvelope(UByte(7), signature)
    val encoded = encode(envelope)
    val (decoded, consumed) = decode[TicketEnvelope](encoded)
    consumed shouldBe TicketEnvelope.Size
    decoded.attempt shouldBe envelope.attempt
    decoded.signature shouldBe envelope.signature
  }

  // ============================================================================
  // Test 2: TicketMark encode/decode (33 bytes fixed)
  // ============================================================================

  "TicketMark" should "encode to exactly 33 bytes" in {
    val id = JamBytes.fill(32)(0xAB.toByte)
    val mark = TicketMark(id, UByte(5))
    val encoded = encode(mark)
    encoded.length shouldBe TicketMark.Size
    encoded.length shouldBe 33
  }

  it should "encode id first, then attempt" in {
    val id = JamBytes.fill(32)(0xFF.toByte)
    val mark = TicketMark(id, UByte(1))
    val encoded = encode(mark)
    // First 32 bytes should be the id
    encoded.slice(0, 32) shouldBe id
    // Last byte should be the attempt
    encoded(32) shouldBe 1.toByte
  }

  it should "round-trip correctly" in {
    val id = JamBytes.fromHexUnsafe("deadbeef" * 8) // 32 bytes
    val mark = TicketMark(id, UByte(255))
    val encoded = encode(mark)
    val (decoded, consumed) = decode[TicketMark](encoded)
    consumed shouldBe TicketMark.Size
    decoded.id shouldBe mark.id
    decoded.attempt shouldBe mark.attempt
  }

  // ============================================================================
  // Test 3: PackageSpec encode/decode (102 bytes fixed)
  // ============================================================================

  "PackageSpec" should "encode to exactly 102 bytes" in {
    val hash = Hash(Array.fill(32)(0x11.toByte))
    val erasureRoot = Hash(Array.fill(32)(0x22.toByte))
    val exportsRoot = Hash(Array.fill(32)(0x33.toByte))
    val spec = PackageSpec(hash, UInt(1000), erasureRoot, exportsRoot, UShort(10))
    val encoded = encode(spec)
    encoded.length shouldBe PackageSpec.Size
    encoded.length shouldBe 102
  }

  it should "encode length as 4-byte little-endian at offset 32" in {
    val hash = Hash.zero
    val erasureRoot = Hash.zero
    val exportsRoot = Hash.zero
    val spec = PackageSpec(hash, UInt(0x12345678), erasureRoot, exportsRoot, UShort(0))
    val encoded = encode(spec)
    // Length is at offset 32, 4 bytes little-endian
    encoded(32) shouldBe 0x78.toByte
    encoded(33) shouldBe 0x56.toByte
    encoded(34) shouldBe 0x34.toByte
    encoded(35) shouldBe 0x12.toByte
  }

  it should "round-trip correctly" in {
    val hash = Hash(Array.tabulate(32)(i => i.toByte))
    val erasureRoot = Hash(Array.tabulate(32)(i => (i + 32).toByte))
    val exportsRoot = Hash(Array.tabulate(32)(i => (i + 64).toByte))
    val spec = PackageSpec(hash, UInt(999999), erasureRoot, exportsRoot, UShort(65535))
    val encoded = encode(spec)
    val (decoded, consumed) = decode[PackageSpec](encoded)
    consumed shouldBe PackageSpec.Size
    decoded.hash.toHex shouldBe spec.hash.toHex
    decoded.length shouldBe spec.length
    decoded.erasureRoot.toHex shouldBe spec.erasureRoot.toHex
    decoded.exportsRoot.toHex shouldBe spec.exportsRoot.toHex
    decoded.exportsCount shouldBe spec.exportsCount
  }

  // ============================================================================
  // Test 4: EpochValidatorKey encode/decode (64 bytes fixed)
  // ============================================================================

  "EpochValidatorKey" should "encode to exactly 64 bytes" in {
    val bandersnatch = BandersnatchPublicKey(Array.fill(32)(0xAA.toByte))
    val ed25519 = Ed25519PublicKey(Array.fill(32)(0xBB.toByte))
    val key = EpochValidatorKey(bandersnatch, ed25519)
    val encoded = encode(key)
    encoded.length shouldBe EpochValidatorKey.Size
    encoded.length shouldBe 64
  }

  it should "encode bandersnatch first, then ed25519" in {
    val bandersnatch = BandersnatchPublicKey(Array.fill(32)(0x11.toByte))
    val ed25519 = Ed25519PublicKey(Array.fill(32)(0x22.toByte))
    val key = EpochValidatorKey(bandersnatch, ed25519)
    val encoded = encode(key)
    // First 32 bytes should be bandersnatch
    encoded.slice(0, 32).toArray.forall(_ == 0x11.toByte) shouldBe true
    // Next 32 bytes should be ed25519
    encoded.slice(32, 64).toArray.forall(_ == 0x22.toByte) shouldBe true
  }

  it should "round-trip correctly" in {
    val bandersnatch = BandersnatchPublicKey(Array.tabulate(32)(i => i.toByte))
    val ed25519 = Ed25519PublicKey(Array.tabulate(32)(i => (i + 100).toByte))
    val key = EpochValidatorKey(bandersnatch, ed25519)
    val encoded = encode(key)
    val (decoded, consumed) = decode[EpochValidatorKey](encoded)
    consumed shouldBe EpochValidatorKey.Size
    decoded.bandersnatch.bytes.toSeq shouldBe key.bandersnatch.bytes.toSeq
    decoded.ed25519.bytes.toSeq shouldBe key.ed25519.bytes.toSeq
  }

  // ============================================================================
  // Test 5: Vote encode/decode (67 bytes fixed)
  // ============================================================================

  "Vote" should "encode to exactly 67 bytes" in {
    val signature = Ed25519Signature(Array.fill(64)(0xCC.toByte))
    val vote = Vote(true, ValidatorIndex(100), signature)
    val encoded = encode(vote)
    encoded.length shouldBe Vote.Size
    encoded.length shouldBe 67
  }

  it should "encode vote boolean as first byte" in {
    val signature = Ed25519Signature(Array.fill(64)(0x00.toByte))
    val voteTrue = Vote(true, ValidatorIndex(0), signature)
    val voteFalse = Vote(false, ValidatorIndex(0), signature)
    val encodedTrue = encode(voteTrue)
    val encodedFalse = encode(voteFalse)
    encodedTrue(0) shouldBe 1.toByte
    encodedFalse(0) shouldBe 0.toByte
  }

  it should "encode validator index as 2-byte little-endian at offset 1" in {
    val signature = Ed25519Signature(Array.fill(64)(0x00.toByte))
    val vote = Vote(false, ValidatorIndex(0x1234), signature)
    val encoded = encode(vote)
    encoded(1) shouldBe 0x34.toByte
    encoded(2) shouldBe 0x12.toByte
  }

  it should "round-trip correctly" in {
    val signature = Ed25519Signature(Array.tabulate(64)(i => i.toByte))
    val vote = Vote(true, ValidatorIndex(999), signature)
    val encoded = encode(vote)
    val (decoded, consumed) = decode[Vote](encoded)
    consumed shouldBe Vote.Size
    decoded.vote shouldBe vote.vote
    decoded.validatorIndex.toInt shouldBe vote.validatorIndex.toInt
    decoded.signature.bytes.toSeq shouldBe vote.signature.bytes.toSeq
  }

  // ============================================================================
  // Test 6: ExecutionResult encode/decode
  // ============================================================================

  "ExecutionResult.Ok" should "encode with 0x00 tag followed by compact length and data" in {
    val output = JamBytes(Array[Byte](1, 2, 3, 4, 5))
    val result = ExecutionResult.Ok(output)
    val encoded = encode(result)
    encoded(0) shouldBe 0x00.toByte
    // Compact length of 5 is just 5
    encoded(1) shouldBe 5.toByte
    encoded.slice(2, 7) shouldBe output
  }

  "ExecutionResult.Panic" should "encode as single 0x02 byte" in {
    val result = ExecutionResult.Panic
    val encoded = encode(result)
    encoded.length shouldBe 1
    encoded(0) shouldBe 0x02.toByte
  }

  "ExecutionResult" should "round-trip Ok correctly" in {
    val output = JamBytes.fill(100)(0xAB.toByte)
    val result = ExecutionResult.Ok(output)
    val encoded = encode(result)
    val (decoded, consumed) = decode[ExecutionResult](encoded)
    decoded match
      case ExecutionResult.Ok(decodedOutput) =>
        decodedOutput shouldBe output
      case ExecutionResult.Panic =>
        fail("Expected Ok, got Panic")
    consumed shouldBe encoded.length
  }

  it should "round-trip Panic correctly" in {
    val result = ExecutionResult.Panic
    val encoded = encode(result)
    val (decoded, consumed) = decode[ExecutionResult](encoded)
    decoded shouldBe ExecutionResult.Panic
    consumed shouldBe 1
  }

  // ============================================================================
  // Test 7: Culprit encode/decode (128 bytes fixed)
  // ============================================================================

  "Culprit" should "encode to exactly 128 bytes" in {
    val target = Hash(Array.fill(32)(0x11.toByte))
    val key = Ed25519PublicKey(Array.fill(32)(0x22.toByte))
    val signature = Ed25519Signature(Array.fill(64)(0x33.toByte))
    val culprit = Culprit(target, key, signature)
    val encoded = encode(culprit)
    encoded.length shouldBe Culprit.Size
    encoded.length shouldBe 128
  }

  it should "round-trip correctly" in {
    val target = Hash(Array.tabulate(32)(i => i.toByte))
    val key = Ed25519PublicKey(Array.tabulate(32)(i => (i + 50).toByte))
    val signature = Ed25519Signature(Array.tabulate(64)(i => (i + 100).toByte))
    val culprit = Culprit(target, key, signature)
    val encoded = encode(culprit)
    val (decoded, consumed) = decode[Culprit](encoded)
    consumed shouldBe Culprit.Size
    decoded.target.toHex shouldBe culprit.target.toHex
    decoded.key.bytes.toSeq shouldBe culprit.key.bytes.toSeq
    decoded.signature.bytes.toSeq shouldBe culprit.signature.bytes.toSeq
  }

  // ============================================================================
  // Test 8: Fault encode/decode (129 bytes fixed)
  // ============================================================================

  "Fault" should "encode to exactly 129 bytes" in {
    val target = Hash(Array.fill(32)(0x44.toByte))
    val key = Ed25519PublicKey(Array.fill(32)(0x55.toByte))
    val signature = Ed25519Signature(Array.fill(64)(0x66.toByte))
    val fault = Fault(target, true, key, signature)
    val encoded = encode(fault)
    encoded.length shouldBe Fault.Size
    encoded.length shouldBe 129
  }

  it should "encode vote boolean at offset 32" in {
    val target = Hash.zero
    val key = Ed25519PublicKey(Array.fill(32)(0x00.toByte))
    val signature = Ed25519Signature(Array.fill(64)(0x00.toByte))
    val faultTrue = Fault(target, true, key, signature)
    val faultFalse = Fault(target, false, key, signature)
    val encodedTrue = encode(faultTrue)
    val encodedFalse = encode(faultFalse)
    encodedTrue(32) shouldBe 1.toByte
    encodedFalse(32) shouldBe 0.toByte
  }

  it should "round-trip correctly" in {
    val target = Hash(Array.tabulate(32)(i => (i * 2).toByte))
    val key = Ed25519PublicKey(Array.tabulate(32)(i => (i * 3).toByte))
    val signature = Ed25519Signature(Array.tabulate(64)(i => (i * 4).toByte))
    val fault = Fault(target, false, key, signature)
    val encoded = encode(fault)
    val (decoded, consumed) = decode[Fault](encoded)
    consumed shouldBe Fault.Size
    decoded.target.toHex shouldBe fault.target.toHex
    decoded.vote shouldBe fault.vote
    decoded.key.bytes.toSeq shouldBe fault.key.bytes.toSeq
    decoded.signature.bytes.toSeq shouldBe fault.signature.bytes.toSeq
  }

  // ============================================================================
  // Test 9: GuaranteeSignature encode/decode (66 bytes fixed)
  // ============================================================================

  "GuaranteeSignature" should "encode to exactly 66 bytes" in {
    val signature = Ed25519Signature(Array.fill(64)(0x77.toByte))
    val gs = GuaranteeSignature(ValidatorIndex(42), signature)
    val encoded = encode(gs)
    encoded.length shouldBe GuaranteeSignature.Size
    encoded.length shouldBe 66
  }

  it should "encode validator index as 2-byte little-endian first" in {
    val signature = Ed25519Signature(Array.fill(64)(0x00.toByte))
    val gs = GuaranteeSignature(ValidatorIndex(0xABCD), signature)
    val encoded = encode(gs)
    encoded(0) shouldBe 0xCD.toByte
    encoded(1) shouldBe 0xAB.toByte
  }

  it should "round-trip correctly" in {
    val signature = Ed25519Signature(Array.tabulate(64)(i => (i + 200).toByte))
    val gs = GuaranteeSignature(ValidatorIndex(1022), signature)
    val encoded = encode(gs)
    val (decoded, consumed) = decode[GuaranteeSignature](encoded)
    consumed shouldBe GuaranteeSignature.Size
    decoded.validatorIndex.toInt shouldBe gs.validatorIndex.toInt
    decoded.signature.bytes.toSeq shouldBe gs.signature.bytes.toSeq
  }

  // ============================================================================
  // Test 10: EpochMark encode/decode (config-dependent size)
  // ============================================================================

  "EpochMark" should "encode with correct size for TINY config" in {
    val entropy = Hash(Array.fill(32)(0xEE.toByte))
    val ticketsEntropy = Hash(Array.fill(32)(0xFF.toByte))
    val validators = (0 until 6).map { i =>
      EpochValidatorKey(
        BandersnatchPublicKey(Array.fill(32)(i.toByte)),
        Ed25519PublicKey(Array.fill(32)((i + 10).toByte))
      )
    }.toList
    val epochMark = EpochMark(entropy, ticketsEntropy, validators)
    // Use the codec factory for 6 validators
    val codec = EpochMark.epochMarkCodec(6)
    val encoded = codec.encode(epochMark).require.bytes
    encoded.length shouldBe EpochMark.size(6)
    encoded.length shouldBe (32 + 32 + 6 * 64) // 448 bytes
  }

  it should "round-trip correctly with decoder" in {
    val entropy = Hash(Array.tabulate(32)(i => i.toByte))
    val ticketsEntropy = Hash(Array.tabulate(32)(i => (i + 100).toByte))
    val validators = (0 until 3).map { i =>
      EpochValidatorKey(
        BandersnatchPublicKey(Array.fill(32)((i * 11).toByte)),
        Ed25519PublicKey(Array.fill(32)((i * 13).toByte))
      )
    }.toList
    val epochMark = EpochMark(entropy, ticketsEntropy, validators)
    // Use the codec factory for 3 validators
    val codec = EpochMark.epochMarkCodec(3)
    val encoded = codec.encode(epochMark).require.bytes
    val decoded = codec.decode(BitVector(encoded)).require.value
    decoded.entropy.toHex shouldBe epochMark.entropy.toHex
    decoded.ticketsEntropy.toHex shouldBe epochMark.ticketsEntropy.toHex
    decoded.validators.length shouldBe epochMark.validators.length
    for (orig, dec) <- epochMark.validators.zip(decoded.validators) do
      dec.bandersnatch.bytes.toSeq shouldBe orig.bandersnatch.bytes.toSeq
      dec.ed25519.bytes.toSeq shouldBe orig.ed25519.bytes.toSeq
  }
