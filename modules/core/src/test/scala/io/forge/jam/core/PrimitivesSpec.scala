package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.UInt
import primitives.*

class PrimitivesSpec extends AnyFlatSpec with Matchers:

  "Hash" should "be exactly 32 bytes" in {
    val bytes = Array.fill[Byte](32)(0x42)
    val hash = Hash(bytes)
    hash.size shouldBe 32
  }

  it should "reject wrong size" in {
    an[IllegalArgumentException] should be thrownBy {
      Hash(Array[Byte](1, 2, 3))
    }
  }

  it should "convert to/from hex" in {
    val hex = "0000000000000000000000000000000000000000000000000000000000000000"
    val hash = Hash.fromHex(hex)
    hash.isRight shouldBe true
    hash.toOption.get.toHex shouldBe hex
  }

  "ServiceId" should "wrap UInt correctly" in {
    val sid = ServiceId(42)
    sid.value shouldBe UInt(42)
    sid.toInt shouldBe 42
  }

  "Gas" should "support arithmetic" in {
    val g1 = Gas(1000L)
    val g2 = Gas(400L)
    (g1 - g2).toLong shouldBe 600L
    (g1 + g2).toLong shouldBe 1400L
    (g1 >= g2) shouldBe true
  }

  "Timeslot" should "support arithmetic" in {
    val ts = Timeslot(100)
    (ts + 5).value shouldBe UInt(105)
    (ts - 10).value shouldBe UInt(90)
  }

  "Hash.zero" should "create zero hash" in {
    val hash = Hash.zero
    hash.bytes.toArray.forall(_ == 0) shouldBe true
    hash.size shouldBe 32
  }

  "Hash.equals" should "compare by content" in {
    val h1 = Hash(Array.fill[Byte](32)(1))
    val h2 = Hash(Array.fill[Byte](32)(1))
    val h3 = Hash(Array.fill[Byte](32)(2))
    h1 shouldBe h2
    h1 should not be h3
  }

  "Hash.hashCode" should "be consistent with equals" in {
    val h1 = Hash(Array.fill[Byte](32)(0x42))
    val h2 = Hash(Array.fill[Byte](32)(0x42))
    h1.hashCode shouldBe h2.hashCode
  }

  "Ed25519PublicKey" should "create from Array[Byte]" in {
    val bytes = Array.fill[Byte](32)(0x42)
    val key = Ed25519PublicKey(bytes)
    key.bytes.toArray shouldBe bytes
  }

  "Ed25519PublicKey.equals" should "compare by content" in {
    val k1 = Ed25519PublicKey(Array.fill[Byte](32)(1))
    val k2 = Ed25519PublicKey(Array.fill[Byte](32)(1))
    val k3 = Ed25519PublicKey(Array.fill[Byte](32)(2))
    k1 shouldBe k2
    k1 should not be k3
  }

  "Ed25519PublicKey.hashCode" should "be consistent with equals" in {
    val k1 = Ed25519PublicKey(Array.fill[Byte](32)(0x42))
    val k2 = Ed25519PublicKey(Array.fill[Byte](32)(0x42))
    k1.hashCode shouldBe k2.hashCode
  }

  "Ed25519Signature" should "create from Array[Byte]" in {
    val bytes = Array.fill[Byte](64)(0x42)
    val sig = Ed25519Signature(bytes)
    sig.bytes.toArray shouldBe bytes
  }

  "Ed25519Signature.equals" should "compare by content" in {
    val s1 = Ed25519Signature(Array.fill[Byte](64)(1))
    val s2 = Ed25519Signature(Array.fill[Byte](64)(1))
    val s3 = Ed25519Signature(Array.fill[Byte](64)(2))
    s1 shouldBe s2
    s1 should not be s3
  }

  "Ed25519Signature.hashCode" should "be consistent with equals" in {
    val s1 = Ed25519Signature(Array.fill[Byte](64)(0x42))
    val s2 = Ed25519Signature(Array.fill[Byte](64)(0x42))
    s1.hashCode shouldBe s2.hashCode
  }

  "BandersnatchPublicKey" should "create from Array[Byte]" in {
    val bytes = Array.fill[Byte](32)(0x42)
    val key = BandersnatchPublicKey(bytes)
    key.bytes.toArray shouldBe bytes
  }

  it should "create zero" in {
    val key = BandersnatchPublicKey.zero
    key.bytes.toArray.forall(_ == 0) shouldBe true
  }

  "BandersnatchPublicKey.equals" should "compare by content" in {
    val k1 = BandersnatchPublicKey(Array.fill[Byte](32)(1))
    val k2 = BandersnatchPublicKey(Array.fill[Byte](32)(1))
    val k3 = BandersnatchPublicKey(Array.fill[Byte](32)(2))
    k1 shouldBe k2
    k1 should not be k3
  }

  "BandersnatchPublicKey.hashCode" should "be consistent with equals" in {
    val k1 = BandersnatchPublicKey(Array.fill[Byte](32)(0x42))
    val k2 = BandersnatchPublicKey(Array.fill[Byte](32)(0x42))
    k1.hashCode shouldBe k2.hashCode
  }

  "BandersnatchSignature" should "create from Array[Byte]" in {
    val bytes = Array.fill[Byte](96)(0x42)
    val sig = BandersnatchSignature(bytes)
    sig.bytes.toArray shouldBe bytes
  }

  "BandersnatchSignature.equals" should "compare by content" in {
    val s1 = BandersnatchSignature(Array.fill[Byte](96)(1))
    val s2 = BandersnatchSignature(Array.fill[Byte](96)(1))
    val s3 = BandersnatchSignature(Array.fill[Byte](96)(2))
    s1 shouldBe s2
    s1 should not be s3
  }

  "BandersnatchSignature.hashCode" should "be consistent with equals" in {
    val s1 = BandersnatchSignature(Array.fill[Byte](96)(0x42))
    val s2 = BandersnatchSignature(Array.fill[Byte](96)(0x42))
    s1.hashCode shouldBe s2.hashCode
  }

  "BlsPublicKey" should "create from Array[Byte]" in {
    val bytes = Array.fill[Byte](144)(0x42)
    val key = BlsPublicKey(bytes)
    key.bytes.toArray shouldBe bytes
  }

  "BlsPublicKey.equals" should "compare by content" in {
    val k1 = BlsPublicKey(Array.fill[Byte](144)(1))
    val k2 = BlsPublicKey(Array.fill[Byte](144)(1))
    val k3 = BlsPublicKey(Array.fill[Byte](144)(2))
    k1 shouldBe k2
    k1 should not be k3
  }

  "BlsPublicKey.hashCode" should "be consistent with equals" in {
    val k1 = BlsPublicKey(Array.fill[Byte](144)(0x42))
    val k2 = BlsPublicKey(Array.fill[Byte](144)(0x42))
    k1.hashCode shouldBe k2.hashCode
  }

  "BlsSignature" should "create from Array[Byte]" in {
    val bytes = Array.fill[Byte](48)(0x42)
    val sig = BlsSignature(bytes)
    sig.bytes.toArray shouldBe bytes
  }

  "BlsSignature.equals" should "compare by content" in {
    val s1 = BlsSignature(Array.fill[Byte](48)(1))
    val s2 = BlsSignature(Array.fill[Byte](48)(1))
    val s3 = BlsSignature(Array.fill[Byte](48)(2))
    s1 shouldBe s2
    s1 should not be s3
  }

  "BlsSignature.hashCode" should "be consistent with equals" in {
    val s1 = BlsSignature(Array.fill[Byte](48)(0x42))
    val s2 = BlsSignature(Array.fill[Byte](48)(0x42))
    s1.hashCode shouldBe s2.hashCode
  }
