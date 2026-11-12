package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.UInt
import primitives.*

class PrimitivesSpec extends AnyFlatSpec with Matchers:

  "Hash" should "reject wrong size" in {
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

  private final case class FixedCase[T](companion: FixedBytes[T], bytesOf: T => Array[Byte])

  private val fixedCases: List[FixedCase[?]] = List(
    FixedCase(Ed25519PublicKey, (k: Ed25519PublicKey) => k.bytes),
    FixedCase(Ed25519Signature, (s: Ed25519Signature) => s.bytes),
    FixedCase(BandersnatchPublicKey, (k: BandersnatchPublicKey) => k.bytes),
    FixedCase(BandersnatchSignature, (s: BandersnatchSignature) => s.bytes),
    FixedCase(BlsPublicKey, (k: BlsPublicKey) => k.bytes),
    FixedCase(BlsSignature, (s: BlsSignature) => s.bytes)
  )

  "Fixed-size key and signature types" should "create from bytes, create zero, and compare/hash by content" in {
    def check[T](c: FixedCase[T]): Unit =
      val n = c.companion.Size
      val bytes = Array.fill[Byte](n)(0x42)
      c.bytesOf(c.companion(bytes)) shouldBe bytes
      c.bytesOf(c.companion.zero).forall(_ == 0) shouldBe true
      val a = c.companion(Array.fill[Byte](n)(1))
      val b = c.companion(Array.fill[Byte](n)(1))
      val other = c.companion(Array.fill[Byte](n)(2))
      a shouldBe b
      a should not be other
      a.hashCode shouldBe b.hashCode
    for c <- fixedCases do withClue(s"size=${c.companion.Size}: ")(check(c))
  }

  "Timeslot" should "support arithmetic and keep 32-bit unsigned semantics over its Long" in {
    (Timeslot(100) + 5).value shouldBe UInt(105)
    (Timeslot(100) - 10).value shouldBe UInt(90)
    Timeslot(-1).toLong shouldBe 0xffffffffL
    Timeslot(-1).value shouldBe UInt(-1)
    (Timeslot(-1) + 1) shouldBe Timeslot.Zero
    (Timeslot.Zero - 1).toLong shouldBe 0xffffffffL
  }

  "Gas" should "support arithmetic and compare unsigned, as ULong did" in {
    (Gas(1000L) - Gas(400L)).toLong shouldBe 600L
    (Gas(1000L) + Gas(400L)).toLong shouldBe 1400L
    (Gas(1000L) >= Gas(400L)) shouldBe true
    (Gas.Max >= Gas(1L)) shouldBe true
    (Gas(1L) < Gas.Max) shouldBe true
    (Gas(5L) < Gas(4L)) shouldBe false
  }

  "ValidatorIndex" should "keep 16-bit unsigned semantics over its Int" in {
    ValidatorIndex(-1).toInt shouldBe 0xffff
    ValidatorIndex(3).value.toInt shouldBe 3
  }
