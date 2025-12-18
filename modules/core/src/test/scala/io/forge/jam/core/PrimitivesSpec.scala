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
