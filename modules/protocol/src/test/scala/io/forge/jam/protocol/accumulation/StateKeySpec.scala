package io.forge.jam.protocol.accumulation

import io.forge.jam.core.JamBytes
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateKeySpec extends AnyFlatSpec with Matchers:

  "StateKey.isUnprovidedRequest" should "match decodePreimageInfoValue.isEmpty for well-formed values" in {
    StateKey.isUnprovidedRequest(JamBytes.empty) shouldBe true
    StateKey.isUnprovidedRequest(StateKey.encodePreimageInfoValue(List.empty)) shouldBe true       // count=0 -> []
    StateKey.isUnprovidedRequest(StateKey.encodePreimageInfoValue(List(5L))) shouldBe false         // count=1 -> provided
    StateKey.isUnprovidedRequest(StateKey.encodePreimageInfoValue(List(1L, 2L, 3L))) shouldBe false // count=3
  }

  it should "return false (not throw) on a malformed stored value (HPAS-10)" in {
    // count byte = 5 (> 3) -> decodePreimageInfoValue would throw RuntimeException
    noException should be thrownBy StateKey.isUnprovidedRequest(JamBytes(Array[Byte](5)))
    StateKey.isUnprovidedRequest(JamBytes(Array[Byte](5))) shouldBe false
    // count=2 but wrong length (5 bytes, expected 9) -> decode would throw
    StateKey.isUnprovidedRequest(JamBytes(Array[Byte](2, 0, 0, 0, 0))) shouldBe false
  }

  "StateKey preimage-info value" should "round-trip and keep a single-byte count for 0-3 entries" in {
    for ts <- Seq(List.empty[Long], List(7L), List(1L, 2L), List(10L, 20L, 30L)) do
      val enc = StateKey.encodePreimageInfoValue(ts)
      enc.toArray(0) shouldBe ts.size.toByte // compact(0..3) == single raw byte
      enc.length shouldBe (1 + ts.size * 4)
      StateKey.decodePreimageInfoValue(enc) shouldBe ts
  }

  it should "reject count>3 and a wrong body length" in {
    // count = 4 (> 3) -> rejected on the count cap (checked before length).
    a[RuntimeException] should be thrownBy StateKey.decodePreimageInfoValue(JamBytes(Array[Byte](4)))
    // count = 2 but only 5 bytes (expected 1 + 2*4 = 9) -> rejected on length.
    a[RuntimeException] should be thrownBy
      StateKey.decodePreimageInfoValue(JamBytes(Array[Byte](2, 0, 0, 0, 0)))
  }
