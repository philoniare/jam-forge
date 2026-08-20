package io.forge.jam.core.scodec

import scodec.bits.ByteVector
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FullJamStateCodecsSpec extends AnyFlatSpec with Matchers:

  "FullJamStateCodecs state decoders" should "round-trip a well-formed value" in {
    val outputs = List(
      (1L, ByteVector.fill(32L)(0xaa.toByte)),
      (2L, ByteVector.fill(32L)(0xbb.toByte))
    )
    val encoded = FullJamStateCodecs.encodeLastAccumulationOutputs(outputs).toArray
    FullJamStateCodecs.decodeLastAccumulationOutputs(encoded) shouldBe outputs
  }

  it should "reject trailing bytes after a valid value (SER-13)" in {
    val encoded = FullJamStateCodecs.encodeLastAccumulationOutputs(List.empty).toArray
    // Sanity: the clean encoding decodes fine.
    FullJamStateCodecs.decodeLastAccumulationOutputs(encoded) shouldBe List.empty
    // A garbage tail must now be rejected as corruption, not silently dropped.
    val withTrailing = encoded :+ 0xab.toByte
    a[CodecDecodingException] should be thrownBy
      FullJamStateCodecs.decodeLastAccumulationOutputs(withTrailing)
  }
