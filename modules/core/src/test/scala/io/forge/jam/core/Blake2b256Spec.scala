package io.forge.jam.core

import org.bouncycastle.crypto.digests.Blake2bDigest
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Blake2b256Spec extends AnyFunSuite with Matchers:

  private def hex(bytes: Array[Byte]): String = bytes.map(b => f"${b & 0xff}%02x").mkString

  private def bouncyCastle(data: Array[Byte], offset: Int, length: Int): Array[Byte] =
    val digest = new Blake2bDigest(256)
    digest.update(data, offset, length)
    val out = new Array[Byte](32)
    digest.doFinal(out, 0)
    out

  test("GP: H is BLAKE2b-256 of the empty string") {
    hex(Blake2b256.hash(Array.emptyByteArray, 0, 0)) shouldBe
      "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
  }

  test("BLAKE2b-256 of \"abc\" (RFC 7693 input, 32-byte digest)") {
    hex(Blake2b256.hash("abc".getBytes("US-ASCII"), 0, 3)) shouldBe
      "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"
  }

  test("BLAKE2b-256 of the 64-byte zero trie preimage") {
    hex(Hashing.blake2b256(new Array[Byte](64)).bytes) shouldBe
      hex(bouncyCastle(new Array[Byte](64), 0, 64))
  }

  test("matches Bouncy Castle for every length 0..1100 at random offsets, across block boundaries") {
    val rnd = new scala.util.Random(7693)
    for
      len <- 0 to 1100
      _ <- 0 until 3
    do
      val data = new Array[Byte](len + 9)
      rnd.nextBytes(data)
      val offset = rnd.nextInt(10)
      withClue(s"len=$len offset=$offset: ") {
        hex(Blake2b256.hash(data, offset, len)) shouldBe hex(bouncyCastle(data, offset, len))
      }
  }

  test("large input (1 MiB) matches Bouncy Castle") {
    val data = Array.tabulate[Byte](1 << 20)(i => (i * 131 + 7).toByte)
    hex(Blake2b256.hash(data, 0, data.length)) shouldBe hex(bouncyCastle(data, 0, data.length))
  }

  test("writes the digest at the given output offset only") {
    val out = Array.fill[Byte](40)(0x55)
    Blake2b256.hash("abc".getBytes("US-ASCII"), 0, 3, out, 4)
    out.take(4).forall(_ == 0x55) shouldBe true
    out.drop(36).forall(_ == 0x55) shouldBe true
    hex(out.slice(4, 36)) shouldBe "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"
  }

  test("rejects out-of-range slices") {
    an[IndexOutOfBoundsException] should be thrownBy Blake2b256.hash(new Array[Byte](8), 4, 5)
    an[IndexOutOfBoundsException] should be thrownBy Blake2b256.hash(new Array[Byte](8), -1, 1)
    an[IndexOutOfBoundsException] should be thrownBy Blake2b256.hash(new Array[Byte](8), 0, -1)
  }
