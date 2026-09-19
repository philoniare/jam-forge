package io.forge.jam.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LittleEndianSpec extends AnyFunSuite with Matchers:

  private val values = List(0L, 1L, 0x7fL, 0x80L, 0xffL, 0x0102030405060708L, -1L)

  test("put/get round-trip all sizes, including high-bit values") {
    for
      size <- 1 to 8
      value <- values
    do
      val buf = new Array[Byte](8)
      LittleEndian.put(buf, 0, value, size)
      val mask = if size == 8 then -1L else (1L << (size * 8)) - 1
      LittleEndian.get(buf, 0, size) shouldBe (value & mask)
  }

  test("byte layout is little-endian") {
    val buf = new Array[Byte](4)
    LittleEndian.put(buf, 0, 0x01020304L, 4)
    buf shouldBe Array[Byte](0x04, 0x03, 0x02, 0x01)
  }

  test("put honours the offset and leaves surrounding bytes untouched") {
    val buf = Array.fill[Byte](8)(0x7f)
    LittleEndian.put(buf, 2, 0xaabbL, 2)
    buf shouldBe Array[Byte](0x7f, 0x7f, 0xbb.toByte, 0xaa.toByte, 0x7f, 0x7f, 0x7f, 0x7f)
    LittleEndian.get(buf, 2, 2) shouldBe 0xaabbL
  }

  test("get assembles unsigned — never sign-extends") {
    val buf = Array.fill[Byte](8)(0xff.toByte)
    LittleEndian.get(buf, 0, 1) shouldBe 0xffL
    LittleEndian.get(buf, 0, 2) shouldBe 0xffffL
    LittleEndian.get(buf, 0, 4) shouldBe 0xffffffffL
    LittleEndian.get(buf, 0, 8) shouldBe -1L // all 64 bits set
  }

  test("size 0 is a no-op for put and yields 0 for get") {
    val buf = Array.fill[Byte](4)(0x11)
    LittleEndian.put(buf, 0, 0x0abbccddL, 0)
    buf shouldBe Array.fill[Byte](4)(0x11)
    LittleEndian.get(buf, 0, 0) shouldBe 0L
  }

  test("arithmetic-shift and logical-shift hand-rolled variants match LittleEndian.put") {
    def legacySignedShift(buf: Array[Byte], offset: Int, value: Long, size: Int): Unit =
      var i = 0
      while i < size do
        buf(offset + i) = ((value >> (i * 8)) & 0xff).toByte
        i += 1

    def legacyLogicalShift(buf: Array[Byte], offset: Int, value: Long, size: Int): Unit =
      var i = 0
      while i < size do
        buf(offset + i) = ((value >>> (8 * i)) & 0xff).toByte
        i += 1

    val cases = values ++ List(Long.MinValue, Long.MaxValue, -2L, 0x8000000000000000L, 0xdeadbeefcafebabeL)
    for
      size <- 0 to 8
      value <- cases
    do
      val a, b, c = new Array[Byte](8)
      legacySignedShift(a, 0, value, size)
      legacyLogicalShift(b, 0, value, size)
      LittleEndian.put(c, 0, value, size)
      withClue(s"size=$size value=0x${value.toHexString}: ") {
        a shouldBe c
        b shouldBe c
      }
  }

  test("get is the exact inverse of put for every offset in an 8-byte window") {
    for
      size <- 1 to 8
      offset <- 0 to (8 - size)
      value <- values
    do
      val buf = new Array[Byte](8)
      LittleEndian.put(buf, offset, value, size)
      val mask = if size == 8 then -1L else (1L << (size * 8)) - 1
      LittleEndian.get(buf, offset, size) shouldBe (value & mask)
  }
