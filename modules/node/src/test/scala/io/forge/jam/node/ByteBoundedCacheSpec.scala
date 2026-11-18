package io.forge.jam.node

import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.primitives.Hash
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ByteBoundedCacheSpec extends AnyFunSuite with Matchers:

  private def bytes(n: Int, fill: Int = 0) = Array.fill[Byte](n)(fill.toByte)
  private def root(i: Int) = Hash(Array.fill[Byte](32)(i.toByte))

  test("the oldest entries leave once the held bytes would pass the budget") {
    val cache = new ByteBoundedCache[Int, Array[Byte]](maxEntries = 100, maxBytes = 10, _.length.toLong)
    cache.put(1, bytes(4)); cache.put(2, bytes(4))
    cache.bytes shouldBe 8
    cache.put(3, bytes(4))
    cache.get(1) shouldBe None
    cache.get(2).map(_.length) shouldBe Some(4)
    cache.bytes shouldBe 8
  }

  test("the entry bound still holds, and the newest entry is kept even when it alone is over the budget") {
    val cache = new ByteBoundedCache[Int, Array[Byte]](maxEntries = 2, maxBytes = 10, _.length.toLong)
    cache.put(1, bytes(1)); cache.put(2, bytes(1)); cache.put(3, bytes(1))
    cache.get(1) shouldBe None
    cache.put(4, bytes(50))
    cache.get(4).map(_.length) shouldBe Some(50)
    cache.size shouldBe 1
  }

  test("replacing a key counts its new size, not both") {
    val cache = new ByteBoundedCache[Int, Array[Byte]](maxEntries = 10, maxBytes = 100, _.length.toLong)
    cache.put(1, bytes(30)); cache.put(1, bytes(10))
    cache.bytes shouldBe 10
  }

  test("the segment pool gives up its oldest segment sets past its byte budget") {
    val segment = Csegmentsize.toInt
    val pool = new SegmentPool(maxBytes = 3L * segment)
    pool.put(root(1), IndexedSeq(bytes(segment, 1), bytes(segment, 2)))
    pool.put(root(2), IndexedSeq(bytes(segment, 3), bytes(segment, 4)))
    pool.get(root(1), Seq(0)) shouldBe None
    pool.get(root(2), Seq(1)).map(_.head.head) shouldBe Some(4.toByte)
  }
