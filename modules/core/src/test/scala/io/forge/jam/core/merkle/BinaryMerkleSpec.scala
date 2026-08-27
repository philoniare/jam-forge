package io.forge.jam.core.merkle

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BinaryMerkleSpec extends AnyFunSuite with Matchers:

  private def h(parts: Array[Byte]*): Array[Byte] =
    BinaryMerkle.blake2b(parts.reduce(_ ++ _))

  private val nodeP = "node".getBytes("UTF-8")
  private val leafP = "leaf".getBytes("UTF-8")

  test("node: empty is the zero hash, singleton is the value itself") {
    BinaryMerkle.node(IndexedSeq.empty) shouldBe BinaryMerkle.ZeroHash
    val v = Array[Byte](1, 2, 3)
    BinaryMerkle.node(IndexedSeq(v)) shouldBe v
  }

  test("merklizeWB: singleton hashes the value; pair hashes node-prefixed children") {
    val a = Array[Byte](1)
    val b = Array[Byte](2)
    BinaryMerkle.merklizeWB(IndexedSeq(a)) shouldBe BinaryMerkle.blake2b(a)
    BinaryMerkle.merklizeWB(IndexedSeq(a, b)) shouldBe h(nodeP, a, b)
    BinaryMerkle.merklizeWB(IndexedSeq.empty) shouldBe BinaryMerkle.ZeroHash
  }

  test("merklizeWB: three leaves split ceil(3/2)=2 | 1") {
    val (a, b, c) = (Array[Byte](1), Array[Byte](2), Array[Byte](3))
    val left = h(nodeP, a, b)
    val expected = h(nodeP, left, c)
    BinaryMerkle.merklizeWB(IndexedSeq(a, b, c)) shouldBe expected
  }

  test("merklizeCD: pads leaf hashes to the next power of two with zero hashes") {
    val (a, b, c) = (Array[Byte](1), Array[Byte](2), Array[Byte](3))
    val la = h(leafP, a)
    val lb = h(leafP, b)
    val lc = h(leafP, c)
    val z = BinaryMerkle.ZeroHash
    val expected = h(nodeP, h(nodeP, la, lb), h(nodeP, lc, z))
    BinaryMerkle.merklizeCD(IndexedSeq(a, b, c)) shouldBe expected
    // Empty: C([]) = [zerohash], N of singleton = the value.
    BinaryMerkle.merklizeCD(IndexedSeq.empty) shouldBe z
  }

  test("trace reconstructs the root for every leaf of a padded CD tree") {
    val leaves = (0 until 11).map(i => Array[Byte](i.toByte, (i + 1).toByte))
    val padded = BinaryMerkle.constancyPreprocess(leaves)
    val root = BinaryMerkle.node(padded)

    for i <- padded.indices do
      val path = BinaryMerkle.trace(padded, i)
      // Reconstruct bottom-up: path is top-to-bottom, index bits (MSB first)
      // pick the side at each level.
      var acc = padded(i)
      val depth = path.length
      for k <- (depth - 1) to 0 by -1 do
        val bit = (i >> (depth - 1 - k)) & 1
        acc =
          if bit == 0 then h(nodeP, acc, path(k))
          else h(nodeP, path(k), acc)
      acc shouldBe root
  }

  test("justifySubPath + subtreePage reconstruct the CD root across pages") {
    // 130 leaves → padded to 256, page size 2^6 = 64, path length 8-6 = 2.
    val leaves = (0 until 130).map(i => Array[Byte]((i & 0xff).toByte, (i >> 8).toByte))
    val root = BinaryMerkle.merklizeCD(leaves)
    val pageCount = (leaves.length + 63) / 64

    BinaryMerkle.pathLength(leaves.length, 6) shouldBe 2

    for page <- 0 until pageCount do
      val path = BinaryMerkle.justifySubPath(leaves, 6, page)
      path.length shouldBe 2

      // The verifier knows the page's real leaves and pads to the subtree size.
      val pageLeaves = BinaryMerkle.subtreePage(leaves, 6, page).toBuffer
      while pageLeaves.size < 64 do pageLeaves += BinaryMerkle.ZeroHash
      var acc = BinaryMerkle.node(pageLeaves.toIndexedSeq)

      val depth = path.length
      for k <- (depth - 1) to 0 by -1 do
        val bit = (page >> (depth - 1 - k)) & 1
        acc =
          if bit == 0 then h(nodeP, acc, path(k))
          else h(nodeP, path(k), acc)
      acc shouldBe root
  }

  test("subtreePage: final partial page is unpadded") {
    val leaves = (0 until 70).map(i => Array[Byte](i.toByte))
    BinaryMerkle.subtreePage(leaves, 6, 0).length shouldBe 64
    BinaryMerkle.subtreePage(leaves, 6, 1).length shouldBe 6
  }
