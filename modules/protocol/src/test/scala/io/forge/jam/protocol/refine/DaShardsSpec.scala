package io.forge.jam.protocol.refine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.primitives.Hash
import io.forge.jam.crypto.ErasureCoding

class DaShardsSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private def available = ErasureCoding.isAvailable

  private val bundle: Array[Byte] =
    (0 until 5000).map(i => (i * 31 + 7).toByte).toArray
  private val segments: IndexedSeq[Array[Byte]] =
    (0 until 3).map(s => Array.tabulate(Csegmentsize.toInt)(i => (i + s).toByte))

  private def erasureRootOf(
      b: Array[Byte],
      segs: IndexedSeq[Array[Byte]]
  ): Hash =
    AvailabilitySpecifier
      .build(Hash(new Array[Byte](32)), b, segs, config)
      .toOption
      .get
      .erasureRoot

  test("shards verify against the availability specifier's erasure-root") {
    assume(available)
    val root = erasureRootOf(bundle, segments)
    val all = DaShards.buildAll(bundle, segments, config).toOption.get
    all.size shouldBe config.validatorCount
    all.foreach { s =>
      withClue(s"validator ${s.validatorIndex}") {
        DaShards.verify(root, config.validatorCount, s) shouldBe true
      }
    }
  }

  test("verification fails for tampered shards, wrong index, wrong root") {
    assume(available)
    val root = erasureRootOf(bundle, segments)
    val all = DaShards.buildAll(bundle, segments, config).toOption.get
    val s = all(2)

    val tamperedBundle = s.copy(bundleShard = s.bundleShard.updated(0, (s.bundleShard(0) ^ 1).toByte))
    DaShards.verify(root, config.validatorCount, tamperedBundle) shouldBe false

    val tamperedSegment = s.copy(segmentShards =
      s.segmentShards.updated(0, s.segmentShards(0).updated(0, (s.segmentShards(0)(0) ^ 1).toByte))
    )
    DaShards.verify(root, config.validatorCount, tamperedSegment) shouldBe false

    val wrongIndex = s.copy(validatorIndex = 3)
    DaShards.verify(root, config.validatorCount, wrongIndex) shouldBe false

    val otherRoot = erasureRootOf(bundle :+ 1.toByte, segments)
    DaShards.verify(otherRoot, config.validatorCount, s) shouldBe false
  }

  test("no exported segments: empty columns still verify") {
    assume(available)
    val root = erasureRootOf(bundle, IndexedSeq.empty)
    val all = DaShards.buildAll(bundle, IndexedSeq.empty, config).toOption.get
    all.foreach { s =>
      s.segmentShards shouldBe empty
      DaShards.verify(root, config.validatorCount, s) shouldBe true
    }
  }

  test("bundle reconstructs from any originalCount shards") {
    assume(available)
    val all = DaShards.buildAll(bundle, segments, config).toOption.get
    val originalCount = config.ecPieceSize / 2

    // From the first `originalCount` shards.
    val first = all.take(originalCount).map(s => (s.validatorIndex, s.bundleShard))
    DaShards.reconstructBundle(bundle.length, first, config).toOption.get shouldBe bundle

    // From the last `originalCount` shards (pure recovery shards).
    val last = all.takeRight(originalCount).map(s => (s.validatorIndex, s.bundleShard))
    DaShards.reconstructBundle(bundle.length, last, config).toOption.get shouldBe bundle

    // Too few shards fails.
    DaShards
      .reconstructBundle(bundle.length, first.take(originalCount - 1), config)
      .isLeft shouldBe true
  }

  test("encode/decode round-trip") {
    assume(available)
    val all = DaShards.buildAll(bundle, segments, config).toOption.get
    all.foreach { s =>
      val decoded = DaShards.decode(s.encode).toOption.get
      decoded.validatorIndex shouldBe s.validatorIndex
      decoded.bundleShard shouldBe s.bundleShard
      decoded.segmentShards.size shouldBe s.segmentShards.size
      decoded.segmentShards.zip(s.segmentShards).foreach((a, b) => a shouldBe b)
      decoded.justification.size shouldBe s.justification.size
      decoded.justification.zip(s.justification).foreach((a, b) => a shouldBe b)
    }
    DaShards.decode(Array[Byte](1, 2, 3)).isLeft shouldBe true
  }
