package io.forge.jam.node

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.primitives.{Ed25519Signature, Hash, ValidatorIndex}
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.crypto.ErasureCoding
import io.forge.jam.protocol.refine.{AvailabilitySpecifier, DaShards}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Wire encodings and proof checks of the JAMNP-S DA and guarantee protocols. */
class JamnpWireSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY

  private def hash(b: Int): Hash = Hash(Array.fill[Byte](32)(b.toByte))

  test("JAMNP-S: shard index is (c·R + v) mod V, a permutation of validators per core") {
    val r = DaShards.recoveryThreshold(config)
    r shouldBe 3
    for core <- 0 until config.coresCount do
      val indices = (0 until config.validatorCount).map(v => DaShards.shardIndexFor(core, v, config))
      indices.sorted shouldBe (0 until config.validatorCount)
      indices.head shouldBe (core * r) % config.validatorCount
  }

  test("JAMNP-S: segment shard is 4104 / R bytes") {
    DaShards.segmentShardSize(config) shouldBe 1368
    DaShards.segmentShardSize(config.copy(ecPieceSize = 684)) shouldBe 12
  }

  test("JAMNP-S: CE 139/140 request carries several roots with u16 indices") {
    val groups = List((hash(1), 5, List(0, 1, 700)), (hash(2), 0, Nil))
    val bytes = ShardService.encodeSegmentShardRequest(groups)
    bytes.length shouldBe (34 + 1 + 6) + (34 + 1)
    ShardService.decodeSegmentShardRequest(bytes) shouldBe Some(groups)
    ShardService.decodeSegmentShardRequest(bytes.dropRight(1)) shouldBe None
    ShardService.decodeSegmentShardRequest(Array.emptyByteArray) shouldBe None
  }

  test("JAMNP-S: justification entries are tagged 0 (hash), 1 (64-byte leaf), 2 (segment shard)") {
    val shardSize = DaShards.segmentShardSize(config)
    val entries = List(Array.fill[Byte](32)(1), Array.fill[Byte](64)(2), Array.fill[Byte](shardSize)(3))
    val bytes = ShardService.encodeJustification(entries, shardSize)
    bytes.length shouldBe 33 + 65 + 1 + shardSize
    Seq(bytes(0), bytes(33), bytes(98)) shouldBe Seq[Byte](0, 1, 2)
    ShardService.decodeJustification(bytes, shardSize).get.map(_.toSeq) shouldBe entries.map(_.toSeq)
    ShardService.decodeJustification(Array[Byte](3), shardSize) shouldBe None
    ShardService.decodeJustification(bytes.dropRight(1), shardSize) shouldBe None
  }

  test("JAMNP-S: CE 138 bundle shards and CE 140 segment shards verify against the erasure-root") {
    assume(ErasureCoding.isAvailable)
    val bundle = (0 until 5000).map(i => (i * 31 + 7).toByte).toArray
    val segments = (0 until 3).map(s => Array.tabulate(Csegmentsize.toInt)(i => (i + s).toByte))
    val root = AvailabilitySpecifier.build(hash(0), bundle, segments, config).toOption.get.erasureRoot
    val all = DaShards.buildAll(bundle, segments, config).toOption.get
    val v = config.validatorCount
    all.foreach { s =>
      withClue(s"shard ${s.shardIndex}: ") {
        DaShards.verifyAuditShard(root, v, s.shardIndex, s.bundleShard, DaShards.auditJustification(s)) shouldBe true
        DaShards.verifyAuditShard(
          root,
          v,
          (s.shardIndex + 1) % v,
          s.bundleShard,
          DaShards.auditJustification(s)
        ) shouldBe false
        // Exported segments plus their paged-proof segments.
        val count = s.segmentShards.size
        count shouldBe segments.size + 1
        (0 until count).foreach { i =>
          val j = DaShards.segmentJustification(s, i)
          // Through the wire encoding and back.
          val shardSize = DaShards.segmentShardSize(config)
          val wire = ShardService.decodeJustification(ShardService.encodeJustification(j, shardSize), shardSize).get
          ShardService.verifySegmentShard(root, v, s.shardIndex, i, count, s.segmentShards(i), wire) shouldBe true
          val other = s.segmentShards((i + 1) % count)
          ShardService.verifySegmentShard(root, v, s.shardIndex, i, count, other, wire) shouldBe false
        }
      }
    }
  }

  test("JAMNP-S: CE 134 header is core ++ shard count ++ len++[package hash ++ segments-root]") {
    val mappings = Seq(hash(1) -> hash(2), hash(3) -> hash(4))
    val bytes = GuarantorService.encodeSharingHeader(1, 6, mappings)
    bytes.length shouldBe 2 + 2 + 1 + 128
    GuarantorService.decodeSharingHeader(bytes) shouldBe Right((1, 6, mappings.toMap))
    GuarantorService.decodeSharingHeader(GuarantorService.encodeSharingHeader(0, 1023, Nil)) shouldBe
      Right((0, 1023, Map.empty))
    GuarantorService.decodeSharingHeader(bytes.dropRight(1)).isLeft shouldBe true
  }

  test("JAMNP-S: CE 141 assurance is anchor ++ bitfield ++ signature, without a validator index") {
    val a = AssuranceExtrinsic(
      anchor = hash(9),
      bitfield = JamBytes(Array[Byte](0x01)),
      validatorIndex = ValidatorIndex(4),
      signature = Ed25519Signature(Array.fill[Byte](64)(7))
    )
    val bytes = DistributionService.encodeAssurance(a)
    bytes.length shouldBe 32 + 1 + 64
    val (anchor, bitfield, sig) = DistributionService.decodeAssurance(bytes, config.coresCount).toOption.get
    anchor shouldBe a.anchor
    bitfield shouldBe a.bitfield
    sig.bytes.toSeq shouldBe a.signature.bytes.toSeq
    DistributionService.decodeAssurance(bytes :+ 0.toByte, config.coresCount).isLeft shouldBe true
  }
