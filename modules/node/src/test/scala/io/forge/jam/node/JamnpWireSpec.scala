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

  private val polkajamGuarantee: Array[Byte] =
    ("93169700" + "02" +
      "0400" + "a56194a05ea8cca86665a40f2932d70e06dfb281d60e61737371df5c81fbaa05" +
      "d30c6c4bc971c8d590b1ff7cd443d67355d425e81f37736632b3ef6ecd292a06" +
      "0500" + "70dbfb34f2ae2af53366a6372b5a4bf45f4c53566a7dc138d918559dd0f1baf3" +
      "83645191303b7bbf443a88b1612dcbf0bd32d871ee3b30ea3bfd4da7072cd606")
      .grouped(2)
      .map(Integer.parseInt(_, 16).toByte)
      .toArray

  test("JAMNP-S: CE 145 guarantee is slot ++ len ++ [validator index ++ signature], as polkajam sends it") {
    polkajamGuarantee.length shouldBe 137
    val (slot, signed) = JudgmentWire.decodeGuarantee(polkajamGuarantee).get
    slot shouldBe 9901715L
    signed.map(_._1) shouldBe List(4, 5)
    signed.map(_._2.toSeq) shouldBe List(polkajamGuarantee.slice(7, 71).toSeq, polkajamGuarantee.slice(73, 137).toSeq)
    JudgmentWire.encodeGuarantee(slot, signed).map(_.toSeq) shouldBe Some(polkajamGuarantee.toSeq)
  }

  test("JAMNP-S: a CE 145 guarantee carries two or three signatures and nothing else") {
    val sig = Array.fill[Byte](64)(7)
    def signers(n: Int) = (0 until n).toList.map(_ -> sig)
    def wire(count: Int, entries: Int) =
      Array[Byte](5, 0, 0, 0, count.toByte) ++ (0 until entries).flatMap(i => Array[Byte](i.toByte, 0) ++ sig)
    JudgmentWire.encodeGuarantee(5L, signers(1)) shouldBe None
    JudgmentWire.encodeGuarantee(5L, signers(4)) shouldBe None
    val three = JudgmentWire.encodeGuarantee(5L, signers(3)).get
    three.toSeq shouldBe wire(3, 3).toSeq
    JudgmentWire.decodeGuarantee(three).map((slot, signed) => (slot, signed.map(_._1))) shouldBe Some((5L, List(0, 1, 2)))
    JudgmentWire.decodeGuarantee(three :+ 0.toByte) shouldBe None
    JudgmentWire.decodeGuarantee(three.dropRight(1)) shouldBe None
    JudgmentWire.decodeGuarantee(wire(2, 3)) shouldBe None
    JudgmentWire.decodeGuarantee(wire(1, 1)) shouldBe None
    JudgmentWire.decodeGuarantee(wire(4, 4)) shouldBe None
    JudgmentWire.decodeGuarantee(Array.emptyByteArray) shouldBe None
    JudgmentWire.decodeGuarantee(JudgmentWire.encodeGuarantee(0xfffffff0L, signers(2)).get).map(_._1) shouldBe Some(0xfffffff0L)
  }

  test("JAMNP-S: on a CE 145 stream a guarantee follows a negative judgment and nothing else") {
    import JudgmentWire.Part.*
    def judgment(valid: Boolean) = new Array[Byte](JudgmentWire.JudgmentLength).updated(6, (if valid then 1 else 0).toByte)
    val guarantee = polkajamGuarantee
    def parts(messages: Array[Byte]*) =
      val stream = new JudgmentWire.Stream
      messages.toList.map(stream.next)
    parts(judgment(valid = false), guarantee) shouldBe List(Judgment, Guarantee)
    parts(judgment(valid = false)) shouldBe List(Judgment)
    parts(judgment(valid = true)) shouldBe List(Judgment)
    parts(judgment(valid = true), guarantee) shouldBe List(Judgment, Malformed)
    parts(judgment(valid = false), guarantee, guarantee) shouldBe List(Judgment, Guarantee, Malformed)
    parts(judgment(valid = false), guarantee.dropRight(1)) shouldBe List(Judgment, Malformed)
    parts(guarantee) shouldBe List(Malformed)
    parts(judgment(valid = false), judgment(valid = true), judgment(valid = false), guarantee) shouldBe
      List(Judgment, Judgment, Judgment, Guarantee)
  }

  private def judgmentOf(v: Int, valid: Boolean): Array[Byte] =
    Array[Byte](9, 0, 0, 0, v.toByte, 0, (if valid then 1 else 0).toByte) ++ Array.fill[Byte](32)(0x11) ++ Array.fill[Byte](64)(v.toByte)

  test("JAMNP-S: CE 145 inline form puts the guarantee of a negative judgment after its validity byte") {
    val negative = judgmentOf(3, valid = false)
    val positive = judgmentOf(2, valid = true)
    val inlined = JudgmentWire.encodeInline(negative, Some(polkajamGuarantee)).get
    inlined.length shouldBe 103 + 137
    inlined.toSeq shouldBe (negative.take(7) ++ polkajamGuarantee ++ negative.drop(7)).toSeq
    JudgmentWire.encodeInline(positive, None).map(_.toSeq) shouldBe Some(positive.toSeq)
    JudgmentWire.encodeInline(positive, Some(polkajamGuarantee)).map(_.toSeq) shouldBe Some(positive.toSeq)
    JudgmentWire.encodeInline(negative, None) shouldBe None
    JudgmentWire.encodeInline(negative, Some(polkajamGuarantee.dropRight(1))) shouldBe None
  }

  test("JAMNP-S: a CE 145 inline message is any number of judgments, each negative one with its guarantee") {
    val negative = judgmentOf(3, valid = false)
    val positive = judgmentOf(2, valid = true)
    val other = judgmentOf(5, valid = true)
    val inlined = JudgmentWire.encodeInline(negative, Some(polkajamGuarantee)).get
    def decoded(message: Array[Byte]) =
      JudgmentWire.decodeBatch(message).map(_.map((judgment, guarantee) => (judgment.toSeq, guarantee.map(_.toSeq))))
    decoded(positive ++ inlined ++ other) shouldBe Some(
      List((positive.toSeq, None), (negative.toSeq, Some(polkajamGuarantee.toSeq)), (other.toSeq, None))
    )
    decoded(inlined) shouldBe Some(List((negative.toSeq, Some(polkajamGuarantee.toSeq))))
    decoded(positive) shouldBe Some(List((positive.toSeq, None)))
    decoded(Array.emptyByteArray) shouldBe None
    decoded(positive ++ inlined.dropRight(1)) shouldBe None
    decoded(inlined :+ 0.toByte) shouldBe None
    decoded(negative) shouldBe None
    decoded(inlined.updated(7 + 4, 1.toByte)) shouldBe None
    decoded(positive.updated(6, 2.toByte)) shouldBe None
  }

  test("JAMNP-S: on a CE 145 stream both forms are told apart") {
    import JudgmentWire.Part.*
    val negative = judgmentOf(3, valid = false)
    val positive = judgmentOf(2, valid = true)
    val inlined = JudgmentWire.encodeInline(negative, Some(polkajamGuarantee)).get
    def parts(messages: Array[Byte]*) =
      val stream = new JudgmentWire.Stream
      messages.toList.map(stream.next)
    parts(inlined) shouldBe List(Batch)
    parts(positive ++ inlined ++ positive) shouldBe List(Batch)
    parts(positive ++ positive) shouldBe List(Batch)
    parts(positive) shouldBe List(Judgment)
    parts(negative, polkajamGuarantee) shouldBe List(Judgment, Guarantee)
    parts(negative, inlined) shouldBe List(Judgment, Batch)
    parts(inlined, polkajamGuarantee) shouldBe List(Batch, Malformed)
    parts(inlined.dropRight(1)) shouldBe List(Malformed)
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
