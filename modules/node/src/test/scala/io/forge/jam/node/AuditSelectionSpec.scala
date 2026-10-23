package io.forge.jam.node

import io.forge.jam.core.Shuffle
import io.forge.jam.core.primitives.Hash
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import AuditSelection.*

class AuditSelectionSpec extends AnyFunSuite with Matchers:

  private def h(b: Int): Hash = Hash(Array.fill(32)(b.toByte))

  test("tranche n = ⌊(T − P·H_t) / 8⌋") {
    trancheAt(nowMillis = 1000L, originMillis = 1000L) shouldBe 0
    trancheAt(8999L, 1000L) shouldBe 0
    trancheAt(9000L, 1000L) shouldBe 1
    trancheAt(25000L, 1000L) shouldBe 3
    trancheAt(500L, 1000L) shouldBe 0
  }

  test("a_0 is the non-empty cores among the first ten of F(q, Y(s_0))") {
    val seed = Array.tabulate[Byte](32)(i => (i * 7 + 3).toByte)
    val q = IndexedSeq.tabulate(20)(c => Option.when(c % 3 != 0)(c))
    val order = Shuffle.jamComputeShuffle(20, Hash(seed))
    initialCores(q, seed) shouldBe order.take(10).filter(_ % 3 != 0)
    // With two cores (tiny) every available report is drawn.
    initialCores(IndexedSeq(Some(1), None), seed) shouldBe Seq(0)
  }

  test("a_n escalates when V/(65536·2) · E₂⁻¹(Y[0..2]) < no-shows") {
    def out(d: Int) = Array[Byte]((d & 0xff).toByte, ((d >> 8) & 0xff).toByte) ++ Array.fill[Byte](30)(0)
    // V = 1023, one no-show: d < 131072 / 1023 ≈ 128.1, so ≈ 2 of 1023 validators escalate (C_bias = 2).
    escalates(out(128), 1023, 1) shouldBe true
    escalates(out(129), 1023, 1) shouldBe false
    escalates(out(0), 1023, 0) shouldBe false
    escalates(out(256), 1023, 2) shouldBe true
    // V = 6: each validator joins with probability 2/6 per no-show.
    escalates(out(21844), 6, 1) shouldBe true
    escalates(out(21846), 6, 1) shouldBe false
  }

  test("U(w) needs every announced auditor of some tranche to judge positively and no negative judgment") {
    val announced = Map(0 -> Set(1, 2), 1 -> Set(3))
    audited(announced, positive = Set(1), negative = Set.empty, validatorCount = 6, currentTranche = 1) shouldBe false
    audited(announced, Set(1, 2), Set.empty, 6, 1) shouldBe true
    audited(announced, Set(3), Set.empty, 6, 1) shouldBe true
    audited(announced, Set(1, 2), Set(4), 6, 1) shouldBe false
    // More than two thirds positive overrides a negative judgment.
    audited(announced, Set(0, 1, 2, 3, 5), Set(4), 6, 1) shouldBe true
    // A later tranche is not consulted before it starts.
    audited(announced, Set(3), Set.empty, 6, 0) shouldBe false
  }

  test("an unannounced tranche 0 counts only after it ends; an empty later tranche never counts") {
    audited(Map.empty, Set.empty, Set.empty, 6, 0) shouldBe false
    audited(Map.empty, Set.empty, Set.empty, 6, 1) shouldBe true
    audited(Map(0 -> Set(1)), Set.empty, Set.empty, 6, 3) shouldBe false
  }

  test("a third of the validators judging a report invalid disregards its block") {
    disregards(Set(1), 6) shouldBe false
    disregards(Set(1, 2), 6) shouldBe true
    disregards(Set.tabulate(341)(identity), 1023) shouldBe true
    disregards(Set.tabulate(340)(identity), 1023) shouldBe false
  }

  test("the announcement signs $jam_announce ++ n ++ x_n ++ H(header), x_n in announced order") {
    val items = Seq(300 -> h(2), 1 -> h(1))
    val msg = announcementMessage(2, items, h(9))
    msg.take(12) shouldBe "jam_announce".getBytes("UTF-8")
    msg(12) shouldBe 2
    // E₂(300) = 2c 01, listed first, stays first (polkajam signs the wire order).
    msg.slice(13, 15) shouldBe Array[Byte](0x2c, 1)
    msg.slice(47, 49) shouldBe Array[Byte](1, 0)
    msg.takeRight(32) shouldBe h(9).bytes.toArray
    msg.length shouldBe 12 + 1 + 2 * 34 + 32
  }

  test("jam-np CE 144 messages round-trip") {
    val a = Announcement(Seq(0 -> h(1), 1 -> h(2)), Array.fill(64)(7.toByte))
    val head = encodeHead(h(5), 3, a)
    val Right((header, tranche, back)) = decodeHead(head): @unchecked
    header shouldBe h(5)
    tranche shouldBe 3
    back.items shouldBe a.items
    back.signature.toSeq shouldBe a.signature.toSeq
    decodeHead(head.dropRight(1)).isLeft shouldBe true

    val sig = Array.fill(96)(3.toByte)
    val evidence = encodeLaterEvidence(Seq(sig -> Seq(NoShow(4, a)), sig -> Nil))
    val Right(per) = decodeLaterEvidence(evidence, 2): @unchecked
    per.map(_._2.map(_.validator)) shouldBe Seq(Seq(4), Nil)
    per.head._2.head.announcement.items shouldBe a.items
    decodeLaterEvidence(evidence, 1).isLeft shouldBe true
  }

  test("historical lookup availability I(l, t)") {
    import StateHistoricalLookup.availableAt
    availableAt(Nil, 5) shouldBe false
    availableAt(List(5L), 4) shouldBe false
    availableAt(List(5L), 5) shouldBe true
    availableAt(List(5L, 9L), 8) shouldBe true
    availableAt(List(5L, 9L), 9) shouldBe false
    availableAt(List(5L, 9L, 12L), 10) shouldBe false
    availableAt(List(5L, 9L, 12L), 12) shouldBe true
    availableAt(List(5L, 9L, 12L), 6) shouldBe true
  }
