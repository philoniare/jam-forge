package io.forge.jam.node

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.{CoreIndex, Ed25519Signature, Gas, Hash, Timeslot, ValidatorIndex}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.dispute.GuaranteeSignature
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.work.PackageSpec
import io.forge.jam.core.types.workpackage.WorkReport
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

/** A restart must not lose what the validator roles had learned: dispute evidence and the index of exported
  * segments are read back from the role store by the objects that replace the ones the node had before.
  */
class RoleStateRestartSpec extends AnyFunSuite with Matchers:

  private def h(tag: String): Hash = Hashing.blake2b256(tag.getBytes("UTF-8"))
  private def sig(b: Int): Array[Byte] = Array.fill(64)(b.toByte)

  private def guarantee(tag: String, core: Int = 0, exports: Int = 0, length: Int = 100, signers: Seq[Int] = Seq(1, 4)) =
    val report = WorkReport(
      packageSpec = PackageSpec(h(tag), UInt(length), h(s"erasure-$tag"), UShort(0), h(s"exports-$tag"), UShort(exports)),
      context = Context(Hash.zero, Timeslot(UInt(0)), Hash.zero, Hash.zero, Hash.zero, Timeslot(UInt(0)), Hash.zero, List.empty),
      coreIndex = CoreIndex(core),
      authorizerHash = Hash.zero,
      authGasUsed = Gas(0),
      authOutput = JamBytes.empty,
      segmentRootLookup = List.empty,
      results = List.empty
    )
    GuaranteeExtrinsic(
      report,
      Timeslot(UInt(0)),
      signers.toList.map(v => GuaranteeSignature(ValidatorIndex(v), Ed25519Signature(sig(v))))
    )

  private def reportHash(g: GuaranteeExtrinsic): Hash = Hashing.blake2b256(g.report.encode.toArray)

  /** Dispute evidence in a comparable form: byte arrays as lists, in a fixed order. */
  private def evidence(pools: ExtrinsicPools) =
    pools.disputed
      .map(d =>
        (
          d.reportHash,
          d.epoch,
          d.judgments.map((v, valid, s) => (v, valid, s.toList)).sortBy(_._1),
          d.credentials.map((parent, slot, creds) => (parent, slot, creds.map((v, s) => v -> s.toList)))
        )
      )
      .sortBy(d => (d._1.toHex, d._2))

  test("judgments and guarantor credentials are still there after a restart") {
    val store = new InMemoryRoleStore
    val before = new ExtrinsicPools(store = store)
    val bad = guarantee("bad")
    val fine = guarantee("fine", core = 1)
    before.recordCredentials(h("parent"), 90L, List(bad, fine))
    before.addJudgment(reportHash(bad), 7L, 2, true, sig(0x22))
    before.addJudgment(reportHash(bad), 7L, 5, false, sig(0x55))
    before.addJudgment(reportHash(bad), 8L, 3, false, sig(0x33))
    before.addJudgment(reportHash(fine), 7L, 2, true, sig(0x23))
    evidence(before) should have size 2

    val after = new ExtrinsicPools(store = store)
    after.judgmentCount shouldBe 4
    after.judgmentsFor(reportHash(fine)) shouldBe Map(2 -> true)
    evidence(after) shouldBe evidence(before)
    evidence(after).head._4.map(_._2) shouldBe Some(90L)
  }

  test("judgments and credentials dropped before a restart do not come back") {
    val store = new InMemoryRoleStore
    val before = new ExtrinsicPools(store = store)
    val bad = guarantee("bad")
    val old = guarantee("old", core = 1)
    val fine = guarantee("fine")
    before.recordCredentials(h("parent"), 30L, List(bad))
    before.recordCredentials(h("old-parent"), 5L, List(old))
    before.addJudgment(reportHash(bad), 3L, 5, false, sig(0x55))
    before.addJudgment(reportHash(old), 1L, 4, false, sig(0x44))
    before.addJudgment(reportHash(fine), 3L, 2, true, sig(0x22))
    before.addJudgment(h("judged"), 3L, 1, false, sig(0x11))
    // The audit of `fine` is over, `judged` got its verdict, and epoch 1 is too old for a verdict in epoch 3.
    before.removeJudgments(List(reportHash(fine), reportHash(bad)))
    before.pruneDisputes(currentEpoch = 3L, epochLength = 12, judged = Set(h("judged")))
    before.judgmentCount shouldBe 1
    evidence(before).map(_._1) shouldBe List(reportHash(bad))

    val after = new ExtrinsicPools(store = store)
    after.judgmentCount shouldBe 1
    evidence(after) shouldBe evidence(before)
    // Only the credentials of the block two epochs back are gone.
    after.addJudgment(reportHash(old), 3L, 4, false, sig(0x44))
    evidence(after).find(_._1 == reportHash(old)).flatMap(_._4) shouldBe None
    evidence(after).find(_._1 == reportHash(bad)).flatMap(_._4).map(_._2) shouldBe Some(30L)
  }

  test("the exports and bundle lengths of included reports are still known after a restart") {
    val store = new InMemoryRoleStore
    val exporting = guarantee("exporting", core = 1, exports = 3, length = 4096)
    val plain = guarantee("plain", length = 77)
    new ExportIndex(store = store).record(List(exporting, plain))

    val after = new ExportIndex(store = store)
    after.get(h("exports-exporting")) shouldBe Some(ExportIndex.Entry(h("erasure-exporting"), 3, 1))
    after.get(h("exports-plain")) shouldBe None
    after.bundleLength(h("erasure-exporting")) shouldBe Some(4096)
    after.bundleLength(h("erasure-plain")) shouldBe Some(77)
  }

  test("after a restart the export index still lets its oldest entries go first, for good") {
    val store = new InMemoryRoleStore
    def g(n: Int) = guarantee(s"g$n", exports = 1, length = n)
    val before = new ExportIndex(maxEntries = 2, store = store)
    before.record(List(g(1)))
    before.record(List(g(2)))

    val restarted = new ExportIndex(maxEntries = 2, store = store)
    restarted.record(List(g(3)))
    restarted.get(h("exports-g1")) shouldBe None
    restarted.get(h("exports-g2")).isDefined shouldBe true
    restarted.get(h("exports-g3")).isDefined shouldBe true

    val again = new ExportIndex(maxEntries = 2, store = store)
    again.get(h("exports-g1")) shouldBe None
    again.bundleLength(h("erasure-g1")) shouldBe None
    again.get(h("exports-g2")).isDefined shouldBe true
    again.bundleLength(h("erasure-g3")) shouldBe Some(3)
    // Nothing is kept on disk for what memory let go.
    store.size shouldBe 4
  }
