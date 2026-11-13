package io.forge.jam.protocol.refine

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.merkle.BinaryMerkle
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.{WorkItem, WorkItemImportSegment}
import io.forge.jam.core.types.workpackage.WorkPackage
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UShort

class ImportResolverSpec extends AnyFunSuite with Matchers:

  private def segment(i: Int): Array[Byte] = Array.fill[Byte](Csegmentsize.toInt)(i.toByte)
  private def h(s: String): Hash = Hashing.blake2b256(s.getBytes("UTF-8"))

  test("justifications from the exporter verify for every segment count, and only for the right segment and index") {
    for n <- Seq(1, 2, 3, 5, 8, 13) do
      val segments = (0 until n).map(segment)
      val root = AvailabilitySpecifier.segmentsRoot(segments)
      val proofs = BinaryMerkle.pagedJustifications(segments, 0).map(_._1)
      for i <- 0 until n do
        ImportResolver.verify(root, i, segments(i), proofs(i)) shouldBe true
        ImportResolver.verify(root, i, segment(99), proofs(i)) shouldBe false
        if n > 1 then ImportResolver.verify(root, (i + 1) % n, segments(i), proofs(i)) shouldBe false
      ImportResolver.verify(h("other"), 0, segments(0), proofs(0)) shouldBe false
  }

  private def wp(imports: List[WorkItemImportSegment]) = WorkPackage(
    authCodeHost = ServiceId(0),
    authCodeHash = h("auth"),
    context = Context(h("a"), Timeslot(1), h("s"), h("b"), h("a"), Timeslot(1), h("s"), Nil),
    authorization = JamBytes.empty,
    authorizerConfig = JamBytes.empty,
    items = List(
      WorkItem(ServiceId(0), h("code"), JamBytes.empty, Gas(1L), Gas(1L), imports, Nil, UShort(0))
    )
  )

  test("imports by package hash resolve through reported packages into the lookup; unknown packages are refused") {
    val root = h("root")
    val p = wp(List(WorkItemImportSegment(root, UShort(3)), WorkItemImportSegment(h("pkg"), UShort(0x8000 | 2))))
    val Right((imports, lookup)) = ImportResolver.resolve(p, Map(h("pkg") -> h("pkg-root"))): @unchecked
    imports shouldBe IndexedSeq(IndexedSeq(ImportResolver.Import(root, 3), ImportResolver.Import(h("pkg-root"), 2)))
    lookup shouldBe Map(h("pkg") -> h("pkg-root"))
    ImportResolver.resolve(p, Map.empty).isLeft shouldBe true
  }

  test("a bundle's imports are checked against their justifications") {
    val segments = (0 until 3).map(segment)
    val root = AvailabilitySpecifier.segmentsRoot(segments)
    val proofs = BinaryMerkle.pagedJustifications(segments, 0).map(_._1)
    val p = wp(List(WorkItemImportSegment(h("pkg"), UShort(0x8000 | 1))))
    val good = WorkPackageBundle(p, IndexedSeq(IndexedSeq.empty), IndexedSeq(IndexedSeq(segments(1))), IndexedSeq(IndexedSeq(proofs(1))))
    ImportResolver.verifyBundle(good, Map(h("pkg") -> root)) shouldBe Right(())
    ImportResolver.verifyBundle(good.copy(importSegments = IndexedSeq(IndexedSeq(segments(2)))), Map(h("pkg") -> root)).isLeft shouldBe
      true
    ImportResolver.verifyBundle(good, Map(h("pkg") -> h("wrong"))).isLeft shouldBe true
    ImportResolver.verifyBundle(good, Map.empty).isLeft shouldBe true
  }

  test("a justification derived from a page's paged-proof segment equals the exporter's and verifies") {
    for n <- Seq(1, 2, 3, 63, 64, 65, 130) do
      val segments = (0 until n).map(i => Array.fill[Byte](Csegmentsize.toInt)((i * 7).toByte))
      val root = AvailabilitySpecifier.segmentsRoot(segments)
      val pages = AvailabilitySpecifier.pagedProofs(segments)
      pages.size shouldBe ImportResolver.pagedProofCount(n)
      val expected = BinaryMerkle.pagedJustifications(segments, 0).map(_._1)
      for i <- 0 until n do
        val j = ImportResolver.justificationFromPagedProof(pages(i / ImportResolver.PageSize), i, n).getOrElse(fail(s"n=$n i=$i"))
        j.map(_.toList) shouldBe expected(i).map(_.toList)
        ImportResolver.verify(root, i, segments(i), j) shouldBe true
    ImportResolver.justificationFromPagedProof(Array.fill[Byte](Csegmentsize.toInt)(0xff.toByte), 0, 3) shouldBe None
  }
