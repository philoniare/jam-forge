package io.forge.jam.protocol.refine

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.{WorkItem, WorkItemExtrinsic}
import io.forge.jam.core.types.workpackage.WorkPackage
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

class WorkPackageBundleSpec extends AnyFunSuite with Matchers:

  private def h(s: String): Hash = Hashing.blake2b256(s.getBytes("UTF-8"))
  private val data = "extrinsic".getBytes("UTF-8")

  private val wp = WorkPackage(
    authCodeHost = ServiceId(0),
    authCodeHash = h("auth"),
    context = Context(h("a"), Timeslot(1), h("s"), h("b"), h("a"), Timeslot(1), h("s"), Nil),
    authorization = JamBytes.empty,
    authorizerConfig = JamBytes.empty,
    items = List(
      WorkItem(
        ServiceId(0), h("code"), JamBytes.empty, Gas(1L), Gas(1L), Nil,
        List(WorkItemExtrinsic(Hashing.blake2b256(data), UInt(data.length))), UShort(0)
      )
    )
  )
  private val none = IndexedSeq(IndexedSeq.empty[Array[Byte]])

  test("a bundle's extrinsic data decodes when it matches the hashes its work items name") {
    val bundle = WorkPackageBundle(wp, IndexedSeq(IndexedSeq(data)), none, IndexedSeq(IndexedSeq.empty))
    WorkPackageBundle.decode(bundle.encode).map(_.extrinsicData.head.head.toSeq) shouldBe Right(data.toSeq)
  }

  test("a bundle whose extrinsic data does not match its work item's hash is refused") {
    val other = data.clone()
    other(0) = (other(0) ^ 1).toByte // same length, other bytes
    val bundle = WorkPackageBundle(wp, IndexedSeq(IndexedSeq(other)), none, IndexedSeq(IndexedSeq.empty))
    WorkPackageBundle.decode(bundle.encode).left.toOption.getOrElse(fail("decoded")) should include("hash")
  }
