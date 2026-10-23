package io.forge.jam.node

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{CoreIndex, Gas, Hash, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.work.PackageSpec
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.protocol.report.ReportTypes.AncestorHeader
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

/** Which pooled guarantees the author drops before building a block (they would fail the import). */
class GuaranteeUsableSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY // rotation 4 slots, L = 24
  private def h(s: String): Hash = Hashing.blake2b256(s.getBytes("UTF-8"))
  private val anchor = h("anchor")
  private val lookup = h("lookup")

  private def guarantee(slot: Long, lookupSlot: Long, anchorHash: Hash = anchor): GuaranteeExtrinsic =
    val report = WorkReport(
      packageSpec = PackageSpec(h("pkg"), UInt(0), Hash.zero, UShort(0), Hash.zero, UShort(0)),
      context = Context(anchorHash, Timeslot(UInt(lookupSlot.toInt)), Hash.zero, Hash.zero, lookup,
        Timeslot(UInt(lookupSlot.toInt)), Hash.zero, List.empty),
      coreIndex = CoreIndex(0),
      authorizerHash = Hash.zero,
      authGasUsed = Gas(0),
      authOutput = JamBytes.empty,
      segmentRootLookup = List.empty,
      results = List.empty
    )
    GuaranteeExtrinsic(report, Timeslot(UInt(slot.toInt)), List.empty)

  private def usable(g: GuaranteeExtrinsic, slot: Long, lookupSlot: Long = 100) =
    BlockAuthor.guaranteeUsable(g, slot, Set(anchor), List(AncestorHeader(lookupSlot, lookup)), config)

  test("a guarantee from the current or previous rotation with recent anchors is usable") {
    usable(guarantee(slot = 101, lookupSlot = 100), slot = 102) shouldBe true
    usable(guarantee(slot = 99, lookupSlot = 100), slot = 102) shouldBe true
  }

  test("a guarantee from before the previous rotation is dropped (ReportEpochBeforeLast)") {
    usable(guarantee(slot = 95, lookupSlot = 100), slot = 104) shouldBe false
  }

  test("a lookup anchor older than L or off our chain is dropped (LookupAnchorNotRecent)") {
    usable(guarantee(slot = 125, lookupSlot = 100), slot = 126) shouldBe false
    BlockAuthor.guaranteeUsable(guarantee(101, 100), 102, Set(anchor), Nil, config) shouldBe false
  }

  test("an anchor outside recent history is dropped (AnchorNotRecent)") {
    usable(guarantee(slot = 101, lookupSlot = 100, anchorHash = h("old")), slot = 102) shouldBe false
  }
