package io.forge.jam.protocol.refine

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes, constants}
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.history.HistoricalBeta
import io.forge.jam.core.types.workitem.{WorkItem, WorkItemExtrinsic, WorkItemImportSegment}
import io.forge.jam.core.types.workpackage.WorkPackage
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UInt, UShort}

class WorkPackageValidatorSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private def h(s: String): Hash = Hashing.blake2b256(s.getBytes("UTF-8"))

  private val anchor = HistoricalBeta(h("anchor"), h("beefy"), h("root"), 10L, Nil)

  private val item = WorkItem(
    service = ServiceId(0),
    codeHash = h("code"),
    payload = JamBytes("x".getBytes("UTF-8")),
    refineGasLimit = Gas(1_000_000L),
    accumulateGasLimit = Gas(1_000_000L),
    importSegments = List.empty,
    extrinsic = List.empty,
    exportCount = UShort(0)
  )

  private val wp = WorkPackage(
    authCodeHost = ServiceId(0),
    authCodeHash = h("auth"),
    context = Context(
      anchor = anchor.headerHash,
      anchorSlot = Timeslot(10),
      stateRoot = anchor.stateRoot,
      beefyRoot = anchor.beefyRoot,
      lookupAnchor = anchor.headerHash,
      lookupAnchorSlot = Timeslot(10),
      lookupAnchorStateRoot = anchor.stateRoot,
      prerequisites = List.empty
    ),
    authorization = JamBytes.empty,
    authorizerConfig = JamBytes.empty,
    items = List(item)
  )

  private val chain = WorkPackageValidator.ChainContext(
    authPool = List(IsAuthorizedExecutor.authorizerHashOf(wp)),
    recentHistory = List(anchor),
    slot = 12L
  )

  private def check(p: WorkPackage = wp, core: Int = 0, c: WorkPackageValidator.ChainContext = chain) =
    WorkPackageValidator.validate(p, core, c, config)

  test("a package within the limits, authorized, with a recent anchor passes") {
    check() shouldBe Right(())
  }

  test("the authorizer must be in the core's pool") {
    check(c = chain.copy(authPool = List(h("other")))).left.toOption.get should include("authorizer")
  }

  test("the anchor must be in recent history with matching roots and slot") {
    check(c = chain.copy(recentHistory = Nil)).isLeft shouldBe true
    check(wp.copy(context = wp.context.copy(stateRoot = h("bad")))).isLeft shouldBe true
    check(wp.copy(context = wp.context.copy(beefyRoot = h("bad")))).isLeft shouldBe true
    check(wp.copy(context = wp.context.copy(anchorSlot = Timeslot(11)))).isLeft shouldBe true
  }

  test("the lookup anchor must be within the maximum lookup-anchor age and not in the future") {
    check(c = chain.copy(slot = 10L + config.maxLookupAnchorAge)) shouldBe Right(())
    check(c = chain.copy(slot = 11L + config.maxLookupAnchorAge)).isLeft shouldBe true
    check(c = chain.copy(slot = 9L)).isLeft shouldBe true
  }

  test("item, import, export, extrinsic, size, gas and dependency limits") {
    check(wp.copy(items = Nil)).isLeft shouldBe true
    check(wp.copy(items = List.fill(config.maxWorkItems + 1)(item))).isLeft shouldBe true
    check(core = config.coresCount).isLeft shouldBe true
    val seg = WorkItemImportSegment(h("seg"), UShort(0))
    check(wp.copy(items = List(item.copy(importSegments = List.fill(constants.Cmaxpackageimports + 1)(seg))))).isLeft shouldBe
      true
    check(wp.copy(items = List(item.copy(exportCount = UShort(constants.Cmaxpackageexports + 1))))).isLeft shouldBe true
    val ext = WorkItemExtrinsic(h("e"), UInt(1))
    check(wp.copy(items = List(item.copy(extrinsic = List.fill(constants.Cmaxpackagexts + 1)(ext))))).isLeft shouldBe true
    check(wp.copy(items = List(item.copy(extrinsic = List(WorkItemExtrinsic(h("big"), UInt(constants.Cmaxinputsize.toInt + 1))))))).isLeft shouldBe
      true
    check(wp.copy(items = List(item.copy(accumulateGasLimit = Gas(config.reportAccGas))))) shouldBe Right(())
    check(wp.copy(items = List(item.copy(accumulateGasLimit = Gas(config.reportAccGas + 1))))).isLeft shouldBe true
    check(wp.copy(items = List(item.copy(refineGasLimit = Gas(-1L))))).isLeft shouldBe true // u64 max, not -1
    // Imports by package hash (index top bit) and prerequisites count as dependencies.
    val boxplus = (0 to config.maxDependencies).map(i => WorkItemImportSegment(h(s"wp$i"), UShort(0x8000))).toList
    check(wp.copy(items = List(item.copy(importSegments = boxplus)))).isLeft shouldBe true
    check(wp.copy(items = List(item.copy(importSegments = boxplus.take(config.maxDependencies))))) shouldBe Right(())
  }
