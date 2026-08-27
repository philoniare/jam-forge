package io.forge.jam.protocol.refine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.protocol.accumulation.{
  ConstantsBlob,
  HostCall,
  HostCallResult,
  MockPvmInstance
}
import spire.math.{ULong, UShort}

import scala.collection.mutable

class RefineHostCallsSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY

  private class MapLookupService(
      preimages: Map[(Long, Hash), Array[Byte]],
      services: Set[Long]
  ) extends HistoricalLookupService:
    val lookups = mutable.ListBuffer.empty[(Long, Long, Hash)]
    def serviceExists(serviceId: Long): Boolean = services.contains(serviceId)
    def historicalLookup(
        serviceId: Long,
        lookupAnchorTimeslot: Long,
        hash: Hash
    ): Option[Array[Byte]] =
      lookups += ((serviceId, lookupAnchorTimeslot, hash))
      preimages.get((serviceId, hash))

  private def testWorkItem(
      service: Long = 42L,
      payload: Array[Byte] = Array[Byte](1, 2, 3)
  ): WorkItem =
    WorkItem(
      service = ServiceId(service.toInt),
      codeHash = Hash(Array.fill[Byte](32)(7)),
      payload = JamBytes(payload),
      refineGasLimit = Gas(1_000_000L),
      accumulateGasLimit = Gas(10_000L),
      importSegments = List.empty,
      extrinsic = List.empty,
      exportCount = UShort(2)
    )

  private def testWorkPackage(items: List[WorkItem]): WorkPackage =
    WorkPackage(
      authCodeHost = ServiceId(1),
      authCodeHash = Hash(Array.fill[Byte](32)(9)),
      context = Context(
        anchor = Hash(Array.fill[Byte](32)(1)),
        stateRoot = Hash(Array.fill[Byte](32)(2)),
        beefyRoot = Hash(Array.fill[Byte](32)(3)),
        lookupAnchor = Hash(Array.fill[Byte](32)(4)),
        lookupAnchorSlot = Timeslot(77),
        prerequisites = List.empty
      ),
      authorization = JamBytes(Array[Byte](0x0a, 0x0b)),
      authorizerConfig = JamBytes(Array[Byte](0x0c)),
      items = items
    )

  private def newContext(
      accounts: HistoricalLookupService = new MapLookupService(Map.empty, Set.empty),
      importSegments: IndexedSeq[IndexedSeq[Array[Byte]]] = IndexedSeq(IndexedSeq.empty),
      extrinsicData: IndexedSeq[IndexedSeq[Array[Byte]]] = IndexedSeq(IndexedSeq.empty),
      exportOffset: Long = 0L
  ): RefineContext =
    val item = testWorkItem()
    new RefineContext(
      config = config,
      workPackage = testWorkPackage(List(item)),
      workItemIndex = 0,
      coreIndex = 1,
      authorizerTrace = Array[Byte](0x55, 0x66),
      importSegments = importSegments,
      extrinsicData = extrinsicData,
      exportSegmentOffset = exportOffset,
      accounts = accounts
    )

  private def newInstance(): MockPvmInstance =
    new MockPvmInstance(memorySize = 0x100000, initialGas = 1_000_000L)

  // =========================================================================
  // fetch
  // =========================================================================

  test("FETCH selector 0 returns the shared constants blob") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()
    val expected = ConstantsBlob.build(config)

    instance.setReg(7, 0x1000L) // output address
    instance.setReg(8, 0L) // offset
    instance.setReg(9, expected.length.toLong) // length
    instance.setReg(10, 0L) // selector

    hc.dispatch(HostCall.FETCH, instance)

    instance.reg(7) shouldBe expected.length.toLong
    instance.readBytes(0x1000, expected.length).get shouldBe expected
  }

  test("FETCH selector 1 returns 32 zero bytes (refine entropy is zerohash)") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()

    instance.setReg(7, 0x1000L)
    instance.setReg(8, 0L)
    instance.setReg(9, 32L)
    instance.setReg(10, 1L)

    hc.dispatch(HostCall.FETCH, instance)

    instance.reg(7) shouldBe 32L
    instance.readBytes(0x1000, 32).get shouldBe Array.fill[Byte](32)(0)
  }

  test("FETCH selector 2 returns the authorizer trace") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()

    instance.setReg(7, 0x1000L)
    instance.setReg(8, 0L)
    instance.setReg(9, 100L)
    instance.setReg(10, 2L)

    hc.dispatch(HostCall.FETCH, instance)

    instance.reg(7) shouldBe 2L
    instance.readBytes(0x1000, 2).get shouldBe Array[Byte](0x55, 0x66)
  }

  test("FETCH selectors 3/4 serve extrinsic data with index bounds") {
    val xbar = IndexedSeq(IndexedSeq(Array[Byte](1, 1), Array[Byte](2, 2)))
    val ctx = newContext(extrinsicData = xbar)
    val hc = new RefineHostCalls(ctx)

    // selector 3: explicit item + extrinsic index
    val i1 = newInstance()
    i1.setReg(7, 0x1000L); i1.setReg(8, 0L); i1.setReg(9, 10L)
    i1.setReg(10, 3L); i1.setReg(11, 0L); i1.setReg(12, 1L)
    hc.dispatch(HostCall.FETCH, i1)
    i1.reg(7) shouldBe 2L
    i1.readBytes(0x1000, 2).get shouldBe Array[Byte](2, 2)

    // selector 4: current item, extrinsic index in r11
    val i2 = newInstance()
    i2.setReg(7, 0x1000L); i2.setReg(8, 0L); i2.setReg(9, 10L)
    i2.setReg(10, 4L); i2.setReg(11, 0L)
    hc.dispatch(HostCall.FETCH, i2)
    i2.reg(7) shouldBe 2L
    i2.readBytes(0x1000, 2).get shouldBe Array[Byte](1, 1)

    // out-of-range index → NONE
    val i3 = newInstance()
    i3.setReg(7, 0x1000L); i3.setReg(10, 3L); i3.setReg(11, 5L); i3.setReg(12, 0L)
    hc.dispatch(HostCall.FETCH, i3)
    ULong(i3.reg(7)) shouldBe HostCallResult.NONE
  }

  test("FETCH selectors 5/6 serve import segments") {
    val seg = Array.fill[Byte](8)(0x11)
    val ibar = IndexedSeq(IndexedSeq(seg))
    val ctx = newContext(importSegments = ibar)
    val hc = new RefineHostCalls(ctx)

    val i1 = newInstance()
    i1.setReg(7, 0x1000L); i1.setReg(8, 0L); i1.setReg(9, 8L)
    i1.setReg(10, 5L); i1.setReg(11, 0L); i1.setReg(12, 0L)
    hc.dispatch(HostCall.FETCH, i1)
    i1.reg(7) shouldBe 8L
    i1.readBytes(0x1000, 8).get shouldBe seg

    val i2 = newInstance()
    i2.setReg(7, 0x1000L); i2.setReg(8, 0L); i2.setReg(9, 8L)
    i2.setReg(10, 6L); i2.setReg(11, 0L)
    hc.dispatch(HostCall.FETCH, i2)
    i2.reg(7) shouldBe 8L
  }

  test("FETCH selector 12 returns the 62-byte work-item summary S(w)") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()

    instance.setReg(7, 0x1000L)
    instance.setReg(8, 0L)
    instance.setReg(9, 100L)
    instance.setReg(10, 12L)
    instance.setReg(11, 0L)

    hc.dispatch(HostCall.FETCH, instance)

    instance.reg(7) shouldBe 62L
    val s = instance.readBytes(0x1000, 62).get
    // service id (4 LE)
    s.take(4) shouldBe Array[Byte](42, 0, 0, 0)
    // code hash
    s.slice(4, 36) shouldBe Array.fill[Byte](32)(7)
    // refine gas limit (8 LE) = 1_000_000
    java.nio.ByteBuffer
      .wrap(s.slice(36, 44))
      .order(java.nio.ByteOrder.LITTLE_ENDIAN)
      .getLong shouldBe 1_000_000L
    // export count (2 LE) = 2, imports = 0, extrinsics = 0
    s.slice(52, 54) shouldBe Array[Byte](2, 0)
    s.slice(54, 56) shouldBe Array[Byte](0, 0)
    s.slice(56, 58) shouldBe Array[Byte](0, 0)
    // payload length (4 LE) = 3
    s.slice(58, 62) shouldBe Array[Byte](3, 0, 0, 0)
  }

  test("FETCH selector 13 returns the work-item payload; 14/15 are NONE in refine") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)

    val i1 = newInstance()
    i1.setReg(7, 0x1000L); i1.setReg(8, 0L); i1.setReg(9, 10L)
    i1.setReg(10, 13L); i1.setReg(11, 0L)
    hc.dispatch(HostCall.FETCH, i1)
    i1.reg(7) shouldBe 3L
    i1.readBytes(0x1000, 3).get shouldBe Array[Byte](1, 2, 3)

    for sel <- List(14L, 15L) do
      val i = newInstance()
      i.setReg(7, 0x1000L); i.setReg(10, sel)
      hc.dispatch(HostCall.FETCH, i)
      ULong(i.reg(7)) shouldBe HostCallResult.NONE
  }

  // =========================================================================
  // export
  // =========================================================================

  test("EXPORT zero-pads to the segment size and returns the segment index") {
    val ctx = newContext(exportOffset = 5L)
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()

    instance.writeBytes(0x2000, Array[Byte](1, 2, 3, 4)) shouldBe true
    instance.setReg(7, 0x2000L)
    instance.setReg(8, 4L)

    hc.dispatch(HostCall.EXPORT, instance)

    instance.reg(7) shouldBe 5L // exportSegmentOffset + 0 prior exports
    ctx.exports.size shouldBe 1
    ctx.exports.head.length shouldBe Csegmentsize.toInt
    ctx.exports.head.take(4) shouldBe Array[Byte](1, 2, 3, 4)
    ctx.exports.head.drop(4).forall(_ == 0) shouldBe true

    // Second export gets the next index.
    instance.setReg(7, 0x2000L)
    instance.setReg(8, 4L)
    hc.dispatch(HostCall.EXPORT, instance)
    instance.reg(7) shouldBe 6L
  }

  test("EXPORT reports FULL at the Cmaxpackageexports bound") {
    val ctx = newContext(exportOffset = 3072L)
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()

    instance.setReg(7, 0x2000L)
    instance.setReg(8, 0L)
    hc.dispatch(HostCall.EXPORT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.FULL
    ctx.exports shouldBe empty
  }

  // =========================================================================
  // historical_lookup
  // =========================================================================

  test("HISTORICAL_LOOKUP resolves own-service preimages via the anchor timeslot") {
    val hash = Hash(Array.fill[Byte](32)(0x33))
    val blob = Array[Byte](9, 8, 7, 6)
    val accounts =
      new MapLookupService(Map((42L, hash) -> blob), Set(42L))
    val ctx = newContext(accounts = accounts)
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()

    instance.writeBytes(0x3000, hash.bytes.toArray) shouldBe true
    instance.setReg(7, -1L) // 2^64 - 1 → own service
    instance.setReg(8, 0x3000L) // hash address
    instance.setReg(9, 0x4000L) // output address
    instance.setReg(10, 0L) // offset
    instance.setReg(11, 10L) // length

    hc.dispatch(HostCall.HISTORICAL_LOOKUP, instance)

    instance.reg(7) shouldBe 4L
    instance.readBytes(0x4000, 4).get shouldBe blob
    // lookup went through the anchor timeslot of the package context (77)
    accounts.lookups.head shouldBe ((42L, 77L, hash))
  }

  test("HISTORICAL_LOOKUP returns NONE for unknown service or missing preimage") {
    val accounts = new MapLookupService(Map.empty, Set(42L))
    val ctx = newContext(accounts = accounts)
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()

    instance.writeBytes(0x3000, Array.fill[Byte](32)(1)) shouldBe true
    instance.setReg(7, 999L) // unknown service
    instance.setReg(8, 0x3000L)
    instance.setReg(9, 0x4000L)

    hc.dispatch(HostCall.HISTORICAL_LOOKUP, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.NONE

    // known service, missing preimage
    instance.setReg(7, 42L)
    hc.dispatch(HostCall.HISTORICAL_LOOKUP, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.NONE
  }

  // =========================================================================
  // machine / peek / poke / pages / expunge
  // =========================================================================

  /** Minimal valid deblob-format blob: 0 jump-table entries, entry size 0,
    * 1-byte code (panic), 1-byte bitmask.
    */
  private val validGuestCode: Array[Byte] = Array[Byte](0, 0, 1, 0, 1)

  private def createMachine(hc: RefineHostCalls, pc: Long = 7L): Long =
    val instance = newInstance()
    instance.writeBytes(0x5000, validGuestCode) shouldBe true
    instance.setReg(7, 0x5000L)
    instance.setReg(8, validGuestCode.length.toLong)
    instance.setReg(9, pc)
    hc.dispatch(HostCall.MACHINE, instance)
    instance.reg(7)

  test("MACHINE allocates ascending indices for valid code and HUH for garbage") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)

    createMachine(hc) shouldBe 0L
    createMachine(hc) shouldBe 1L
    ctx.innerPvms(0L).pc shouldBe 7L

    // Garbage code → HUH
    val instance = newInstance()
    instance.writeBytes(0x5000, Array[Byte](-1, -1, -1)) shouldBe true
    instance.setReg(7, 0x5000L)
    instance.setReg(8, 3L)
    instance.setReg(9, 0L)
    hc.dispatch(HostCall.MACHINE, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("MACHINE reuses the lowest expunged index") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)
    createMachine(hc) shouldBe 0L
    createMachine(hc) shouldBe 1L

    val instance = newInstance()
    instance.setReg(7, 0L)
    hc.dispatch(HostCall.EXPUNGE, instance)
    instance.reg(7) shouldBe 7L // returns the guest's pc
    ctx.innerPvms.contains(0L) shouldBe false

    createMachine(hc) shouldBe 0L
  }

  test("PEEK/POKE report WHO for a missing machine and OOB before pages are granted") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()

    // WHO: no machine 5
    instance.setReg(7, 5L); instance.setReg(8, 0x1000L)
    instance.setReg(9, 0x10000L); instance.setReg(10, 4L)
    hc.dispatch(HostCall.PEEK, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.WHO

    createMachine(hc) shouldBe 0L

    // OOB: guest pages not accessible yet
    instance.setReg(7, 0L); instance.setReg(8, 0x1000L)
    instance.setReg(9, 0x10000L); instance.setReg(10, 4L)
    hc.dispatch(HostCall.PEEK, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OOB
  }

  test("PAGES grants access and POKE/PEEK round-trip through guest RAM") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()

    createMachine(hc) shouldBe 0L

    // pages(n=0, p=16, c=2, r=2): two writable zeroed pages at 0x10000
    instance.setReg(7, 0L); instance.setReg(8, 16L)
    instance.setReg(9, 2L); instance.setReg(10, 2L)
    hc.dispatch(HostCall.PAGES, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // poke outer[0x1000..] -> guest[0x10000..]
    instance.writeBytes(0x1000, Array[Byte](0x21, 0x22, 0x23, 0x24)) shouldBe true
    instance.setReg(7, 0L); instance.setReg(8, 0x1000L)
    instance.setReg(9, 0x10000L); instance.setReg(10, 4L)
    hc.dispatch(HostCall.POKE, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // peek guest[0x10000..] -> outer[0x2000..]
    instance.setReg(7, 0L); instance.setReg(8, 0x2000L)
    instance.setReg(9, 0x10000L); instance.setReg(10, 4L)
    hc.dispatch(HostCall.PEEK, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    instance.readBytes(0x2000, 4).get shouldBe Array[Byte](0x21, 0x22, 0x23, 0x24)
  }

  test("PAGES validates variant and page bounds") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()
    createMachine(hc) shouldBe 0L

    // variant > 4 → HUH
    instance.setReg(7, 0L); instance.setReg(8, 16L)
    instance.setReg(9, 1L); instance.setReg(10, 5L)
    hc.dispatch(HostCall.PAGES, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.HUH

    // p < 16 → HUH
    instance.setReg(7, 0L); instance.setReg(8, 15L)
    instance.setReg(9, 1L); instance.setReg(10, 1L)
    hc.dispatch(HostCall.PAGES, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.HUH

    // p + c beyond the 2^20 page space → HUH
    instance.setReg(7, 0L); instance.setReg(8, (1L << 20) - 1)
    instance.setReg(9, 2L); instance.setReg(10, 1L)
    hc.dispatch(HostCall.PAGES, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.HUH

    // variant 3 (preserve, set R) on inaccessible pages → HUH
    instance.setReg(7, 0L); instance.setReg(8, 100L)
    instance.setReg(9, 1L); instance.setReg(10, 3L)
    hc.dispatch(HostCall.PAGES, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.HUH

    // read-only grant: poke must then fail with OOB
    instance.setReg(7, 0L); instance.setReg(8, 16L)
    instance.setReg(9, 1L); instance.setReg(10, 1L)
    hc.dispatch(HostCall.PAGES, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    instance.writeBytes(0x1000, Array[Byte](1)) shouldBe true
    instance.setReg(7, 0L); instance.setReg(8, 0x1000L)
    instance.setReg(9, 0x10000L); instance.setReg(10, 1L)
    hc.dispatch(HostCall.POKE, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OOB
  }

  test("PAGES variant 0 revokes access; variants < 3 zero the contents") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()
    createMachine(hc) shouldBe 0L

    // grant W, write data
    instance.setReg(7, 0L); instance.setReg(8, 16L)
    instance.setReg(9, 1L); instance.setReg(10, 2L)
    hc.dispatch(HostCall.PAGES, instance)
    instance.writeBytes(0x1000, Array[Byte](0x7f)) shouldBe true
    instance.setReg(7, 0L); instance.setReg(8, 0x1000L)
    instance.setReg(9, 0x10000L); instance.setReg(10, 1L)
    hc.dispatch(HostCall.POKE, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // re-grant W with variant 2 → contents zeroed
    instance.setReg(7, 0L); instance.setReg(8, 16L)
    instance.setReg(9, 1L); instance.setReg(10, 2L)
    hc.dispatch(HostCall.PAGES, instance)
    instance.setReg(7, 0L); instance.setReg(8, 0x2000L)
    instance.setReg(9, 0x10000L); instance.setReg(10, 1L)
    hc.dispatch(HostCall.PEEK, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    instance.readBytes(0x2000, 1).get shouldBe Array[Byte](0)

    // variant 0 revokes: subsequent peek is OOB
    instance.setReg(7, 0L); instance.setReg(8, 16L)
    instance.setReg(9, 1L); instance.setReg(10, 0L)
    hc.dispatch(HostCall.PAGES, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    instance.setReg(7, 0L); instance.setReg(8, 0x2000L)
    instance.setReg(9, 0x10000L); instance.setReg(10, 1L)
    hc.dispatch(HostCall.PEEK, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OOB
  }

  // =========================================================================
  // invoke
  // =========================================================================

  /** Wrap raw (code, bitmask) into a deblob blob for MACHINE. */
  private def guestBlob(code: Array[Byte], bitmask: Array[Byte]): Array[Byte] =
    Array[Byte](0, 0, code.length.toByte) ++ code ++ bitmask

  private def createMachineWith(
      hc: RefineHostCalls,
      blob: Array[Byte],
      pc: Long
  ): Long =
    val instance = newInstance()
    instance.writeBytes(0x5000, blob) shouldBe true
    instance.setReg(7, 0x5000L)
    instance.setReg(8, blob.length.toLong)
    instance.setReg(9, pc)
    hc.dispatch(HostCall.MACHINE, instance)
    instance.reg(7)

  /** Write the 112-byte invoke block (gas + 13 registers) at `addr`. */
  private def writeInvokeBlock(
      instance: MockPvmInstance,
      addr: Int,
      gas: Long,
      regs: Map[Int, Long] = Map.empty
  ): Unit =
    val block = new Array[Byte](112)
    def putLE8(offset: Int, value: Long): Unit =
      var i = 0
      while i < 8 do
        block(offset + i) = ((value >> (8 * i)) & 0xff).toByte
        i += 1
    putLE8(0, gas)
    regs.foreach { case (r, v) => putLE8(8 + 8 * r, v) }
    instance.writeBytes(addr, block) shouldBe true

  private def readInvokeBlock(
      instance: MockPvmInstance,
      addr: Int
  ): (Long, Array[Long]) =
    val block = instance.readBytes(addr, 112).get
    def getLE8(offset: Int): Long =
      var v = 0L
      var i = 0
      while i < 8 do
        v |= (block(offset + i).toLong & 0xff) << (8 * i)
        i += 1
      v
    (getLE8(0), Array.tabulate(13)(i => getLE8(8 + 8 * i)))

  test("INVOKE runs a guest to halt and writes back gas and registers") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)

    // LoadImm r7 = 42; JumpIndirect r0 + 0 (r0 = 0xffff0000 → halt).
    val code = Array[Byte](51, 7, 42, 50, 0)
    val bitmask = Array[Byte](9) // instructions at offsets 0 and 3
    createMachineWith(hc, guestBlob(code, bitmask), pc = 0L) shouldBe 0L

    val instance = newInstance()
    writeInvokeBlock(instance, 0x8000, gas = 100L, regs = Map(0 -> 0xffff0000L))
    instance.setReg(7, 0L) // machine index
    instance.setReg(8, 0x8000L) // block address
    hc.dispatch(HostCall.INVOKE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HALT
    val (gasAfter, regsAfter) = readInvokeBlock(instance, 0x8000)
    gasAfter shouldBe 98L // two instructions, 1 gas each
    regsAfter(7) shouldBe 42L
    regsAfter(0) shouldBe 0xffff0000L
  }

  test("INVOKE reports HOST on guest ecalli and resumes past it") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)

    // ecalli 5; JumpIndirect r0 + 0.
    val code = Array[Byte](10, 5, 50, 0)
    val bitmask = Array[Byte](5) // instructions at offsets 0 and 2
    createMachineWith(hc, guestBlob(code, bitmask), pc = 0L) shouldBe 0L

    val instance = newInstance()
    writeInvokeBlock(instance, 0x8000, gas = 100L, regs = Map(0 -> 0xffff0000L))
    instance.setReg(7, 0L)
    instance.setReg(8, 0x8000L)
    hc.dispatch(HostCall.INVOKE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HOST
    instance.reg(8) shouldBe 5L // guest host-call id
    ctx.innerPvms(0L).pc shouldBe 2L // resumed past the ecalli

    // Second invoke continues from pc 2 and halts.
    instance.setReg(7, 0L)
    instance.setReg(8, 0x8000L)
    hc.dispatch(HostCall.INVOKE, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.HALT
  }

  test("INVOKE reports FAULT with the faulting page address") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)

    // LoadU8 r7 ← mem[0x10000]; no guest pages granted → page fault.
    val code = Array[Byte](52, 7, 0, 0, 1)
    val bitmask = Array[Byte](1)
    createMachineWith(hc, guestBlob(code, bitmask), pc = 0L) shouldBe 0L

    val instance = newInstance()
    writeInvokeBlock(instance, 0x8000, gas = 100L)
    instance.setReg(7, 0L)
    instance.setReg(8, 0x8000L)
    hc.dispatch(HostCall.INVOKE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.FAULT
    instance.reg(8) shouldBe 0x10000L
  }

  test("INVOKE reads guest memory granted via PAGES and poked from the host") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)

    // LoadU8 r7 ← mem[0x10000]; page granted and poked this time.
    val code = Array[Byte](52, 7, 0, 0, 1)
    val bitmask = Array[Byte](1)
    createMachineWith(hc, guestBlob(code, bitmask), pc = 0L) shouldBe 0L

    val setup = newInstance()
    // pages(n=0, p=16, c=1, r=2): writable page at 0x10000
    setup.setReg(7, 0L); setup.setReg(8, 16L)
    setup.setReg(9, 1L); setup.setReg(10, 2L)
    hc.dispatch(HostCall.PAGES, setup)
    ULong(setup.reg(7)) shouldBe HostCallResult.OK
    // poke guest[0x10000] = 0x5a
    setup.writeBytes(0x1000, Array[Byte](0x5a)) shouldBe true
    setup.setReg(7, 0L); setup.setReg(8, 0x1000L)
    setup.setReg(9, 0x10000L); setup.setReg(10, 1L)
    hc.dispatch(HostCall.POKE, setup)
    ULong(setup.reg(7)) shouldBe HostCallResult.OK

    val instance = newInstance()
    writeInvokeBlock(instance, 0x8000, gas = 100L)
    instance.setReg(7, 0L)
    instance.setReg(8, 0x8000L)
    hc.dispatch(HostCall.INVOKE, instance)

    // Guest ran off the end of the single-instruction program → panic, but the
    // load itself succeeded and r7 was written back as 0x5a.
    val (_, regsAfter) = readInvokeBlock(instance, 0x8000)
    regsAfter(7) shouldBe 0x5aL
  }

  test("INVOKE reports OOG when the guest runs out of gas") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)
    val code = Array[Byte](51, 7, 42, 50, 0)
    val bitmask = Array[Byte](9)
    createMachineWith(hc, guestBlob(code, bitmask), pc = 0L) shouldBe 0L

    val instance = newInstance()
    writeInvokeBlock(instance, 0x8000, gas = 1L, regs = Map(0 -> 0xffff0000L))
    instance.setReg(7, 0L)
    instance.setReg(8, 0x8000L)
    hc.dispatch(HostCall.INVOKE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OOG
  }

  test("INVOKE reports WHO for an unknown machine and panics on unwritable block") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)

    val instance = newInstance()
    writeInvokeBlock(instance, 0x8000, gas = 10L)
    instance.setReg(7, 9L) // no machine 9
    instance.setReg(8, 0x8000L)
    hc.dispatch(HostCall.INVOKE, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.WHO

    instance.setReg(7, 0L)
    instance.setReg(8, 0x100000L - 10L) // 112-byte block does not fit
    intercept[RuntimeException] {
      hc.dispatch(HostCall.INVOKE, instance)
    }
  }

  test("unknown host calls report WHAT") {
    val ctx = newContext()
    val hc = new RefineHostCalls(ctx)
    val instance = newInstance()
    hc.dispatch(77, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.WHAT
  }
