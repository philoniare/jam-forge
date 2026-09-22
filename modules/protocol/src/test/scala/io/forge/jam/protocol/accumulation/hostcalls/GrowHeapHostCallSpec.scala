package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._
import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule}
import io.forge.jam.pvm.program.{JumpTable, ProgramBlob}
import io.forge.jam.protocol.HostCallPanic

class GrowHeapHostCallSpec extends HostCallTestBase:

  private val CgasGeminiConst = 275L
  private val CgasGeminiLinear = 121L

  test("GROW_HEAP: no-op when r7 <= h returns h and charges only the base cost") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance(gas = 100000L)
    instance.configureGrowHeap(hInitial = 48L, maxPage = 1044431L)

    instance.setReg(7, 48L) // r7 == h: no-op
    hostCalls.dispatch(HostCall.GROW_HEAP, instance)

    instance.reg(7) shouldBe 48L
    instance.gas shouldBe (100000L - CgasGeminiConst)
    instance.growHeapPagesGrown shouldBe 0L

    // r7 < h is the same branch.
    val instance2 = createMockInstance(gas = 100000L)
    instance2.configureGrowHeap(hInitial = 48L, maxPage = 1044431L)
    instance2.setReg(7, 10L)
    hostCalls.dispatch(HostCall.GROW_HEAP, instance2)
    instance2.reg(7) shouldBe 48L
    instance2.gas shouldBe (100000L - CgasGeminiConst)
    instance2.growHeapPagesGrown shouldBe 0L
  }

  test("GROW_HEAP: request beyond max page b returns h and charges only the base cost") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance(gas = 100000L)
    instance.configureGrowHeap(hInitial = 48L, maxPage = 1044431L)

    instance.setReg(7, 1044432L) // r7 == b + 1
    hostCalls.dispatch(HostCall.GROW_HEAP, instance)

    instance.reg(7) shouldBe 48L
    instance.gas shouldBe (100000L - CgasGeminiConst)
    instance.growHeapPagesGrown shouldBe 0L
  }

  test("GROW_HEAP: success charges 275 + 121*(r7-h), marks pages [h,r7) writable, returns r7") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance(gas = 100000L)
    instance.configureGrowHeap(hInitial = 48L, maxPage = 1044431L)

    instance.setReg(7, 50L) // grow by 2 pages
    hostCalls.dispatch(HostCall.GROW_HEAP, instance)

    val expectedGas = CgasGeminiConst + 2L * CgasGeminiLinear
    instance.reg(7) shouldBe 50L
    instance.gas shouldBe (100000L - expectedGas)
    instance.growHeapPagesGrown shouldBe 2L

    // heap pointer moved: a second no-op check against the NEW h confirms
    // the page-map effect actually stuck (not just a one-shot register write).
    instance.setReg(7, 50L)
    hostCalls.dispatch(HostCall.GROW_HEAP, instance)
    instance.reg(7) shouldBe 50L
    instance.growHeapPagesGrown shouldBe 2L // unchanged: still a no-op
  }

  test("GROW_HEAP: insufficient gas is OOG and leaves heap/page-map state unchanged") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance(gas = 300L) // < 275 + 121*2 = 517
    instance.configureGrowHeap(hInitial = 48L, maxPage = 1044431L)

    instance.setReg(7, 50L)
    hostCalls.dispatch(HostCall.GROW_HEAP, instance)

    instance.gas shouldBe 300L // unchanged — NOT decremented, NOT negative
    instance.isForcedOutOfGas shouldBe true
    instance.growHeapPagesGrown shouldBe 0L // no pages marked
  }

  test("GROW_HEAP: instance without a heap-page model throws (PANIC), never fabricates a heap pointer") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance(gas = 100000L) // configureGrowHeap NOT called

    instance.setReg(7, 999L)
    val ex = intercept[HostCallPanic] {
      hostCalls.dispatch(HostCall.GROW_HEAP, instance)
    }
    ex.getMessage should include("GrowHeap PANIC")
    instance.gas shouldBe 100000L // thrown BEFORE any gas charge
    instance.reg(7) shouldBe 999L // register untouched
  }

  test("dispatch: index 1 routes to grow_heap, index 2 routes to fetch (gp-0.8 renumbering)") {
    HostCall.GROW_HEAP shouldBe 1
    HostCall.FETCH shouldBe 2

    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)

    val growInstance = createMockInstance(gas = 10000L)
    growInstance.configureGrowHeap(hInitial = 48L, maxPage = 1044431L)
    growInstance.setReg(7, 48L)
    hostCalls.dispatch(1, growInstance)
    growInstance.reg(7) shouldBe 48L
    growInstance.gas shouldBe (10000L - CgasGeminiConst)

    val fetchInstance = createMockInstance()
    fetchInstance.setReg(10, 0L) // FetchSelector.CONSTANTS
    fetchInstance.setReg(7, 0x1000L) // output address
    hostCalls.dispatch(2, fetchInstance)
    fetchInstance.reg(7) should not be 0L
  }

  private def realGrowHeapInstance(): InterpretedInstance =
    val blob = ProgramBlob(
      code = Array[Byte](0),
      bitmask = Array[Byte](1),
      jumpTable = JumpTable(Array.empty, 0),
      is64Bit = true,
      roData = new Array[Byte](100),
      rwData = Array.empty,
      stackSize = 100
    )
    val module = InterpretedModule.create(blob) match
      case Right(m)  => m
      case Left(err) => fail(s"module creation failed: $err")
    InterpretedInstance.fromModule(module, Array.empty)

  test("GROW_HEAP (real instance): growHeapPageBounds matches the hand-computed a/b") {
    val instance = realGrowHeapInstance()
    val (h, b) = instance.growHeapPageBounds
    h shouldBe 48L
    // Z(100) = 65536 (gp #538 deviation: the stack reservation is zone-rounded,
    // not page-rounded) => b = (2^32 - 3*65536 - 2^24 - 65536) / 4096
    b shouldBe 1044416L

  }

  test("GROW_HEAP (real instance): success marks real PageMap pages writable via BasicMemory.sbrk") {
    val instance = realGrowHeapInstance()
    val wrapper = new InterpretedInstanceWrapper(instance)
    val pageSize = 4096

    // Page 48 (h) is NOT writable before growth (no static rwData/heap
    // pages were pre-allocated for this blob).
    wrapper.isMemoryWritable(48 * pageSize, 1) shouldBe false

    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    wrapper.setGas(100000L)
    wrapper.setReg(7, 50L) // grow to page 50 (2 pages: 48, 49)
    hostCalls.dispatch(HostCall.GROW_HEAP, wrapper)

    wrapper.reg(7) shouldBe 50L
    wrapper.gas shouldBe (100000L - (275L + 2L * 121L))
    // Pages 48 and 49 are now writable (real PageMap effect)...
    wrapper.isMemoryWritable(48 * pageSize, pageSize) shouldBe true
    wrapper.isMemoryWritable(49 * pageSize, pageSize) shouldBe true
    // ...but page 50 (the exclusive upper bound) is NOT.
    wrapper.isMemoryWritable(50 * pageSize, 1) shouldBe false

    // A follow-up query (r7 == new h == 50) is a no-op against the moved
    // heap pointer, proving growHeapPageBounds re-derives h from the
    // grown basicMemory.heapEnd rather than a stale cached value.
    wrapper.setReg(7, 50L)
    hostCalls.dispatch(HostCall.GROW_HEAP, wrapper)
    wrapper.reg(7) shouldBe 50L
    wrapper.gas shouldBe (100000L - (275L + 2L * 121L) - 275L)
  }
