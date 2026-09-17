package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._
import spire.math.ULong

class HostCallGasCostSpec extends HostCallTestBase:

  test("flat-cost host calls charge their base constant") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    hostCalls.getGasCost(HostCall.GAS, instance) shouldBe 48L
    hostCalls.getGasCost(HostCall.INFO, instance) shouldBe 703L
    hostCalls.getGasCost(HostCall.ASSIGN, instance) shouldBe 1818L
    hostCalls.getGasCost(HostCall.CHECKPOINT, instance) shouldBe 103L
    hostCalls.getGasCost(HostCall.NEW, instance) shouldBe 3855L
    hostCalls.getGasCost(HostCall.UPGRADE, instance) shouldBe 1028L
    hostCalls.getGasCost(HostCall.EJECT, instance) shouldBe 458L
    hostCalls.getGasCost(HostCall.QUERY, instance) shouldBe 643L
    hostCalls.getGasCost(HostCall.SOLICIT, instance) shouldBe 2193L
    hostCalls.getGasCost(HostCall.FORGET, instance) shouldBe 3250L
    hostCalls.getGasCost(HostCall.YIELD, instance) shouldBe 98L
  }

  test("LOOKUP (3) charges 600 + fnmemgas(248, z) where z = r11") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(11, 0L)
    hostCalls.getGasCost(HostCall.LOOKUP, instance) shouldBe 600L

    // z=203: fnmemgas(248, 203) = ceil(248*203/1024) = ceil(50344/1024) = 50
    instance.setReg(11, 203L)
    hostCalls.getGasCost(HostCall.LOOKUP, instance) shouldBe 650L
  }

  test("READ (4) charges 2407 + fnmemgas(1736,k) + fnmemgas(248,v), k=r9 v=r12") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // k=32, v=203 (exact arithmetic pinned per the brief's TDD requirement):
    // fnmemgas(1736,32) = ceil(1736*32/1024) = ceil(55552/1024) = ceil(54.25) = 55
    // fnmemgas(248,203)  = ceil(248*203/1024) = ceil(50344/1024) = ceil(49.16..) = 50
    instance.setReg(9, 32L)
    instance.setReg(12, 203L)
    hostCalls.getGasCost(HostCall.READ, instance) shouldBe (2407L + 55L + 50L)
  }

  test("WRITE (5) charges 2442 + fnmemgas(3358,k) + fnmemgas(216,v), k=r8 v=r10") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // k=32, v=203:
    // fnmemgas(3358,32) = ceil(3358*32/1024) = ceil(107456/1024) = 105
    // fnmemgas(216,203)  = ceil(216*203/1024) = ceil(43848/1024) = ceil(42.82..) = 43
    instance.setReg(8, 32L)
    instance.setReg(10, 203L)
    hostCalls.getGasCost(HostCall.WRITE, instance) shouldBe (2442L + 105L + 43L)
  }

  test("BLESS (15) charges 422 + 20*n, n=r12") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(12, 5L)
    hostCalls.getGasCost(HostCall.BLESS, instance) shouldBe (422L + 20L * 5L)
  }

  test("DESIGNATE (17) charges 1100 + 302*z, z=r8 (NOT r7)") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(7, 999L) // must be ignored by the gas formula
    instance.setReg(8, 6L)
    hostCalls.getGasCost(HostCall.DESIGNATE, instance) shouldBe (1100L + 302L * 6L)
  }

  test("TRANSFER (21) pre-charge is the base 575 only — t is per-outcome, charged by the handler on OK alone") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(9, 100L)
    hostCalls.getGasCost(HostCall.TRANSFER, instance) shouldBe 575L

    instance.setReg(9, 0L)
    hostCalls.getGasCost(HostCall.TRANSFER, instance) shouldBe 575L
    instance.setReg(9, Long.MaxValue)
    hostCalls.getGasCost(HostCall.TRANSFER, instance) shouldBe 575L
  }

  test("PROVIDE (27) charges 3980 + fnmemgas(2264,z), z=r9") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // z=1024 exact: fnmemgas(2264,1024) = ceil(2264*1024/1024) = 2264
    instance.setReg(9, 1024L)
    hostCalls.getGasCost(HostCall.PROVIDE, instance) shouldBe (3980L + 2264L)
  }

  // ===========================================================================
  // fetch (2): per-selector case table, cases 0/2/14/15/otherwise
  // ===========================================================================

  test("FETCH (2) case 0 (constants): c=390, l=0 -> no fnmemgas term") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(10, 0L) // selector
    instance.setReg(9, 500L) // z — irrelevant since l=0
    hostCalls.getGasCost(HostCall.FETCH, instance) shouldBe 390L
  }

  test("FETCH (2) case 2 (auth trace): c=80, l=96") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(10, 2L)
    // fnmemgas(96, 10) = ceil(96*10/1024) = ceil(960/1024) = 1
    instance.setReg(9, 10L)
    hostCalls.getGasCost(HostCall.FETCH, instance) shouldBe (80L + 1L)
  }

  test("FETCH (2) case 14 (accumulate items / ALL_OPERANDS): c=287, l=400") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(10, 14L)
    // fnmemgas(400, 100) = ceil(40000/1024) = 40 (39.06.. rounds up)
    instance.setReg(9, 100L)
    hostCalls.getGasCost(HostCall.FETCH, instance) shouldBe (287L + 40L)
  }

  test("FETCH (2) case 15 (any accumulate item / SINGLE_OPERAND): c=355, l=344") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(10, 15L)
    instance.setReg(9, 0L)
    hostCalls.getGasCost(HostCall.FETCH, instance) shouldBe 355L
  }

  test("FETCH (2) otherwise bucket (selector >= 16): c=80, l=0") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(10, 16L)
    instance.setReg(9, 12345L) // irrelevant since l=0
    hostCalls.getGasCost(HostCall.FETCH, instance) shouldBe 80L

    instance.setReg(10, 999999L)
    hostCalls.getGasCost(HostCall.FETCH, instance) shouldBe 80L
  }

  // ===========================================================================
  // fnmemgas boundary cases (l=0, l=1024 exact, l=1025 rounds up)
  // ===========================================================================

  test("fnmemgas boundary: l=0 -> 0 (LOOKUP z=0)") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(11, 0L)
    hostCalls.getGasCost(HostCall.LOOKUP, instance) shouldBe HostCallGas.CgasLconst
  }

  test("fnmemgas boundary: l=1024 exact -> no rounding (LOOKUP z=1024)") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // fnmemgas(248, 1024) = ceil(248*1024/1024) = 248 exactly
    instance.setReg(11, 1024L)
    hostCalls.getGasCost(HostCall.LOOKUP, instance) shouldBe (600L + 248L)
  }

  test("fnmemgas boundary: l=1025 rounds up by exactly 1 (LOOKUP z=1025)") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // fnmemgas(248, 1025) = ceil(248*1025/1024) = ceil(254200/1024) = ceil(248.24..) = 249
    instance.setReg(11, 1025L)
    hostCalls.getGasCost(HostCall.LOOKUP, instance) shouldBe (600L + 249L)
  }

  test("fnmemgas: pure function boundary table") {
    HostCallGas.fnmemgas(248L, ULong(0L)) shouldBe 0L
    HostCallGas.fnmemgas(248L, ULong(1024L)) shouldBe 248L
    HostCallGas.fnmemgas(248L, ULong(1025L)) shouldBe 249L
    HostCallGas.fnmemgas(1L, ULong(1L)) shouldBe 1L // ceil(1/1024) = 1
  }

  test("fnmemgas: huge unsigned length saturates instead of overflowing/wrapping") {
    val huge = ULong(-1L) // 2^64 - 1 unsigned
    HostCallGas.fnmemgas(3358L, huge) shouldBe Long.MaxValue
  }

  // ===========================================================================
  // GROW_HEAP exclusion + unknown-call cost
  // ===========================================================================

  test("GROW_HEAP is excluded from the flat pre-charge (it self-meters)") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    hostCalls.getGasCost(HostCall.GROW_HEAP, instance) shouldBe 0L
  }

  test("unknown host call charges Cgasunknown=1000 (was 10) and returns WHAT") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    hostCalls.getGasCost(9999, instance) shouldBe 1000L
    hostCalls.dispatch(9999, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHAT
  }

  test("LOG (100) keeps its pre-existing flat cost of 10") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    hostCalls.getGasCost(HostCall.LOG, instance) shouldBe 10L
  }
