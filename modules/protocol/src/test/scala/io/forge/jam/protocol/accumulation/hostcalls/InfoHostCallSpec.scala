package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for INFO (5) host call.
 * Gets service account info.
 */
class InfoHostCallSpec extends HostCallTestBase:

  test("INFO: returns NONE for non-existent service") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(7, 999L) // non-existent service
    instance.setReg(8, 0x20000) // output address
    instance.setReg(9, 0) // offset
    instance.setReg(10, 96) // length

    hostCalls.dispatch(HostCall.INFO, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.NONE
  }

  test("INFO: returns 96 bytes of service info for existing service") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val outputAddr = 0x20000
    instance.setReg(7, -1L) // current service
    instance.setReg(8, outputAddr)
    instance.setReg(9, 0) // offset
    instance.setReg(10, 96) // length

    hostCalls.dispatch(HostCall.INFO, instance)

    // Should return 96 bytes
    instance.reg(7) shouldBe 96L
  }

  test("INFO: length >= 2^31 writes the full 96 bytes, not an empty slice (ACC-002)") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val outputAddr = 0x20000

    // Baseline: a known-good length=96 write.
    val good = createMockInstance()
    good.setReg(7, -1L); good.setReg(8, outputAddr); good.setReg(9, 0); good.setReg(10, 96)
    hostCalls.dispatch(HostCall.INFO, good)
    val baseline = good.readBytes(outputAddr, 96)
    baseline shouldBe defined

    val overflow = createMockInstance()
    overflow.setReg(7, -1L); overflow.setReg(8, outputAddr); overflow.setReg(9, 0); overflow.setReg(10, 0x80000000L)
    hostCalls.dispatch(HostCall.INFO, overflow)
    overflow.reg(7) shouldBe 96L
    overflow.readBytes(outputAddr, 96).get shouldBe baseline.get
  }
