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
