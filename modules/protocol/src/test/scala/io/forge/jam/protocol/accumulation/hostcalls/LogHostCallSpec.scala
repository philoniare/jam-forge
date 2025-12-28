package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for LOG (100) host call.
 * Debug logging (returns WHAT).
 */
class LogHostCallSpec extends HostCallTestBase:

  test("LOG: returns WHAT and costs 10 gas") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    hostCalls.dispatch(HostCall.LOG, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHAT

    // Gas cost should be 10
    hostCalls.getGasCost(HostCall.LOG, instance) shouldBe 10L
  }
