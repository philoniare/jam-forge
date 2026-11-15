package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._
import io.forge.jam.protocol.pvmhost.*
import io.forge.jam.protocol.state.*

/**
 * Tests for GAS (0) host call.
 * Returns remaining gas in r7.
 */
class GasHostCallSpec extends HostCallTestBase:

  test("GAS: returns remaining gas in r7") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance(75000L)

    hostCalls.dispatch(HostCall.GAS, instance)

    instance.reg(7) shouldBe 75000L
  }
