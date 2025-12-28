package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for gas cost consistency across all host calls.
 * Also tests unknown host call handling.
 */
class HostCallGasCostSpec extends HostCallTestBase:

  test("all host calls have gas cost of 10") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hostCallIds = List(
      HostCall.GAS,
      HostCall.FETCH,
      HostCall.LOOKUP,
      HostCall.READ,
      HostCall.WRITE,
      HostCall.INFO,
      HostCall.BLESS,
      HostCall.ASSIGN,
      HostCall.DESIGNATE,
      HostCall.CHECKPOINT,
      HostCall.NEW,
      HostCall.UPGRADE,
      HostCall.TRANSFER,
      HostCall.EJECT,
      HostCall.QUERY,
      HostCall.SOLICIT,
      HostCall.FORGET,
      HostCall.YIELD,
      HostCall.PROVIDE,
      HostCall.LOG
    )

    for id <- hostCallIds do
      hostCalls.getGasCost(id, instance) shouldBe 10L
  }

  test("unknown host call returns WHAT") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    hostCalls.dispatch(9999, instance) // Unknown host call ID

    ULong(instance.reg(7)) shouldBe HostCallResult.WHAT
  }
