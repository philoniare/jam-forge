package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._

/**
 * Tests for CHECKPOINT (17) host call.
 * Saves state checkpoint.
 */
class CheckpointHostCallSpec extends HostCallTestBase:

  test("CHECKPOINT: saves state and returns remaining gas") {
    val context = createTestContext()
    val currentGas = 12345L
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance(currentGas)

    context.x.manager = 42L

    hostCalls.dispatch(HostCall.CHECKPOINT, instance)

    instance.reg(7) shouldBe currentGas
    context.y.manager shouldBe 42L
  }
