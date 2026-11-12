package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
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
    context.yieldHash = Some(JamBytes(Array[Byte](1, 2, 3)))
    context.deferredTransfers += DeferredTransfer(100L, 200L, 500L, JamBytes.zeros(128), 1000L)

    hostCalls.dispatch(HostCall.CHECKPOINT, instance)

    instance.reg(7) shouldBe currentGas
    context.y.manager shouldBe 42L
    context.yieldCheckpoint shouldBe Some(JamBytes(Array[Byte](1, 2, 3)))
    context.deferredTransfersCheckpoint.map(_.source) shouldBe Seq(100L)

    // y is a copy, not an alias of x.
    context.x.manager = 200L
    context.y.manager shouldBe 42L
  }
