package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for YIELD (25) host call.
 * Sets accumulation output hash.
 */
class YieldHostCallSpec extends HostCallTestBase:

  test("YIELD: sets accumulation output hash") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hash = Array.fill[Byte](32)(0xef.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)

    hostCalls.dispatch(HostCall.YIELD, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    context.yieldHash shouldBe Some(JamBytes(hash))
  }
