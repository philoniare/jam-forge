package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for DESIGNATE (16) host call.
 * Sets validator queue.
 */
class DesignateHostCallSpec extends HostCallTestBase:

  test("DESIGNATE: returns HUH when caller is not delegator") {
    val context = createTestContext()
    context.x.delegator = 999L // Different from current service (100)

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val validatorKeysAddr = 0x10000
    val validatorKeysBytes = new Array[Byte](336 * testConfig.validatorCount)
    instance.writeBytes(validatorKeysAddr, validatorKeysBytes)

    instance.setReg(7, validatorKeysAddr)

    hostCalls.dispatch(HostCall.DESIGNATE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("DESIGNATE: returns OK when caller is delegator") {
    val context = createTestContext()
    context.x.delegator = context.serviceIndex // Caller is delegator

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val validatorKeysAddr = 0x10000
    val validatorKeysBytes = new Array[Byte](336 * testConfig.validatorCount)
    instance.writeBytes(validatorKeysAddr, validatorKeysBytes)

    instance.setReg(7, validatorKeysAddr)

    hostCalls.dispatch(HostCall.DESIGNATE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
  }
