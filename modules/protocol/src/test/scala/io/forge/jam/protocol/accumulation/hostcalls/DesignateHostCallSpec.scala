package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for DESIGNATE (16) host call.
 * Sets validator queue.
 */
class DesignateHostCallSpec extends HostCallTestBase:

  test("DESIGNATE: returns HUH when caller is not delegator (z valid)") {
    val context = createTestContext()
    context.x.delegator = 999L // Different from current service (100)

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val z = testConfig.validatorCount // 6, a valid valcount member
    val validatorKeysAddr = 0x10000
    val validatorKeysBytes = new Array[Byte](336 * z)
    instance.writeBytes(validatorKeysAddr, validatorKeysBytes)

    instance.setReg(7, validatorKeysAddr)
    instance.setReg(8, z)

    hostCalls.dispatch(HostCall.DESIGNATE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("DESIGNATE: returns OK when caller is delegator (z = 3*coresCount = 6, valcount upper boundary)") {
    val context = createTestContext()
    context.x.delegator = context.serviceIndex // Caller is delegator

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val z = 3 * testConfig.coresCount // 6 for TINY (== config.validatorCount)
    val validatorKeysAddr = 0x10000
    val validatorKeysBytes = new Array[Byte](336 * z)
    instance.writeBytes(validatorKeysAddr, validatorKeysBytes)

    instance.setReg(7, validatorKeysAddr)
    instance.setReg(8, z)

    hostCalls.dispatch(HostCall.DESIGNATE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    context.x.stagingSet.size shouldBe z
  }
  for z <- Seq(5, 7, 3 * (2 /* TINY coresCount */ + 1), 3 * (2 + 1) + 3) do
    test(s"DESIGNATE: returns HUH for z=$z (not in valcount) even as delegator") {
      val context = createTestContext()
      context.x.delegator = context.serviceIndex

      val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
      val instance = createMockInstance()

      val validatorKeysAddr = 0x10000
      val validatorKeysBytes = new Array[Byte](336 * z)
      instance.writeBytes(validatorKeysAddr, validatorKeysBytes)

      instance.setReg(7, validatorKeysAddr)
      instance.setReg(8, z)

      hostCalls.dispatch(HostCall.DESIGNATE, instance)

      ULong(instance.reg(7)) shouldBe HostCallResult.HUH
      context.x.stagingSet shouldBe empty
    }
