package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for BLESS (14) host call.
 * Sets privileged services.
 */
class BlessHostCallSpec extends HostCallTestBase:

  test("BLESS: updates privileges successfully") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Prepare assigners array (4 bytes per core)
    val assignersAddr = 0x10000
    val assignersBytes = new Array[Byte](4 * testConfig.coresCount)
    // Set each assigner to service ID 42
    for i <- 0 until testConfig.coresCount do
      val offset = i * 4
      assignersBytes(offset) = 42.toByte
    instance.writeBytes(assignersAddr, assignersBytes)

    // No always-acc pairs for simplicity
    val alwaysAccAddr = 0x20000

    instance.setReg(7, 1L) // new manager
    instance.setReg(8, assignersAddr)
    instance.setReg(9, 2L) // new delegator
    instance.setReg(10, 3L) // new registrar
    instance.setReg(11, alwaysAccAddr)
    instance.setReg(12, 0) // zero always-acc pairs

    hostCalls.dispatch(HostCall.BLESS, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    context.x.manager shouldBe 1L
    context.x.delegator shouldBe 2L
    context.x.registrar shouldBe 3L
  }

  test("BLESS: returns WHO when manager exceeds UInt32 max") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val assignersAddr = 0x10000
    val assignersBytes = new Array[Byte](4 * testConfig.coresCount)
    instance.writeBytes(assignersAddr, assignersBytes)

    instance.setReg(7, 0xffffffffL + 1L) // Invalid manager (exceeds UInt32)
    instance.setReg(8, assignersAddr)
    instance.setReg(9, 0L) // delegator
    instance.setReg(10, 0L) // registrar
    instance.setReg(11, 0) // alwaysAccPtr
    instance.setReg(12, 0) // alwaysAccCount

    hostCalls.dispatch(HostCall.BLESS, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }

  test("BLESS: returns WHO when delegator exceeds UInt32 max") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val assignersAddr = 0x10000
    val assignersBytes = new Array[Byte](4 * testConfig.coresCount)
    instance.writeBytes(assignersAddr, assignersBytes)

    instance.setReg(7, 0L) // manager
    instance.setReg(8, assignersAddr)
    instance.setReg(9, 0xffffffffL + 1L) // Invalid delegator (exceeds UInt32)
    instance.setReg(10, 0L) // registrar
    instance.setReg(11, 0) // alwaysAccPtr
    instance.setReg(12, 0) // alwaysAccCount

    hostCalls.dispatch(HostCall.BLESS, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }
