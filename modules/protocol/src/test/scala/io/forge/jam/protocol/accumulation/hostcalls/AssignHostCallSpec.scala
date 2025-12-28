package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for ASSIGN (15) host call.
 * Sets core assigner and auth queue.
 */
class AssignHostCallSpec extends HostCallTestBase:

  test("ASSIGN: returns HUH when caller is not current assigner") {
    val context = createTestContext()
    // Set up assigners for core 0 to a different service
    context.x.assigners += 999L // Core 0 assigned to service 999, not 100

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Prepare auth queue
    val authQueueAddr = 0x10000
    val authQueueBytes = new Array[Byte](32 * testConfig.authQueueSize)
    instance.writeBytes(authQueueAddr, authQueueBytes)

    instance.setReg(7, 0) // core index
    instance.setReg(8, authQueueAddr)
    instance.setReg(9, 42L) // new assigner

    hostCalls.dispatch(HostCall.ASSIGN, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("ASSIGN: returns CORE when core index >= coresCount") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val authQueueAddr = 0x10000
    val authQueueBytes = new Array[Byte](32 * testConfig.authQueueSize)
    instance.writeBytes(authQueueAddr, authQueueBytes)

    instance.setReg(7, testConfig.coresCount + 10) // Invalid core index
    instance.setReg(8, authQueueAddr)
    instance.setReg(9, 42L)

    hostCalls.dispatch(HostCall.ASSIGN, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.CORE
  }

  test("ASSIGN: returns WHO when new assigner exceeds UInt32 max") {
    val context = createTestContext()
    // Make caller the current assigner for core 0
    context.x.assigners += context.serviceIndex

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val authQueueAddr = 0x10000
    val authQueueBytes = new Array[Byte](32 * testConfig.authQueueSize)
    instance.writeBytes(authQueueAddr, authQueueBytes)

    instance.setReg(7, 0) // Core 0
    instance.setReg(8, authQueueAddr)
    instance.setReg(9, 0xffffffffL + 1L) // Exceeds UInt32 max

    hostCalls.dispatch(HostCall.ASSIGN, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }

  test("ASSIGN: returns OK on successful assignment") {
    val context = createTestContext()
    // Make caller the current assigner for core 0
    context.x.assigners += context.serviceIndex

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val authQueueAddr = 0x10000
    val authQueueBytes = new Array[Byte](32 * testConfig.authQueueSize)
    // Fill with some test hashes
    for i <- 0 until testConfig.authQueueSize do
      val hash = Array.fill[Byte](32)((i % 256).toByte)
      System.arraycopy(hash, 0, authQueueBytes, i * 32, 32)
    instance.writeBytes(authQueueAddr, authQueueBytes)

    val newAssigner = 42L
    instance.setReg(7, 0) // Core 0
    instance.setReg(8, authQueueAddr)
    instance.setReg(9, newAssigner)

    hostCalls.dispatch(HostCall.ASSIGN, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    context.x.assigners(0) shouldBe newAssigner
    context.x.authQueue(0).size shouldBe testConfig.authQueueSize
  }
