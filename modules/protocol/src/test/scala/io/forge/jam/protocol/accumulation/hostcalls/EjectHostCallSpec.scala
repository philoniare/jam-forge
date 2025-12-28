package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

import scala.collection.mutable

/**
 * Tests for EJECT (21) host call.
 * Removes service accounts.
 */
class EjectHostCallSpec extends HostCallTestBase:

  test("EJECT: returns WHO when trying to eject self") {
    val context = createTestContext(serviceIndex = 100L)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val preimageHash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, preimageHash)

    instance.setReg(7, 100L) // Same as caller's service ID
    instance.setReg(8, hashAddr)

    hostCalls.dispatch(HostCall.EJECT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }

  test("EJECT: returns WHO when target service does not exist") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val preimageHash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, preimageHash)

    instance.setReg(7, 999L) // Non-existent service
    instance.setReg(8, hashAddr)

    hostCalls.dispatch(HostCall.EJECT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }

  test("EJECT: returns WHO when target code hash doesn't match caller service ID") {
    val context = createTestContext(serviceIndex = 100L)
    // Add target service with different code hash
    val targetService = createTestAccount(1000L)
    context.x.accounts(200L) = targetService // Code hash is zeros, not 100L encoded

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val preimageHash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, preimageHash)

    instance.setReg(7, 200L)
    instance.setReg(8, hashAddr)

    hostCalls.dispatch(HostCall.EJECT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }

  test("EJECT: returns HUH when target has items != 2") {
    val context = createTestContext(serviceIndex = 100L)
    // Create target with matching code hash (100L encoded as hash) but wrong items count
    val encodedServiceId = {
      val buffer = java.nio.ByteBuffer.allocate(32).order(java.nio.ByteOrder.LITTLE_ENDIAN)
      buffer.putInt(100) // Encode caller service ID
      JamBytes(buffer.array())
    }
    val targetInfo = createTestServiceInfo(1000L).copy(
      codeHash = Hash(encodedServiceId.toArray),
      items = 5 // Wrong: should be 2
    )
    val targetService = ServiceAccount(
      info = targetInfo,
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0L
    )
    context.x.accounts(200L) = targetService

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val preimageHash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, preimageHash)

    instance.setReg(7, 200L)
    instance.setReg(8, hashAddr)

    hostCalls.dispatch(HostCall.EJECT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("EJECT: returns HUH when preimage request not found") {
    val context = createTestContext(serviceIndex = 100L)
    // Create target with matching code hash and items = 2, but no preimage request
    val encodedServiceId = {
      val buffer = java.nio.ByteBuffer.allocate(32).order(java.nio.ByteOrder.LITTLE_ENDIAN)
      buffer.putInt(100)
      JamBytes(buffer.array())
    }
    val targetInfo = createTestServiceInfo(1000L).copy(
      codeHash = Hash(encodedServiceId.toArray),
      items = 2
    )
    val targetService = ServiceAccount(
      info = targetInfo,
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty, // No preimage requests!
      lastAccumulated = 0L
    )
    context.x.accounts(200L) = targetService

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val preimageHash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, preimageHash)

    instance.setReg(7, 200L)
    instance.setReg(8, hashAddr)

    hostCalls.dispatch(HostCall.EJECT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("EJECT: returns HUH when hold period not expired") {
    // Create context with timeslot
    // TINY config has preimageExpungePeriod = 32
    val state = PartialState(
      accounts = mutable.Map(100L -> createTestAccount(10000000L)),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )
    val context = AccumulationContext(state, 100L, 1000L, JamBytes.zeros(32))

    // Create target with matching code hash and items = 2
    val encodedServiceId = {
      val buffer = java.nio.ByteBuffer.allocate(32).order(java.nio.ByteOrder.LITTLE_ENDIAN)
      buffer.putInt(100)
      JamBytes(buffer.array())
    }
    val targetInfo = createTestServiceInfo(1000L).copy(
      codeHash = Hash(encodedServiceId.toArray),
      items = 2,
      bytesUsed = 81 // So derivedLength = max(81, 81) - 81 = 0
    )
    val targetService = ServiceAccount(
      info = targetInfo,
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0L
    )

    // Add preimage request with 2 timeslots but NOT expired
    // TINY: preimageExpungePeriod = 32
    // context.timeslot = 1000
    // minHoldSlot = max(0, 1000 - 32) = 968
    // timeslots(1) = 999 >= 968, so NOT expired (should return HUH)
    val preimageHash = Array.fill[Byte](32)(0x42.toByte)
    val key = PreimageKey(Hash(preimageHash), 0)
    targetService.preimageRequests(key) = PreimageRequest(List(900L, 999L)) // 2 timeslots, not expired

    context.x.accounts(200L) = targetService

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, preimageHash)

    instance.setReg(7, 200L)
    instance.setReg(8, hashAddr)

    hostCalls.dispatch(HostCall.EJECT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("EJECT: returns OK on successful ejection") {
    // Create context with high timeslot for expiration
    val state = PartialState(
      accounts = mutable.Map(100L -> createTestAccount(10000000L)),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )
    val context = AccumulationContext(state, 100L, 100000L, JamBytes.zeros(32))

    // Create target with matching code hash and items = 2
    val encodedServiceId = {
      val buffer = java.nio.ByteBuffer.allocate(32).order(java.nio.ByteOrder.LITTLE_ENDIAN)
      buffer.putInt(100)
      JamBytes(buffer.array())
    }
    val targetBalance = 5000L
    val targetInfo = createTestServiceInfo(targetBalance).copy(
      codeHash = Hash(encodedServiceId.toArray),
      items = 2,
      bytesUsed = 81
    )
    val targetService = ServiceAccount(
      info = targetInfo,
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0L
    )

    // Add preimage request with 2 timeslots that IS expired
    // minHoldSlot = max(0, 100000 - 28800) = 71200
    // timeslots(1) = 1000 < 71200, so expired
    val preimageHash = Array.fill[Byte](32)(0x42.toByte)
    val key = PreimageKey(Hash(preimageHash), 0)
    targetService.preimageRequests(key) = PreimageRequest(List(500L, 1000L))

    context.x.accounts(200L) = targetService

    val callerBalanceBefore = context.x.accounts(100L).info.balance

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, preimageHash)

    instance.setReg(7, 200L)
    instance.setReg(8, hashAddr)

    hostCalls.dispatch(HostCall.EJECT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // Verify target was removed
    context.x.accounts.contains(200L) shouldBe false

    // Verify balance was transferred to caller
    context.x.accounts(100L).info.balance shouldBe (callerBalanceBefore + targetBalance)
  }
