package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

import scala.collection.mutable

/**
 * Tests for FORGET (24) host call.
 * Forgets preimage requests.
 */
class ForgetHostCallSpec extends HostCallTestBase:

  test("FORGET: removes preimage request with empty history") {
    val context = createTestContext(balance = 10000000L)

    // First solicit
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val key = PreimageKey(Hash(hash), 100)
    context.x.accounts(100L).preimageRequests(key) = PreimageRequest(List.empty)

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.FORGET, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // Verify request was removed
    context.x.accounts(100L).preimageRequests.contains(key) shouldBe false
  }

  test("FORGET: returns HUH when request not found") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.FORGET, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("FORGET: returns HUH when can't forget (count == 3 but not expired)") {
    val context = createTestContext(balance = 10000000L)

    // Add request with count == 3 but recent timestamp (not expired)
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val key = PreimageKey(Hash(hash), 100)
    // timeslots[1] must be < (context.timeslot - expungePeriod) to be expired
    // context.timeslot = 1000, expungePeriod = 28800 (TINY), so minHoldSlot = max(0, 1000-28800) = 0
    // Set timeslots[1] = 999 which is >= 0, so NOT expired
    context.x.accounts(100L).preimageRequests(key) = PreimageRequest(List(500L, 999L, 600L)) // count == 3

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.FORGET, instance)

    // With minHoldSlot = 0 and timeslots[1] = 999 >= 0, isAvailable3 is false
    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("FORGET: returns OK for available1 case (count == 1)") {
    val context = createTestContext(balance = 10000000L)

    // Add request with count == 1 (available)
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val key = PreimageKey(Hash(hash), 100)
    context.x.accounts(100L).preimageRequests(key) = PreimageRequest(List(500L)) // count == 1

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.FORGET, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // Verify timeslot was appended
    val updatedRequest = context.x.accounts(100L).preimageRequests.get(key)
    updatedRequest shouldBe defined
    updatedRequest.get.requestedAt.size shouldBe 2
    updatedRequest.get.requestedAt(1) shouldBe context.timeslot
  }

  test("FORGET: returns OK for expunge case (count == 2 and expired)") {
    // Create context with high timeslot so expunge period is satisfied
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
    val context = AccumulationContext(state, 100L, 100000L, JamBytes.zeros(32)) // High timeslot

    // Add request with count == 2 and old timestamp (expired)
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val key = PreimageKey(Hash(hash), 100)
    // minHoldSlot = max(0, 100000 - 28800) = 71200
    // timeslots(1) = 1000 < 71200, so expired
    context.x.accounts(100L).preimageRequests(key) = PreimageRequest(List(500L, 1000L)) // count == 2, expired

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.FORGET, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // Verify request was removed (expunged)
    context.x.accounts(100L).preimageRequests.contains(key) shouldBe false
  }

  test("FORGET: returns OK for available3 case (count == 3 and expired)") {
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

    // Add request with count == 3 and expired (timeslots(1) < minHoldSlot)
    // minHoldSlot = max(0, 100000 - 28800) = 71200
    // timeslots(1) = 1000 < 71200, so expired
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val key = PreimageKey(Hash(hash), 100)
    context.x.accounts(100L).preimageRequests(key) = PreimageRequest(List(500L, 1000L, 2000L)) // count == 3, expired

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.FORGET, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // Verify timeslots were updated to [t2, now] per GP available3 case
    val updatedRequest = context.x.accounts(100L).preimageRequests.get(key)
    updatedRequest shouldBe defined
    updatedRequest.get.requestedAt.size shouldBe 2
    updatedRequest.get.requestedAt(0) shouldBe 2000L // Original t2
    updatedRequest.get.requestedAt(1) shouldBe context.timeslot // Updated to now
  }

  test("FORGET: returns WHO when current service account not found") {
    // Create context where serviceIndex doesn't exist in accounts
    val state = PartialState(
      accounts = mutable.Map.empty, // No accounts!
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )
    val context = AccumulationContext(state, 100L, 1000L, JamBytes.zeros(32))
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.FORGET, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }
