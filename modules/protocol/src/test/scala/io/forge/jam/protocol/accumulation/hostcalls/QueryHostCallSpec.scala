package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

import scala.collection.mutable

/**
 * Tests for QUERY (22) host call.
 * Queries preimage request status.
 */
class QueryHostCallSpec extends HostCallTestBase:

  test("QUERY: returns NONE when preimage not requested") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.QUERY, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.NONE
  }

  test("QUERY: returns packed count and timeslots on success") {
    val context = createTestContext()

    // Add preimage request with timeslots
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val key = PreimageKey(Hash(hash), 100)
    context.x.accounts(100L).preimageRequests(key) = PreimageRequest(List(500L, 600L))

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.QUERY, instance)

    // Should return packed value (not NONE)
    val result = ULong(instance.reg(7))
    result should not be HostCallResult.NONE
  }

  test("QUERY: returns WHO when current service account not found") {
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

    hostCalls.dispatch(HostCall.QUERY, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }
