package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.HostCallPanic
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
    context.x.accounts = context.x.accounts.updated(
      100L,
      context.x.accounts(100L).copy(
        preimageRequests = context.x.accounts(100L).preimageRequests.updated(key, PreimageRequest(List(500L, 600L)))
      )
    )

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
      accounts = Map.empty, // No accounts!
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

  test("QUERY: returns HUH when z >= 2^32, even with an unreadable hash pointer") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // No hash written at hashAddr, and hashAddr itself is out of the mapped
    // memory range -- if the HUH-z guard did not run FIRST, this would panic.
    val hashAddr = 0x7fffffff
    instance.setReg(7, hashAddr)
    instance.setReg(8, 0x100000000L) // z = 2^32, not in bloblength

    noException should be thrownBy hostCalls.dispatch(HostCall.QUERY, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("StateKey: z = 0xFFFFFFFF aliases a preimage-info key onto a storage key") {
    val k = JamBytes(Array.tabulate[Byte](32)(i => (i + 1).toByte))
    val storageKey = StateKey.computeStorageStateKey(100L, k)
    val infoKey = StateKey.computePreimageInfoStateKey(100L, 0xffffffff.toInt, k)
    infoKey shouldBe storageKey
  }

  private def writeThenQueryWithAliasedZ(storedValue: Array[Byte]): Unit =
    val context = createTestContext(balance = 10000000L)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val k = Array.tabulate[Byte](32)(i => (i + 1).toByte)
    val keyAddr = 0x10000
    val valueAddr = 0x10100
    instance.writeBytes(keyAddr, k)
    instance.writeBytes(valueAddr, storedValue)

    // 1. WRITE k -> storedValue
    instance.setReg(7, keyAddr)
    instance.setReg(8, k.length)
    instance.setReg(9, valueAddr)
    instance.setReg(10, storedValue.length)
    hostCalls.dispatch(HostCall.WRITE, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.NONE

    // 2. QUERY the same bytes with the aliasing z
    instance.setReg(7, keyAddr)
    instance.setReg(8, 0xffffffffL)

    val ex = intercept[HostCallPanic] {
      hostCalls.dispatch(HostCall.QUERY, instance)
    }
    ex.getMessage should include("PANIC")

  test("QUERY: PANICs (not IllegalStateException) on a guest-written info value with count > 3") {
    // 0x10 -> compact integer 16, which fails the `count > 3` well-formedness check.
    writeThenQueryWithAliasedZ(Array[Byte](0x10))
  }

  test("QUERY: PANICs on a guest-written info value whose length contradicts its count") {
    // count = 2 demands 1 + 8 bytes; only 2 are present.
    writeThenQueryWithAliasedZ(Array[Byte](0x02, 0x00))
  }

  test("QUERY: PANICs on a guest-written info value with a truncated compact prefix") {
    // Multi-byte compact prefix with no continuation bytes: drives the decoder
    // off the end of the value rather than tripping either explicit check.
    writeThenQueryWithAliasedZ(Array[Byte](0x80.toByte))
  }
