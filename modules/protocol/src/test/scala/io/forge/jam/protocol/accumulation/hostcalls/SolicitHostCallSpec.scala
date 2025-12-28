package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

import scala.collection.mutable

/**
 * Tests for SOLICIT (23) host call.
 * Requests a preimage.
 */
class SolicitHostCallSpec extends HostCallTestBase:

  test("SOLICIT: creates preimage request and returns OK") {
    val context = createTestContext(balance = 10000000L)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.SOLICIT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // Verify request was created
    val key = PreimageKey(Hash(hash), 100)
    context.x.accounts(100L).preimageRequests.contains(key) shouldBe true
  }

  test("SOLICIT: returns HUH when already in invalid state") {
    val context = createTestContext(balance = 10000000L)

    // Add existing request with count == 1 (available, can't re-solicit)
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val key = PreimageKey(Hash(hash), 100)
    context.x.accounts(100L).preimageRequests(key) = PreimageRequest(List(500L)) // count == 1

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.SOLICIT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("SOLICIT: returns FULL when balance threshold exceeded") {
    val context = createTestContext(balance = 100L) // Very low balance
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100000) // Large length to exceed threshold

    hostCalls.dispatch(HostCall.SOLICIT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.FULL
  }

  test("SOLICIT: returns OK for re-solicit when count == 2 (previously available)") {
    val context = createTestContext(balance = 10000000L)

    // Add existing request with count == 2 (previously available, can re-solicit)
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val key = PreimageKey(Hash(hash), 100)
    context.x.accounts(100L).preimageRequests(key) = PreimageRequest(List(500L, 600L)) // count == 2

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.SOLICIT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
  }

  test("SOLICIT: returns WHO when current service account not found") {
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

    hostCalls.dispatch(HostCall.SOLICIT, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }
