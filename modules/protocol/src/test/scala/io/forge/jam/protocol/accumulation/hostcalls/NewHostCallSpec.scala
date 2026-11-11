package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for NEW (18) host call.
 * Creates new service accounts.
 */
class NewHostCallSpec extends HostCallTestBase:

  test("NEW: creates service account with correct parentService") {
    val context = createTestContext(balance = 10000000L)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val codeHash = Array.fill[Byte](32)(0xab.toByte)
    val codeHashAddr = 0x10000
    instance.writeBytes(codeHashAddr, codeHash)

    instance.setReg(7, codeHashAddr)
    instance.setReg(8, 1000) // code length
    instance.setReg(9, 100L) // minAccumulateGas
    instance.setReg(10, 50L) // minMemoGas
    instance.setReg(11, 0L) // gratisStorage
    instance.setReg(12, 0L) // requestedServiceId

    hostCalls.dispatch(HostCall.NEW, instance)

    val newServiceId = instance.reg(7)
    newServiceId should be >= testConfig.minPublicServiceIndex

    val newAccount = context.x.accounts(newServiceId)
    newAccount.info.parentService shouldBe 100L // Creator's service ID
    newAccount.info.minItemGas shouldBe 100L
    newAccount.info.minMemoGas shouldBe 50L
  }

  test("NEW: returns CASH when balance insufficient") {
    val context = createTestContext(balance = 100L) // Low balance
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val codeHash = Array.fill[Byte](32)(0xab.toByte)
    val codeHashAddr = 0x10000
    instance.writeBytes(codeHashAddr, codeHash)

    instance.setReg(7, codeHashAddr)
    instance.setReg(8, 1000)
    instance.setReg(9, 100L)
    instance.setReg(10, 50L)
    instance.setReg(11, 0L)
    instance.setReg(12, 0L)

    hostCalls.dispatch(HostCall.NEW, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.CASH
  }

  test("NEW: returns HUH when non-manager tries to set gratisStorage != 0") {
    val context = createTestContext(balance = 10000000L)
    context.x.manager = 999L // Caller (100L) is NOT the manager

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val codeHashAddr = 0x10000
    val codeHash = Array.fill[Byte](32)(0xab.toByte)
    instance.writeBytes(codeHashAddr, codeHash)

    instance.setReg(7, codeHashAddr)
    instance.setReg(8, 100) // codeHashLength
    instance.setReg(9, 1000L) // minAccumulateGas
    instance.setReg(10, 100L) // minMemoGas
    instance.setReg(11, 500L) // gratisStorage != 0
    instance.setReg(12, 0L) // requestedServiceId

    hostCalls.dispatch(HostCall.NEW, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("NEW: returns FULL when registrar requests existing service ID") {
    val context = createTestContext(balance = 10000000L)
    context.x.registrar = 100L // Caller IS the registrar
    // Add a service at ID 5 (below minPublicServiceIndex)
    context.x.accounts = context.x.accounts.updated(5L, createTestAccount(1000L))

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val codeHashAddr = 0x10000
    val codeHash = Array.fill[Byte](32)(0xab.toByte)
    instance.writeBytes(codeHashAddr, codeHash)

    instance.setReg(7, codeHashAddr)
    instance.setReg(8, 100) // codeHashLength
    instance.setReg(9, 1000L) // minAccumulateGas
    instance.setReg(10, 100L) // minMemoGas
    instance.setReg(11, 0L) // gratisStorage = 0
    instance.setReg(12, 5L) // Request service ID 5 which already exists

    hostCalls.dispatch(HostCall.NEW, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.FULL
  }

  // Omega_N: l is any value in N_{2^32}; a.octets = 81 + l and
  // a.minbalance = B_S + 2*B_I + (81 + l)*B_L - gratis. With the test caller
  // (items = 5, octets = 100, gratis = 0) its own threshold is 100 + 50 + 100 = 250.
  private val callerThreshold = 250L
  private def newThreshold(l: Long): Long = 100L + 2L * 10L + (81L + l)

  private def runNew(balance: Long, length: Long): (AccumulationContext, MockPvmInstance, Array[Byte]) =
    val context = createTestContext(balance = balance)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()
    val codeHash = Array.fill[Byte](32)(0xcd.toByte)
    val codeHashAddr = 0x10000
    instance.writeBytes(codeHashAddr, codeHash)
    instance.setReg(7, codeHashAddr)
    instance.setReg(8, length)
    instance.setReg(9, 100L)
    instance.setReg(10, 50L)
    instance.setReg(11, 0L)
    instance.setReg(12, 0L)
    hostCalls.dispatch(HostCall.NEW, instance)
    (context, instance, codeHash)

  private def assertCreated(length: Long, balance: Long): Unit =
    val (context, instance, codeHash) = runNew(balance, length)
    val newServiceId = instance.reg(7)
    newServiceId should be >= testConfig.minPublicServiceIndex
    val created = context.x.accounts(newServiceId)
    created.info.bytesUsed shouldBe 81L + length
    created.info.items shouldBe 2
    created.info.balance shouldBe newThreshold(length)
    created.preimageRequests.keySet shouldBe Set(
      PreimageKey(io.forge.jam.core.primitives.Hash(codeHash), length)
    )
    context.x.accounts(100L).info.balance shouldBe balance - newThreshold(length)

  test("NEW: length 2^31 with sufficient balance returns OK with footprint 81 + 2^31") {
    val l = 1L << 31
    assertCreated(l, newThreshold(l) + callerThreshold)
  }

  test("NEW: length 2^32 - 1 with sufficient balance returns OK with footprint 81 + l") {
    val l = 0xffffffffL
    assertCreated(l, newThreshold(l) + callerThreshold)
  }

  test("NEW: length 2^31 with balance one short returns CASH and creates nothing") {
    val l = 1L << 31
    val balance = newThreshold(l) + callerThreshold - 1L
    val (context, instance, _) = runNew(balance, l)
    ULong(instance.reg(7)) shouldBe HostCallResult.CASH
    context.x.accounts.keySet shouldBe Set(100L)
    context.x.accounts(100L).info.balance shouldBe balance
  }

  test("NEW: small length at the exact balance boundary is unchanged (OK, then CASH one below)") {
    val l = 1000L
    assertCreated(l, newThreshold(l) + callerThreshold)
    val (_, instance, _) = runNew(newThreshold(l) + callerThreshold - 1L, l)
    ULong(instance.reg(7)) shouldBe HostCallResult.CASH
  }
