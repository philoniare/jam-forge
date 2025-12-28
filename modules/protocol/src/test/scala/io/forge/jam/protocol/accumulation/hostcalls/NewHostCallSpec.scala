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
    context.x.accounts(5L) = createTestAccount(1000L)

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
