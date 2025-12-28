package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

import scala.collection.mutable

/**
 * Tests for UPGRADE (19) host call.
 * Upgrades service code hash.
 */
class UpgradeHostCallSpec extends HostCallTestBase:

  test("UPGRADE: updates service code hash successfully") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val newCodeHash = Array.fill[Byte](32)(0xcd.toByte)
    val codeHashAddr = 0x10000
    instance.writeBytes(codeHashAddr, newCodeHash)

    instance.setReg(7, codeHashAddr)
    instance.setReg(8, 200L) // new minAccumulateGas
    instance.setReg(9, 100L) // new minMemoGas

    hostCalls.dispatch(HostCall.UPGRADE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    val account = context.x.accounts(100L)
    account.info.codeHash.bytes.toArray shouldBe newCodeHash
    account.info.minItemGas shouldBe 200L
    account.info.minMemoGas shouldBe 100L
  }

  test("UPGRADE: returns WHO when current service account not found") {
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

    val newCodeHash = Array.fill[Byte](32)(0xcd.toByte)
    val codeHashAddr = 0x10000
    instance.writeBytes(codeHashAddr, newCodeHash)

    instance.setReg(7, codeHashAddr)
    instance.setReg(8, 200L)
    instance.setReg(9, 100L)

    hostCalls.dispatch(HostCall.UPGRADE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }
