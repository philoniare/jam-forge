package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

import scala.collection.mutable

/**
 * Tests for TRANSFER (20) host call.
 * Queues deferred transfers between services.
 */
class TransferHostCallSpec extends HostCallTestBase:

  test("TRANSFER: returns OK and queues deferred transfer on success") {
    // Create context with source and destination accounts
    val sourceId = 100L
    val destId = 200L

    val sourceAccount = createTestAccount(10000L)
    val destAccount = ServiceAccount(
      info = createTestServiceInfo(1000L).copy(minMemoGas = 50L),
      storage = Map.empty,
      preimages = Map.empty,
      preimageRequests = Map.empty,
      lastAccumulated = 0L
    )

    val state = PartialState(
      accounts = Map(sourceId -> sourceAccount, destId -> destAccount),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )
    val context = AccumulationContext(state, sourceId, 1000L, JamBytes.zeros(32))
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val memoAddr = 0x10000
    val memo = new Array[Byte](128)
    instance.writeBytes(memoAddr, memo)

    instance.setReg(7, destId)
    instance.setReg(8, 500L) // amount
    instance.setReg(9, 100L) // gasLimit >= minMemoGas (50)
    instance.setReg(10, memoAddr)

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    context.deferredTransfers.size shouldBe 1
    context.deferredTransfers.head.amount shouldBe 500L
  }

  test("TRANSFER: returns CASH for insufficient balance") {
    val context = createTestContext(balance = 50L) // Low balance
    // Add destination
    context.x.accounts = context.x.accounts.updated(200L, ServiceAccount(
      info = createTestServiceInfo(1000L).copy(minMemoGas = 10L),
      storage = Map.empty,
      preimages = Map.empty,
      preimageRequests = Map.empty,
      lastAccumulated = 0L
    ))

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val memoAddr = 0x10000
    instance.writeBytes(memoAddr, new Array[Byte](128))

    instance.setReg(7, 200L)
    instance.setReg(8, 10000L) // amount > balance
    instance.setReg(9, 100L)
    instance.setReg(10, memoAddr)

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.CASH
  }

  test("TRANSFER: returns LOW for gasLimit < minMemoGas") {
    val sourceId = 100L
    val destId = 200L

    val sourceAccount = createTestAccount(10000L)
    val destAccount = ServiceAccount(
      info = createTestServiceInfo(1000L).copy(minMemoGas = 1000L), // High minMemoGas
      storage = Map.empty,
      preimages = Map.empty,
      preimageRequests = Map.empty,
      lastAccumulated = 0L
    )

    val state = PartialState(
      accounts = Map(sourceId -> sourceAccount, destId -> destAccount),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )
    val context = AccumulationContext(state, sourceId, 1000L, JamBytes.zeros(32))
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val memoAddr = 0x10000
    instance.writeBytes(memoAddr, new Array[Byte](128))

    instance.setReg(7, destId)
    instance.setReg(8, 100L)
    instance.setReg(9, 500L) // gasLimit < minMemoGas (1000)
    instance.setReg(10, memoAddr)

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.LOW
  }

  test("TRANSFER: returns WHO for non-existent destination") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val memoAddr = 0x10000
    instance.writeBytes(memoAddr, new Array[Byte](128))

    instance.setReg(7, 999L) // non-existent
    instance.setReg(8, 100L)
    instance.setReg(9, 50L)
    instance.setReg(10, memoAddr)

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }

  test("TRANSFER: gasLimit exceeding remaining gas forces OOG with FULL gas deducted (C1)") {
    val sourceId = 100L
    val destId = 200L

    val sourceAccount = createTestAccount(10000L)
    val destAccount = ServiceAccount(
      info = createTestServiceInfo(1000L).copy(minMemoGas = 50L),
      storage = Map.empty,
      preimages = Map.empty,
      preimageRequests = Map.empty,
      lastAccumulated = 0L
    )
    val state = PartialState(
      accounts = Map(sourceId -> sourceAccount, destId -> destAccount),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )
    val context = AccumulationContext(state, sourceId, 1000L, JamBytes.zeros(32))
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    // Remaining gas is far below the requested gasLimit (l).
    val initialGas = 1000L
    val instance = createMockInstance(gas = initialGas)

    val memoAddr = 0x10000
    instance.writeBytes(memoAddr, new Array[Byte](128))

    instance.setReg(7, destId)
    instance.setReg(8, 500L) // amount
    instance.setReg(9, 2_000_000L) // gasLimit (l) >> remaining gas (1000)
    instance.setReg(10, memoAddr)

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    instance.isForcedOutOfGas shouldBe true
    // r7 is untouched — still the destId the test wrote before dispatch —
    // NOT overwritten with OK or any other result code.
    instance.reg(7) shouldBe destId
    // No deferred transfer queued and no balance change.
    context.deferredTransfers shouldBe empty
    context.x.accounts(sourceId).info.balance shouldBe 10000L
    instance.gas shouldBe (initialGas - 2_000_000L)
    val gasUsed = if instance.gas >= 0 then initialGas - instance.gas else initialGas
    gasUsed shouldBe initialGas
  }

  test("TRANSFER: minMemoGas >= 2^63 no longer flips the LOW comparison (ACC-002)") {
    val sourceId = 100L
    val destId = 200L

    val sourceAccount = createTestAccount(10000L)
    val destAccount = ServiceAccount(
      info = createTestServiceInfo(1000L).copy(minMemoGas = Long.MinValue),
      storage = Map.empty,
      preimages = Map.empty,
      preimageRequests = Map.empty,
      lastAccumulated = 0L
    )
    val state = PartialState(
      accounts = Map(sourceId -> sourceAccount, destId -> destAccount),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )
    val context = AccumulationContext(state, sourceId, 1000L, JamBytes.zeros(32))
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val memoAddr = 0x10000
    instance.writeBytes(memoAddr, new Array[Byte](128))

    instance.setReg(7, destId)
    instance.setReg(8, 100L) // amount
    instance.setReg(9, 5000L) // gasLimit (l) — modest, but far below 2^63
    instance.setReg(10, memoAddr)

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.LOW
    context.deferredTransfers shouldBe empty
  }
