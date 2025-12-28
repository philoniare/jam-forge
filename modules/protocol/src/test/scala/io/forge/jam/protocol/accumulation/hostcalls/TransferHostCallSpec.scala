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
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0L
    )

    val state = PartialState(
      accounts = mutable.Map(sourceId -> sourceAccount, destId -> destAccount),
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
    context.x.accounts(200L) = ServiceAccount(
      info = createTestServiceInfo(1000L).copy(minMemoGas = 10L),
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0L
    )

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
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0L
    )

    val state = PartialState(
      accounts = mutable.Map(sourceId -> sourceAccount, destId -> destAccount),
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
