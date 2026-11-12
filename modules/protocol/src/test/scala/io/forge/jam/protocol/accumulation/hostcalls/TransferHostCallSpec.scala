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

  private val sourceId = 100L
  private val destId = 200L
  private val memoAddr = 0x10000

  /** Source with `sourceBalance`, destination 200 with `destMinMemoGas`. */
  private def twoServiceContext(sourceBalance: Long = 10000L, destMinMemoGas: Long = 50L): AccumulationContext =
    val destAccount = ServiceAccount(
      info = createTestServiceInfo(1000L).copy(minMemoGas = destMinMemoGas),
      storage = Map.empty,
      preimages = Map.empty,
      preimageRequests = Map.empty,
      lastAccumulated = 0L
    )
    val state = PartialState(
      accounts = Map(sourceId -> createTestAccount(sourceBalance), destId -> destAccount),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = Vector.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )
    AccumulationContext(state, sourceId, 1000L, JamBytes.zeros(32))

  private def setUpTransfer(instance: MockPvmInstance, dest: Long, amount: Long, gasLimit: Long, memo: Array[Byte] = new Array[Byte](128)): Unit =
    instance.writeBytes(memoAddr, memo)
    instance.setReg(7, dest)
    instance.setReg(8, amount)
    instance.setReg(9, gasLimit)
    instance.setReg(10, memoAddr)

  test("TRANSFER: OK queues the deferred transfer and debits the source") {
    val context = twoServiceContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()
    setUpTransfer(instance, destId, amount = 500L, gasLimit = 100L, memo = Array.tabulate[Byte](128)(_.toByte))

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    context.deferredTransfers.size shouldBe 1
    val transfer = context.deferredTransfers.head
    transfer.source shouldBe sourceId
    transfer.destination shouldBe destId
    transfer.amount shouldBe 500L
    transfer.gasLimit shouldBe 100L
    context.x.accounts(sourceId).info.balance shouldBe (10000L - 500L)
  }

  test("TRANSFER: result code and net gas per outcome (OK charges l, WHO/LOW/CASH do not)") {
    val initialGas = 9999L

    def runTransfer(dest: Long, amount: Long, gasLimit: Long): (ULong, Long) =
      val hostCalls = new AccumulationHostCalls(twoServiceContext(), List.empty, testConfig)
      val instance = createMockInstance(initialGas)
      setUpTransfer(instance, dest, amount, gasLimit)
      val preCharge = hostCalls.getGasCost(HostCall.TRANSFER, instance)
      instance.setGas(instance.gas - preCharge)
      hostCalls.dispatch(HostCall.TRANSFER, instance)
      (ULong(instance.reg(7)), initialGas - instance.gas)

    runTransfer(destId, amount = 100L, gasLimit = 50L) shouldBe (HostCallResult.OK, 575L + 50L)
    runTransfer(999L, amount = 100L, gasLimit = 50L) shouldBe (HostCallResult.WHO, 575L)
    runTransfer(destId, amount = 100L, gasLimit = 5L) shouldBe (HostCallResult.LOW, 575L)
    runTransfer(destId, amount = 999999999L, gasLimit = 50L) shouldBe (HostCallResult.CASH, 575L)
  }

  test("TRANSFER: gasLimit exceeding remaining gas forces OOG with FULL gas deducted (C1)") {
    val context = twoServiceContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val initialGas = 1000L
    val instance = createMockInstance(gas = initialGas)
    setUpTransfer(instance, destId, amount = 500L, gasLimit = 2_000_000L) // l >> remaining gas

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    instance.isForcedOutOfGas shouldBe true
    // r7 is untouched (still destId), not overwritten with a result code.
    instance.reg(7) shouldBe destId
    context.deferredTransfers shouldBe empty
    context.x.accounts(sourceId).info.balance shouldBe 10000L
    instance.gas shouldBe (initialGas - 2_000_000L)
  }

  test("TRANSFER: minMemoGas >= 2^63 no longer flips the LOW comparison (ACC-002)") {
    val context = twoServiceContext(destMinMemoGas = Long.MinValue)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()
    setUpTransfer(instance, destId, amount = 100L, gasLimit = 5000L)

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.LOW
    context.deferredTransfers shouldBe empty
  }
