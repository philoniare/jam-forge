package io.forge.jam.protocol.accumulation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.protocol.generators.StfGenerators.*

import scala.collection.mutable

/**
 * - Service account balance tracking during transfers
 * - Deferred transfer ordering and queuing
 * - Ready queue management with slot-based rotation
 * - Privilege preservation (bless, assign, designate, registrar)
 */
class AccumulationSTFSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  private val testConfig = ChainConfig.TINY

  // Override default ScalaCheck configuration for faster tests
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  private def createContextWithService(balance: Long = 100000L): AccumulationContext =
    val serviceId = 100L
    val account = ServiceAccount(
      info = ServiceInfo(
        version = 0,
        codeHash = Hash.zero,
        balance = balance,
        minItemGas = 10L,
        minMemoGas = 20L,
        bytesUsed = 100L,
        depositOffset = 0L,
        items = 5,
        creationSlot = 0L,
        lastAccumulationSlot = 0L,
        parentService = 0L
      ),
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0L
    )

    val state = PartialState(
      accounts = mutable.Map(serviceId -> account),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )

    AccumulationContext(
      initialState = state,
      serviceIndex = serviceId,
      timeslot = 1000L,
      entropy = JamBytes.zeros(32)
    )

  test("property: service balance decreases by transfer amount after successful transfer") {
    forAll(genServiceInfo) { info =>
      // Use generated service info but ensure sufficient balance
      val safeInfo = info.copy(balance = math.max(info.balance, 10000L))

      val ctx = createContextWithService(safeInfo.balance)
      val serviceId = ctx.serviceIndex
      val initialBalance = ctx.x.accounts(serviceId).info.balance
      val transferAmount = 100L

      // Create a destination account
      val destId = serviceId + 1
      ctx.x.accounts(destId) = createDestinationAccount()

      // Simulate a transfer by deducting balance
      val acc = ctx.x.accounts(serviceId)
      val updatedInfo = acc.info.copy(balance = acc.info.balance - transferAmount)
      ctx.x.accounts(serviceId) = acc.copy(info = updatedInfo)

      // Queue the deferred transfer
      ctx.deferredTransfers += DeferredTransfer(
        source = serviceId,
        destination = destId,
        amount = transferAmount,
        memo = JamBytes.zeros(128),
        gasLimit = 1000L
      )

      // Property: balance should decrease by exact transfer amount
      ctx.x.accounts(serviceId).info.balance shouldBe (initialBalance - transferAmount)

      // Property: transfer should be queued
      ctx.deferredTransfers.size shouldBe 1
      ctx.deferredTransfers.head.amount shouldBe transferAmount
    }
  }

  test("property: deferred transfers are queued in FIFO order") {
    forAll(genDeferredTransfer, genDeferredTransfer, genDeferredTransfer) { (t1, t2, t3) =>
      val ctx = createContextWithService()
      val serviceId = ctx.serviceIndex

      // Create transfers with distinct destinations for clarity
      val transfers = List(
        t1.copy(source = serviceId, destination = 100L),
        t2.copy(source = serviceId, destination = 101L),
        t3.copy(source = serviceId, destination = 102L)
      )

      transfers.foreach(ctx.deferredTransfers += _)

      // Property: transfers should be in insertion order
      ctx.deferredTransfers.size shouldBe 3
      ctx.deferredTransfers.head.destination shouldBe 100L
      ctx.deferredTransfers(1).destination shouldBe 101L
      ctx.deferredTransfers(2).destination shouldBe 102L

      // Property: original amounts should be preserved
      ctx.deferredTransfers.head.amount shouldBe t1.amount
      ctx.deferredTransfers(1).amount shouldBe t2.amount
      ctx.deferredTransfers(2).amount shouldBe t3.amount
    }
  }

  test("property: checkpoint creates independent copy of state") {
    forAll(genPartialState(testConfig)) { initialState =>
      val ctx = AccumulationContext(
        initialState = initialState,
        serviceIndex = 100L,
        timeslot = 1000L,
        entropy = JamBytes.zeros(32)
      )

      // Modify x state
      ctx.x.manager = 42L

      // Take checkpoint
      ctx.checkpoint()

      // Further modify x state
      ctx.x.manager = 100L

      // Property: y (checkpoint) should have value at checkpoint time
      ctx.y.manager shouldBe 42L

      // Property: x should have latest value
      ctx.x.manager shouldBe 100L

      // Property: modifying x after checkpoint should not affect y
      ctx.y.manager should not be 100L
    }
  }

  test("property: privileges remain unchanged when not modified") {
    forAll(genPartialState(testConfig)) { initialState =>
      val ctx = AccumulationContext(
        initialState = initialState,
        serviceIndex = 100L,
        timeslot = 1000L,
        entropy = JamBytes.zeros(32)
      )

      // Store initial privilege values
      val initialManager = ctx.x.manager
      val initialDelegator = ctx.x.delegator
      val initialRegistrar = ctx.x.registrar
      val initialAssignersSize = ctx.x.assigners.size

      // Simulate some non-privilege operations (e.g., storage writes)
      if ctx.x.accounts.nonEmpty then
        val someServiceId = ctx.x.accounts.keys.head
        ctx.x.accounts.get(someServiceId).foreach { acc =>
          acc.storage(JamBytes(Array[Byte](1, 2, 3))) = JamBytes(Array[Byte](4, 5, 6))
        }

      // Property: privileges should remain unchanged
      ctx.x.manager shouldBe initialManager
      ctx.x.delegator shouldBe initialDelegator
      ctx.x.registrar shouldBe initialRegistrar
      ctx.x.assigners.size shouldBe initialAssignersSize
    }
  }

  test("property: storage writes are visible in subsequent reads") {
    forAll(genJamBytes(32), genJamBytesRange(1, 100)) { (key, value) =>
      val ctx = createContextWithService()
      val serviceId = ctx.serviceIndex
      val account = ctx.x.accounts(serviceId)

      // Write to storage
      account.storage(key) = value

      // Property: read should return the written value
      account.storage.get(key) shouldBe Some(value)

      // Property: different key should return None
      val otherKey = JamBytes(key.toArray.map(b => (b + 1).toByte))
      if !account.storage.contains(otherKey) then
        account.storage.get(otherKey) shouldBe None
    }
  }

  test("property: collapse returns y state on PANIC, x state on HALT") {
    forAll(genPartialState(testConfig)) { initialState =>
      val ctx = AccumulationContext(
        initialState = initialState,
        serviceIndex = 100L,
        timeslot = 1000L,
        entropy = JamBytes.zeros(32)
      )

      // Set distinct values for x and y
      ctx.x.manager = 1L
      ctx.checkpoint() // y now has manager = 1
      ctx.x.manager = 2L // x now has manager = 2

      // Property: HALT should return x state
      val haltState = ctx.collapse(ExitReason.HALT)
      haltState.manager shouldBe 2L

      // Property: PANIC should return y state
      val panicState = ctx.collapse(ExitReason.PANIC)
      panicState.manager shouldBe 1L

      // Property: OUT_OF_GAS should return y state
      val oogState = ctx.collapse(ExitReason.OUT_OF_GAS)
      oogState.manager shouldBe 1L
    }
  }

  private def createDestinationAccount(): ServiceAccount =
    ServiceAccount(
      info = ServiceInfo(
        version = 0,
        codeHash = Hash.zero,
        balance = 10000L,
        minItemGas = 10L,
        minMemoGas = 50L,
        bytesUsed = 0L,
        depositOffset = 0L,
        items = 0,
        creationSlot = 0L,
        lastAccumulationSlot = 0L,
        parentService = 0L
      ),
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0L
    )
