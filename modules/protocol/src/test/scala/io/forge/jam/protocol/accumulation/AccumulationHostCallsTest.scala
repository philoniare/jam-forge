package io.forge.jam.protocol.accumulation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import spire.math.ULong

import scala.collection.mutable

/**
 * A simple mock implementation of PvmInstance for testing.
 */
class MockPvmInstance(
  memorySize: Int = 0x100000,
  initialGas: Long = 1000000L
) extends PvmInstance:

  private val registers = new Array[Long](13)
  private val memory = new Array[Byte](memorySize)
  private var _gas: Long = initialGas

  override def reg(regIdx: Int): Long = registers(regIdx)

  override def setReg(regIdx: Int, value: Long): Unit =
    if regIdx >= 0 && regIdx < registers.length then
      registers(regIdx) = value

  override def gas: Long = _gas

  override def setGas(value: Long): Unit =
    _gas = value

  override def readByte(address: Int): Option[Byte] =
    if address >= 0 && address < memory.length then
      Some(memory(address))
    else
      None

  override def writeByte(address: Int, value: Byte): Boolean =
    if address >= 0 && address < memory.length then
      memory(address) = value
      true
    else
      false

  override def isMemoryAccessible(address: Int, length: Int): Boolean =
    address >= 0 && (address + length) <= memory.length

  /**
   * Helper to write multiple bytes to memory.
   */
  def writeBytes(address: Int, data: Array[Byte]): Boolean =
    if !isMemoryAccessible(address, data.length) then return false
    var i = 0
    while i < data.length do
      memory(address + i) = data(i)
      i += 1
    true

  /**
   * Helper to read multiple bytes from memory.
   */
  def readBytes(address: Int, length: Int): Option[Array[Byte]] =
    if !isMemoryAccessible(address, length) then return None
    val result = new Array[Byte](length)
    var i = 0
    while i < length do
      result(i) = memory(address + i)
      i += 1
    Some(result)

/**
 * Tests for AccumulationHostCalls
 *
 * - gas(0): Returns remaining gas in r7
 * - read(3)/write(4): Storage operations with balance threshold
 * - checkpoint(17): State is properly saved and restored on panic
 * - transfer(20): Deferred transfer queuing with validation
 * - new(18): Service creation with index calculation
 * - query(22)/solicit(23)/forget(24): Preimage lifecycle
 */
class AccumulationHostCallsTest extends AnyFunSuite with Matchers:

  // ===========================================================================
  // Test Fixtures
  // ===========================================================================

  private val testConfig = ChainConfig.TINY

  private def createTestServiceInfo(balance: Long): ServiceInfo =
    ServiceInfo(
      version = 0,
      codeHash = Hash(JamBytes.zeros(32).toArray),
      balance = balance,
      minItemGas = 10L,
      minMemoGas = 20L,
      bytesUsed = 100L,
      depositOffset = 0L,
      items = 5,
      creationSlot = 0L,
      lastAccumulationSlot = 0L,
      parentService = 0L
    )

  private def createTestAccount(balance: Long): ServiceAccount =
    ServiceAccount(
      info = createTestServiceInfo(balance),
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0L
    )

  private def createTestContext(serviceIndex: Long = 100L, balance: Long = 100000L): AccumulationContext =
    val account = createTestAccount(balance)
    val state = PartialState(
      accounts = mutable.Map(serviceIndex -> account),
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
      serviceIndex = serviceIndex,
      timeslot = 1000L,
      entropy = JamBytes.zeros(32)
    )

  private def createMockInstance(gas: Long = 1000000L): MockPvmInstance =
    val instance = new MockPvmInstance(memorySize = 0x100000, initialGas = gas)
    instance

  // ===========================================================================
  // Test 1: gas(0) - Returns remaining gas in r7
  // ===========================================================================

  test("gas(0) should return remaining gas in r7") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance(50000L)

    // Call gas host call
    hostCalls.dispatch(HostCall.GAS, instance)

    // Check r7 contains remaining gas
    instance.reg(7) shouldBe 50000L
  }

  test("gas cost should be 10 for most host calls") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    hostCalls.getGasCost(HostCall.GAS, instance) shouldBe 10L
    hostCalls.getGasCost(HostCall.READ, instance) shouldBe 10L
    hostCalls.getGasCost(HostCall.WRITE, instance) shouldBe 10L
    hostCalls.getGasCost(HostCall.CHECKPOINT, instance) shouldBe 10L
  }

  test("gas cost for LOG should be 10 (v0.7.2)") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // v0.7.2: LOG now has standard gas cost of 10
    hostCalls.getGasCost(HostCall.LOG, instance) shouldBe 10L
  }

  test("gas cost for TRANSFER should be 10 upfront (v0.7.2)") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Set gas limit in r9
    val gasLimit = 5000L
    instance.setReg(9, gasLimit)

    // v0.7.2: TRANSFER only charges 10 upfront; additional gasLimit charged on success
    hostCalls.getGasCost(HostCall.TRANSFER, instance) shouldBe 10L
  }

  // ===========================================================================
  // Test 2: write(4) - Storage operations with balance threshold
  // ===========================================================================

  test("write(4) should store value and return NONE for new key") {
    val context = createTestContext(balance = 10000000L) // Large balance to avoid threshold issues
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Prepare key and value in memory
    val key = Array[Byte](1, 2, 3, 4)
    val value = Array[Byte](10, 20, 30)

    // Write key to memory at address 0x10000
    val keyAddr = 0x10000
    instance.writeBytes(keyAddr, key)

    // Write value to memory at address 0x10100
    val valueAddr = 0x10100
    instance.writeBytes(valueAddr, value)

    // Set registers: r7=keyAddr, r8=keyLen, r9=valueAddr, r10=valueLen
    instance.setReg(7, keyAddr)
    instance.setReg(8, key.length)
    instance.setReg(9, valueAddr)
    instance.setReg(10, value.length)

    // Call write host call
    hostCalls.dispatch(HostCall.WRITE, instance)

    // r7 should be NONE (key didn't exist)
    ULong(instance.reg(7)) shouldBe HostCallResult.NONE

    // Verify value was stored
    val storedValue = context.x.accounts(100L).storage.get(JamBytes(key))
    storedValue shouldBe Some(JamBytes(value))
  }

  test("write(4) should return FULL when balance threshold exceeded") {
    // Create context with very low balance
    val serviceIndex = 100L
    val info = ServiceInfo(
      version = 0,
      codeHash = Hash(JamBytes.zeros(32).toArray),
      balance = 10L, // Very low balance
      minItemGas = 10L,
      minMemoGas = 20L,
      bytesUsed = 0L,
      depositOffset = 0L,
      items = 0,
      creationSlot = 0L,
      lastAccumulationSlot = 0L,
      parentService = 0L
    )
    val account = ServiceAccount(
      info = info,
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0L
    )
    val state = PartialState(
      accounts = mutable.Map(serviceIndex -> account),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )
    val context = AccumulationContext(
      initialState = state,
      serviceIndex = serviceIndex,
      timeslot = 1000L,
      entropy = JamBytes.zeros(32)
    )
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Prepare key and value
    val key = Array.fill[Byte](100)(1) // Large key
    val value = Array.fill[Byte](1000)(2) // Large value

    val keyAddr = 0x10000
    instance.writeBytes(keyAddr, key)

    val valueAddr = 0x20000
    instance.writeBytes(valueAddr, value)

    instance.setReg(7, keyAddr)
    instance.setReg(8, key.length)
    instance.setReg(9, valueAddr)
    instance.setReg(10, value.length)

    hostCalls.dispatch(HostCall.WRITE, instance)

    // r7 should be FULL
    ULong(instance.reg(7)) shouldBe HostCallResult.FULL
  }

  // ===========================================================================
  // Test 3: checkpoint(17) - State properly saved and restored
  // ===========================================================================

  test("checkpoint(17) should copy x state to y and return remaining gas") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val currentGas = 12345L
    val instance = createMockInstance(currentGas)

    // Modify x state before checkpoint
    context.x.manager = 42L
    context.yieldHash = Some(JamBytes(Array[Byte](1, 2, 3)))
    context.deferredTransfers += DeferredTransfer(100L, 200L, 500L, JamBytes.zeros(128), 1000L)

    // Call checkpoint
    hostCalls.dispatch(HostCall.CHECKPOINT, instance)

    // r7 should contain remaining gas
    instance.reg(7) shouldBe currentGas

    // y state should now have the manager value from x
    context.y.manager shouldBe 42L

    // yieldCheckpoint should be set
    context.yieldCheckpoint shouldBe Some(JamBytes(Array[Byte](1, 2, 3)))

    // deferredTransfersCheckpoint should have the transfer
    context.deferredTransfersCheckpoint.size shouldBe 1
    context.deferredTransfersCheckpoint.head.source shouldBe 100L
  }

  test("checkpoint should preserve independence between x and y states") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance(10000L)

    // Modify x
    context.x.manager = 100L

    // Checkpoint
    hostCalls.dispatch(HostCall.CHECKPOINT, instance)

    // Further modify x
    context.x.manager = 200L

    // y should still have the checkpointed value
    context.y.manager shouldBe 100L
  }

  // ===========================================================================
  // Test 4: transfer(20) - Deferred transfer queuing with validation
  // ===========================================================================

  test("transfer(20) should queue deferred transfer on success") {
    // Create context with two accounts
    val sourceId = 100L
    val destId = 200L

    val sourceInfo = ServiceInfo(
      version = 0,
      codeHash = Hash(JamBytes.zeros(32).toArray),
      balance = 10000L,
      minItemGas = 10L,
      minMemoGas = 50L,
      bytesUsed = 100L,
      depositOffset = 0L,
      items = 5,
      creationSlot = 0L,
      lastAccumulationSlot = 0L,
      parentService = 0L
    )
    val destInfo = ServiceInfo(
      version = 0,
      codeHash = Hash(JamBytes.zeros(32).toArray),
      balance = 1000L,
      minItemGas = 10L,
      minMemoGas = 50L, // This is what gasLimit must be >= for LOW check
      bytesUsed = 100L,
      depositOffset = 0L,
      items = 5,
      creationSlot = 0L,
      lastAccumulationSlot = 0L,
      parentService = 0L
    )

    val sourceAccount = ServiceAccount(sourceInfo, mutable.Map.empty, mutable.Map.empty, mutable.Map.empty)
    val destAccount = ServiceAccount(destInfo, mutable.Map.empty, mutable.Map.empty, mutable.Map.empty)

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
    val context = AccumulationContext(
      initialState = state,
      serviceIndex = sourceId,
      timeslot = 1000L,
      entropy = JamBytes.zeros(32)
    )

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Write memo to memory
    val memoAddr = 0x10000
    val memo = new Array[Byte](128)
    for i <- 0 until 128 do memo(i) = i.toByte
    instance.writeBytes(memoAddr, memo)

    // Set registers: r7=dest, r8=amount, r9=gasLimit, r10=memoAddr
    val transferAmount = 500L
    val transferGasLimit = 100L // Must be >= destAccount.minMemoGas (50)

    instance.setReg(7, destId)
    instance.setReg(8, transferAmount)
    instance.setReg(9, transferGasLimit)
    instance.setReg(10, memoAddr)

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    // Should succeed
    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // Transfer should be queued
    context.deferredTransfers.size shouldBe 1
    val transfer = context.deferredTransfers.head
    transfer.source shouldBe sourceId
    transfer.destination shouldBe destId
    transfer.amount shouldBe transferAmount
    transfer.gasLimit shouldBe transferGasLimit

    // Source balance should be deducted
    context.x.accounts(sourceId).info.balance shouldBe (10000L - transferAmount)
  }

  test("transfer(20) should return WHO if destination does not exist") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Write memo to memory
    val memoAddr = 0x10000
    instance.writeBytes(memoAddr, new Array[Byte](128))

    // Set destination to non-existent account
    instance.setReg(7, 999L) // Non-existent
    instance.setReg(8, 100L) // amount
    instance.setReg(9, 50L) // gasLimit
    instance.setReg(10, memoAddr)

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }

  test("transfer(20) should return LOW if gasLimit < destination.minMemoGas") {
    // Create destination account with high minMemoGas
    val sourceId = 100L
    val destId = 200L

    val sourceAccount = createTestAccount(10000L)
    val destInfo = ServiceInfo(
      version = 0,
      codeHash = Hash(JamBytes.zeros(32).toArray),
      balance = 1000L,
      minItemGas = 10L,
      minMemoGas = 1000L, // High minMemoGas
      bytesUsed = 100L,
      depositOffset = 0L,
      items = 5,
      creationSlot = 0L,
      lastAccumulationSlot = 0L,
      parentService = 0L
    )
    val destAccount = ServiceAccount(destInfo, mutable.Map.empty, mutable.Map.empty, mutable.Map.empty)

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
    val context = AccumulationContext(
      initialState = state,
      serviceIndex = sourceId,
      timeslot = 1000L,
      entropy = JamBytes.zeros(32)
    )

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val memoAddr = 0x10000
    instance.writeBytes(memoAddr, new Array[Byte](128))

    instance.setReg(7, destId)
    instance.setReg(8, 100L) // amount
    instance.setReg(9, 500L) // gasLimit < minMemoGas (1000)
    instance.setReg(10, memoAddr)

    hostCalls.dispatch(HostCall.TRANSFER, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.LOW
  }

  // ===========================================================================
  // Test 5: new(18) - Service creation with index calculation
  // ===========================================================================

  test("new(18) should create new service account and return service ID") {
    val context = createTestContext(balance = 10000000L) // Large balance
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Write code hash to memory
    val codeHash = Array.fill[Byte](32)(0xab.toByte)
    val codeHashAddr = 0x10000
    instance.writeBytes(codeHashAddr, codeHash)

    // Set registers: r7=codeHashAddr, r8=codeLen, r9=minAccGas, r10=minMemoGas, r11=gratisStorage, r12=requestedId
    instance.setReg(7, codeHashAddr)
    instance.setReg(8, 1000) // code length for preimage info
    instance.setReg(9, 100L) // minAccumulateGas
    instance.setReg(10, 50L) // minMemoGas
    instance.setReg(11, 0L) // gratisStorage
    instance.setReg(12, 0L) // requestedServiceId (not registrar, so ignored)

    hostCalls.dispatch(HostCall.NEW, instance)

    // Should return new service ID (>= minPublicServiceIndex)
    val newServiceId = instance.reg(7)
    newServiceId should be >= testConfig.minPublicServiceIndex

    // New account should exist
    context.x.accounts.contains(newServiceId) shouldBe true

    // New account should have correct properties
    val newAccount = context.x.accounts(newServiceId)
    newAccount.info.minItemGas shouldBe 100L
    newAccount.info.minMemoGas shouldBe 50L
    newAccount.info.parentService shouldBe 100L // Creator's service ID
  }

  // ===========================================================================
  // Test 6: query(22)/solicit(23)/forget(24) - Preimage lifecycle
  // ===========================================================================

  test("query(22) should return NONE when preimage not requested") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Write hash to memory
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    // Set registers: r7=hashAddr, r8=length
    instance.setReg(7, hashAddr)
    instance.setReg(8, 100) // preimage length

    hostCalls.dispatch(HostCall.QUERY, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.NONE
  }

  test("solicit(23) should create preimage request and query should find it") {
    val context = createTestContext(balance = 10000000L)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Write hash to memory
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    // Solicit: r7=hashAddr, r8=length
    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.SOLICIT, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // Now query should find it
    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)

    hostCalls.dispatch(HostCall.QUERY, instance)

    // Should return count in r7 low bits (0 for newly solicited, not yet available)
    val r7Value = ULong(instance.reg(7))
    val count = r7Value.toLong & 0xffffffffL
    count shouldBe 0 // Empty list initially
  }

  test("forget(24) should expunge preimage with count 0") {
    val context = createTestContext(balance = 10000000L)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // First solicit
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)
    hostCalls.dispatch(HostCall.SOLICIT, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // Verify request exists
    val key = PreimageKey(Hash(hash), 100)
    context.x.accounts(100L).preimageRequests.contains(key) shouldBe true

    // Now forget (count is 0, so should expunge)
    instance.setReg(7, hashAddr)
    instance.setReg(8, 100)
    hostCalls.dispatch(HostCall.FORGET, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // Request should be removed
    context.x.accounts(100L).preimageRequests.contains(key) shouldBe false
  }

  // ===========================================================================
  // Test 7: read(3) - Storage read operations
  // ===========================================================================

  test("read(3) should return NONE for non-existent key") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Write key to memory
    val key = Array[Byte](1, 2, 3, 4)
    val keyAddr = 0x10000
    instance.writeBytes(keyAddr, key)

    // Set registers: r7=serviceId (-1 for current), r8=keyAddr, r9=keyLen, r10=outputAddr, r11=offset, r12=length
    instance.setReg(7, -1L) // Current service
    instance.setReg(8, keyAddr)
    instance.setReg(9, key.length)
    instance.setReg(10, 0x20000) // output address
    instance.setReg(11, 0) // offset
    instance.setReg(12, 100) // max length

    hostCalls.dispatch(HostCall.READ, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.NONE
  }

  test("read(3) should return WHO for non-existent service") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Write key to memory
    val key = Array[Byte](1, 2, 3, 4)
    val keyAddr = 0x10000
    instance.writeBytes(keyAddr, key)

    // Read from non-existent service
    instance.setReg(7, 999L) // Non-existent service
    instance.setReg(8, keyAddr)
    instance.setReg(9, key.length)
    instance.setReg(10, 0x20000)
    instance.setReg(11, 0)
    instance.setReg(12, 100)

    hostCalls.dispatch(HostCall.READ, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }
