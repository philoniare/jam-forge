package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.accumulation._

import scala.collection.mutable

/**
 * Tests for memory edge cases (OOB and PANIC).
 *
 * These tests verify behavior when:
 * - OOB: Memory write operations fail (output address out of bounds)
 * - PANIC: Memory read operations fail (input address out of bounds)
 */
class MemoryEdgeCaseSpec extends HostCallTestBase:

  // Use a small memory size to make it easy to exceed bounds
  private def createSmallMemoryInstance(gas: Long = 1000000L): MockPvmInstance =
    new MockPvmInstance(memorySize = 0x1000, initialGas = gas) // Only 4KB

  // ===========================================================================
  // FETCH (1) - OOB Tests
  // ===========================================================================

  test("FETCH: returns OOB when output address is out of bounds") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    // Set up FETCH for entropy (selector 1) which returns 32 bytes
    instance.setReg(10, 1) // selector = entropy
    instance.setReg(7, 0x10000) // output address WAY beyond 4KB memory
    instance.setReg(8, 0) // offset
    instance.setReg(9, 32) // length

    // Should throw PANIC (RuntimeException) for inaccessible memory
    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.FETCH, instance)
    }
    exception.getMessage should include("PANIC")
  }

  test("FETCH: returns OOB when output length exceeds memory bounds") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    // Set up FETCH for entropy (selector 1) which returns 32 bytes
    // Put output address at 0xFF0 so 32 bytes won't fit in 4KB (0x1000) memory
    instance.setReg(10, 1) // selector = entropy (returns 32 bytes)
    instance.setReg(7, 0xFF0) // output address near end of memory
    instance.setReg(8, 0) // offset
    instance.setReg(9, 32) // request 32 bytes which extends past 0x1000

    // Should throw PANIC for memory not writable
    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.FETCH, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // LOOKUP (2) - OOB and PANIC Tests
  // ===========================================================================

  test("LOOKUP: throws PANIC when hash address is out of bounds") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    // Set hash address out of bounds - can't read hash
    instance.setReg(7, -1L) // current service
    instance.setReg(8, 0x10000) // hash address WAY beyond memory
    instance.setReg(9, 0x100) // output address (valid)
    instance.setReg(10, 0) // offset
    instance.setReg(11, 100) // length

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.LOOKUP, instance)
    }
    exception.getMessage should include("PANIC")
  }

  test("LOOKUP: throws PANIC when output address is out of bounds") {
    val context = createTestContext()

    // Add a preimage so lookup succeeds but write fails
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val preimage = Array.fill[Byte](100)(0xab.toByte)
    context.x.accounts(100L).preimages(Hash(hash)) = JamBytes(preimage)

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    // Write hash to valid memory location
    val hashAddr = 0x100
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, -1L) // current service
    instance.setReg(8, hashAddr) // valid hash address
    instance.setReg(9, 0x10000) // output address OUT OF BOUNDS
    instance.setReg(10, 0) // offset
    instance.setReg(11, 100) // length

    // Implementation throws PANIC when output memory is not writable
    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.LOOKUP, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // READ (3) - PANIC Tests
  // ===========================================================================

  test("READ: throws PANIC when key address is out of bounds") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, -1L) // current service
    instance.setReg(8, 0x10000) // key address OUT OF BOUNDS
    instance.setReg(9, 4) // key length
    instance.setReg(10, 0x100) // output address (valid)
    instance.setReg(11, 0) // offset
    instance.setReg(12, 100) // length

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.READ, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // WRITE (4) - PANIC Tests
  // ===========================================================================

  test("WRITE: throws PANIC when key address is out of bounds") {
    val context = createTestContext(balance = 10000000L)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, 0x10000) // key address OUT OF BOUNDS
    instance.setReg(8, 4) // key length
    instance.setReg(9, 0x100) // value address (valid)
    instance.setReg(10, 10) // value length

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.WRITE, instance)
    }
    exception.getMessage should include("PANIC")
  }

  test("WRITE: throws PANIC when value address is out of bounds") {
    val context = createTestContext(balance = 10000000L)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    // Write key to valid memory
    val key = Array[Byte](1, 2, 3, 4)
    val keyAddr = 0x100
    instance.writeBytes(keyAddr, key)

    instance.setReg(7, keyAddr) // valid key address
    instance.setReg(8, 4) // key length
    instance.setReg(9, 0x10000) // value address OUT OF BOUNDS
    instance.setReg(10, 10) // value length

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.WRITE, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // INFO (5) - PANIC Tests (OOB on write)
  // ===========================================================================

  test("INFO: throws PANIC when output address is out of bounds") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, -1L) // current service
    instance.setReg(8, 0x10000) // output address OUT OF BOUNDS
    instance.setReg(9, 0) // offset
    instance.setReg(10, 96) // length

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.INFO, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // QUERY (22) - PANIC Tests
  // ===========================================================================

  test("QUERY: throws PANIC when hash address is out of bounds") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, 0x10000) // hash address OUT OF BOUNDS
    instance.setReg(8, 100) // length

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.QUERY, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // SOLICIT (23) - PANIC Tests
  // ===========================================================================

  test("SOLICIT: throws PANIC when hash address is out of bounds") {
    val context = createTestContext(balance = 10000000L)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, 0x10000) // hash address OUT OF BOUNDS
    instance.setReg(8, 100) // length

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.SOLICIT, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // FORGET (24) - PANIC Tests
  // ===========================================================================

  test("FORGET: throws PANIC when hash address is out of bounds") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, 0x10000) // hash address OUT OF BOUNDS
    instance.setReg(8, 100) // length

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.FORGET, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // YIELD (25) - PANIC Tests
  // ===========================================================================

  test("YIELD: throws PANIC when hash address is out of bounds") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, 0x10000) // hash address OUT OF BOUNDS

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.YIELD, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // PROVIDE (26) - PANIC Tests
  // ===========================================================================

  test("PROVIDE: throws PANIC when blob address is out of bounds") {
    val targetId = 200L
    val context = createTestContext()
    context.x.accounts(targetId) = createTestAccount(1000L)

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, targetId)
    instance.setReg(8, 0x10000) // blob address OUT OF BOUNDS
    instance.setReg(9, 50) // blob length

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.PROVIDE, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // BLESS (14) - PANIC Tests
  // ===========================================================================

  test("BLESS: throws PANIC when assigners address is out of bounds") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, 1L) // manager
    instance.setReg(8, 0x10000) // assigners address OUT OF BOUNDS
    instance.setReg(9, 2L) // delegator
    instance.setReg(10, 3L) // registrar
    instance.setReg(11, 0) // alwaysAccPtr
    instance.setReg(12, 0) // alwaysAccCount

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.BLESS, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // ASSIGN (15) - PANIC Tests
  // ===========================================================================

  test("ASSIGN: throws PANIC when auth queue address is out of bounds") {
    val context = createTestContext()
    context.x.assigners += context.serviceIndex // Make caller the assigner

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, 0) // core index
    instance.setReg(8, 0x10000) // auth queue address OUT OF BOUNDS
    instance.setReg(9, 42L) // new assigner

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.ASSIGN, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // DESIGNATE (16) - PANIC Tests
  // ===========================================================================

  test("DESIGNATE: throws PANIC when validator keys address is out of bounds") {
    val context = createTestContext()
    context.x.delegator = context.serviceIndex // Make caller the delegator

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, 0x10000) // validator keys address OUT OF BOUNDS

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.DESIGNATE, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // NEW (18) - PANIC Tests
  // ===========================================================================

  test("NEW: throws PANIC when code hash address is out of bounds") {
    val context = createTestContext(balance = 10000000L)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, 0x10000) // code hash address OUT OF BOUNDS
    instance.setReg(8, 100) // code length
    instance.setReg(9, 100L) // minAccumulateGas
    instance.setReg(10, 50L) // minMemoGas
    instance.setReg(11, 0L) // gratisStorage
    instance.setReg(12, 0L) // requestedServiceId

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.NEW, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // UPGRADE (19) - PANIC Tests
  // ===========================================================================

  test("UPGRADE: throws PANIC when code hash address is out of bounds") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, 0x10000) // code hash address OUT OF BOUNDS
    instance.setReg(8, 200L) // minAccumulateGas
    instance.setReg(9, 100L) // minMemoGas

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.UPGRADE, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // TRANSFER (20) - PANIC Tests
  // ===========================================================================

  test("TRANSFER: throws PANIC when memo address is out of bounds") {
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
    val instance = createSmallMemoryInstance()

    instance.setReg(7, destId)
    instance.setReg(8, 500L) // amount
    instance.setReg(9, 100L) // gasLimit
    instance.setReg(10, 0x10000) // memo address OUT OF BOUNDS

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.TRANSFER, instance)
    }
    exception.getMessage should include("PANIC")
  }

  // ===========================================================================
  // EJECT (21) - PANIC Tests
  // ===========================================================================

  test("EJECT: throws PANIC when preimage hash address is out of bounds") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createSmallMemoryInstance()

    instance.setReg(7, 200L) // target service
    instance.setReg(8, 0x10000) // preimage hash address OUT OF BOUNDS

    val exception = intercept[RuntimeException] {
      hostCalls.dispatch(HostCall.EJECT, instance)
    }
    exception.getMessage should include("PANIC")
  }
