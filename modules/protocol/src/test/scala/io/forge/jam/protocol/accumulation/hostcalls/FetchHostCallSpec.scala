package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.protocol.accumulation._
import io.forge.jam.protocol.generators.StfGenerators._
import spire.math.ULong

import scala.collection.mutable

/**
 * Tests for FETCH (1) host call.
 * Fetches data by selector (0-15).
 */
class FetchHostCallSpec extends HostCallTestBase:

  test("FETCH selector 0: returns protocol constants blob") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Set up registers for FETCH: r10=selector, r7=outputAddr, r8=offset, r9=length
    val outputAddr = 0x20000
    instance.setReg(10, 0) // selector 0 = constants
    instance.setReg(7, outputAddr)
    instance.setReg(8, 0) // offset
    instance.setReg(9, 1000) // max length

    hostCalls.dispatch(HostCall.FETCH, instance)

    // Should return the length of constants blob in r7
    val result = instance.reg(7)
    result should be > 0L // Constants blob should have some length
  }

  test("FETCH PANICs when the output buffer is on a read-only page") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val outputAddr = 0x20000
    // The output region is readable but NOT writable (a read-only page).
    instance.markReadOnly(outputAddr, 1000)
    instance.isMemoryReadable(outputAddr, 1000) shouldBe true
    instance.isMemoryWritable(outputAddr, 1000) shouldBe false

    instance.setReg(10, 0) // selector 0 = constants
    instance.setReg(7, outputAddr)
    instance.setReg(8, 0) // offset
    instance.setReg(9, 1000) // max length

    // ACC-005: writing the output to a read-only page must PANIC, not silently write.
    a[RuntimeException] should be thrownBy hostCalls.dispatch(HostCall.FETCH, instance)
  }

  test("FETCH selector 0: constants blob carries the corrected Cmaxbundlesize (trace 386 regression)") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()
    val outputAddr = 0x20000
    instance.setReg(10, 0) // selector 0 = constants
    instance.setReg(7, outputAddr)
    instance.setReg(8, 0)
    instance.setReg(9, 1000)

    hostCalls.dispatch(HostCall.FETCH, instance)

    val len = instance.reg(7).toInt
    val blob = instance.readBytes(outputAddr, len).get

    def containsLE32(v: Long): Boolean =
      val le = Array((v & 0xff).toByte, ((v >> 8) & 0xff).toByte, ((v >> 16) & 0xff).toByte, ((v >> 24) & 0xff).toByte)
      blob.sliding(4).exists(_.sameElements(le))

    containsLE32(13_791_360L) shouldBe true
    containsLE32(13_794_305L) shouldBe false
  }

  test("FETCH selector 1: returns 32-byte entropy") {
    val entropy = JamBytes(Array.fill[Byte](32)(0xab.toByte))
    val state = PartialState(
      accounts = Map(100L -> createTestAccount(100000L)),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )
    val context = AccumulationContext(state, 100L, 1000L, entropy)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val outputAddr = 0x20000
    instance.setReg(10, 1) // selector 1 = entropy
    instance.setReg(7, outputAddr)
    instance.setReg(8, 0)
    instance.setReg(9, 32)

    hostCalls.dispatch(HostCall.FETCH, instance)

    // Should return 32 bytes
    instance.reg(7) shouldBe 32L

    // Verify entropy was written to memory
    val written = instance.readBytes(outputAddr, 32)
    written shouldBe defined
    written.get shouldBe entropy.toArray
  }

  test("FETCH selector 14: returns operands list encoding with correct count") {
    val context = createTestContext()
    // Create with some operands
    val operands = List(
      AccumulationOperand.WorkItem(genOperandTuple.sample.get)
    )
    val hostCalls = new AccumulationHostCalls(context, operands, testConfig)
    val instance = createMockInstance()

    val outputAddr = 0x20000
    instance.setReg(10, 14) // selector 14 = operands list
    instance.setReg(7, outputAddr)
    instance.setReg(8, 0)
    instance.setReg(9, 1000)

    hostCalls.dispatch(HostCall.FETCH, instance)

    // Should return some length
    val result = instance.reg(7)
    result should be >= 1L // At least the compact-encoded count
  }

  test("FETCH selector 15: returns NONE for invalid operand index") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val outputAddr = 0x20000
    instance.setReg(10, 15) // selector 15 = individual operand
    instance.setReg(11, 999) // invalid index
    instance.setReg(7, outputAddr)
    instance.setReg(8, 0)
    instance.setReg(9, 1000)

    hostCalls.dispatch(HostCall.FETCH, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.NONE
  }

  test("FETCH: invalid selector returns NONE") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    instance.setReg(10, 999) // invalid selector
    instance.setReg(7, 0x20000)
    instance.setReg(8, 0)
    instance.setReg(9, 100)

    hostCalls.dispatch(HostCall.FETCH, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.NONE
  }
