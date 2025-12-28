package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for LOOKUP (2) host call.
 * Looks up preimage by hash.
 */
class LookupHostCallSpec extends HostCallTestBase:

  test("LOOKUP: returns WHO for non-existent service") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, 999L) // non-existent service
    instance.setReg(8, hashAddr)
    instance.setReg(9, 0x20000) // output
    instance.setReg(10, 0) // offset
    instance.setReg(11, 100) // length

    hostCalls.dispatch(HostCall.LOOKUP, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }

  test("LOOKUP: returns NONE for non-existent preimage") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hash = Array.fill[Byte](32)(0x42.toByte)
    val hashAddr = 0x10000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, -1L) // current service
    instance.setReg(8, hashAddr)
    instance.setReg(9, 0x20000)
    instance.setReg(10, 0)
    instance.setReg(11, 100)

    hostCalls.dispatch(HostCall.LOOKUP, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.NONE
  }

  test("LOOKUP: returns preimage length on success") {
    val context = createTestContext()

    // Add preimage to account
    val hash = Array.fill[Byte](32)(0x42.toByte)
    val preimage = Array.fill[Byte](100)(0xab.toByte)
    context.x.accounts(100L).preimages(Hash(hash)) = JamBytes(preimage)

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val hashAddr = 0x10000
    val outputAddr = 0x20000
    instance.writeBytes(hashAddr, hash)

    instance.setReg(7, -1L) // current service
    instance.setReg(8, hashAddr)
    instance.setReg(9, outputAddr)
    instance.setReg(10, 0) // offset
    instance.setReg(11, 200) // length

    hostCalls.dispatch(HostCall.LOOKUP, instance)

    // Should return preimage length
    instance.reg(7) shouldBe 100L

    // Verify preimage was written to memory
    val written = instance.readBytes(outputAddr, 100)
    written shouldBe defined
    written.get shouldBe preimage
  }
