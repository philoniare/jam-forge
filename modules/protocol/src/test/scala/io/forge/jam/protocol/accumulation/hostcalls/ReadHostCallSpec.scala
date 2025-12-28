package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for READ (3) host call.
 * Reads from service storage.
 */
class ReadHostCallSpec extends HostCallTestBase:

  test("READ: returns NONE for non-existent key") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val key = Array[Byte](1, 2, 3, 4)
    val keyAddr = 0x10000
    instance.writeBytes(keyAddr, key)

    instance.setReg(7, -1L) // current service
    instance.setReg(8, keyAddr)
    instance.setReg(9, key.length)
    instance.setReg(10, 0x20000)
    instance.setReg(11, 0)
    instance.setReg(12, 100)

    hostCalls.dispatch(HostCall.READ, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.NONE
  }

  test("READ: returns WHO for non-existent service") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val key = Array[Byte](1, 2, 3, 4)
    val keyAddr = 0x10000
    instance.writeBytes(keyAddr, key)

    instance.setReg(7, 999L) // non-existent service
    instance.setReg(8, keyAddr)
    instance.setReg(9, key.length)
    instance.setReg(10, 0x20000)
    instance.setReg(11, 0)
    instance.setReg(12, 100)

    hostCalls.dispatch(HostCall.READ, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }

  test("READ: partial read with offset/length returns correct slice") {
    val context = createTestContext()
    // Store a value first
    val key = JamBytes(Array[Byte](1, 2, 3, 4))
    val value = JamBytes(Array.tabulate[Byte](100)(i => i.toByte))
    context.x.accounts(100L).storage(key) = value

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val keyAddr = 0x10000
    val outputAddr = 0x20000
    instance.writeBytes(keyAddr, key.toArray)

    instance.setReg(7, -1L)
    instance.setReg(8, keyAddr)
    instance.setReg(9, key.length)
    instance.setReg(10, outputAddr)
    instance.setReg(11, 10) // offset
    instance.setReg(12, 20) // length

    hostCalls.dispatch(HostCall.READ, instance)

    // Should return full value length
    instance.reg(7) shouldBe 100L

    // Read what was written to memory
    val written = instance.readBytes(outputAddr, 20)
    written shouldBe defined
    written.get shouldBe value.toArray.slice(10, 30)
  }
