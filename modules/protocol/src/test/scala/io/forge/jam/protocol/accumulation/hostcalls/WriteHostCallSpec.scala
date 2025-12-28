package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for WRITE (4) host call.
 * Writes to service storage.
 */
class WriteHostCallSpec extends HostCallTestBase:

  test("WRITE: returns NONE for new key creation") {
    val context = createTestContext(balance = 10000000L)
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val key = Array[Byte](1, 2, 3, 4)
    val value = Array[Byte](10, 20, 30)

    val keyAddr = 0x10000
    val valueAddr = 0x10100
    instance.writeBytes(keyAddr, key)
    instance.writeBytes(valueAddr, value)

    instance.setReg(7, keyAddr)
    instance.setReg(8, key.length)
    instance.setReg(9, valueAddr)
    instance.setReg(10, value.length)

    hostCalls.dispatch(HostCall.WRITE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.NONE

    // Verify value was stored
    context.x.accounts(100L).storage.get(JamBytes(key)) shouldBe Some(JamBytes(value))
  }

  test("WRITE: returns FULL when balance threshold exceeded") {
    val context = createTestContext(balance = 10L) // Very low balance
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val key = Array.fill[Byte](100)(1)
    val value = Array.fill[Byte](1000)(2)

    val keyAddr = 0x10000
    val valueAddr = 0x20000
    instance.writeBytes(keyAddr, key)
    instance.writeBytes(valueAddr, value)

    instance.setReg(7, keyAddr)
    instance.setReg(8, key.length)
    instance.setReg(9, valueAddr)
    instance.setReg(10, value.length)

    hostCalls.dispatch(HostCall.WRITE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.FULL
  }

  test("WRITE: updating existing key returns old value length") {
    val context = createTestContext(balance = 10000000L)
    // Store initial value
    val key = JamBytes(Array[Byte](1, 2, 3, 4))
    val oldValue = JamBytes(Array.fill[Byte](50)(0xaa.toByte))
    context.x.accounts(100L).storage(key) = oldValue

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val newValue = Array.fill[Byte](30)(0xbb.toByte)
    val keyAddr = 0x10000
    val valueAddr = 0x10100
    instance.writeBytes(keyAddr, key.toArray)
    instance.writeBytes(valueAddr, newValue)

    instance.setReg(7, keyAddr)
    instance.setReg(8, key.length)
    instance.setReg(9, valueAddr)
    instance.setReg(10, newValue.length)

    hostCalls.dispatch(HostCall.WRITE, instance)

    // Should return old value length
    instance.reg(7) shouldBe 50L

    // New value should be stored
    context.x.accounts(100L).storage.get(key) shouldBe Some(JamBytes(newValue))
  }
