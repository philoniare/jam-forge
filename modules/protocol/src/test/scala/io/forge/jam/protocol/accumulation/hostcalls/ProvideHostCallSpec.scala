package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for PROVIDE (26) host call.
 * Provides preimage for another service.
 */
class ProvideHostCallSpec extends HostCallTestBase:

  test("PROVIDE: returns WHO for non-existent target service") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val blob = Array.fill[Byte](50)(0xab.toByte)
    val blobAddr = 0x10000
    instance.writeBytes(blobAddr, blob)

    instance.setReg(7, 999L) // non-existent service
    instance.setReg(8, blobAddr)
    instance.setReg(9, blob.length)

    hostCalls.dispatch(HostCall.PROVIDE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }

  test("PROVIDE: returns HUH when preimage not solicited") {
    val targetId = 200L
    val context = createTestContext()
    // Add target service without any preimage requests
    context.x.accounts(targetId) = createTestAccount(1000L)

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val blob = Array.fill[Byte](50)(0xab.toByte)
    val blobAddr = 0x10000
    instance.writeBytes(blobAddr, blob)

    instance.setReg(7, targetId)
    instance.setReg(8, blobAddr)
    instance.setReg(9, blob.length)

    hostCalls.dispatch(HostCall.PROVIDE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("PROVIDE: returns HUH when preimage already provided") {
    val targetId = 200L
    val context = createTestContext()

    val blob = Array.fill[Byte](50)(0xab.toByte)
    val blobHash = io.forge.jam.core.Hashing.blake2b256(blob)

    // Add target service with solicited preimage request AND existing preimage
    val targetAccount = createTestAccount(1000L)
    val key = PreimageKey(Hash(blobHash.bytes.toArray), blob.length)
    targetAccount.preimageRequests(key) = PreimageRequest(List.empty)
    targetAccount.preimages(Hash(blobHash.bytes.toArray)) = JamBytes(blob) // Already provided!
    context.x.accounts(targetId) = targetAccount

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val blobAddr = 0x10000
    instance.writeBytes(blobAddr, blob)

    instance.setReg(7, targetId)
    instance.setReg(8, blobAddr)
    instance.setReg(9, blob.length)

    hostCalls.dispatch(HostCall.PROVIDE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("PROVIDE: returns HUH when duplicate provision in same execution") {
    val targetId = 200L
    val context = createTestContext()

    val blob = Array.fill[Byte](50)(0xab.toByte)
    val blobHash = io.forge.jam.core.Hashing.blake2b256(blob)

    // Add target service with solicited preimage request
    val targetAccount = createTestAccount(1000L)
    val key = PreimageKey(Hash(blobHash.bytes.toArray), blob.length)
    targetAccount.preimageRequests(key) = PreimageRequest(List.empty)
    context.x.accounts(targetId) = targetAccount

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val blobAddr = 0x10000
    instance.writeBytes(blobAddr, blob)

    instance.setReg(7, targetId)
    instance.setReg(8, blobAddr)
    instance.setReg(9, blob.length)

    // First provision succeeds
    hostCalls.dispatch(HostCall.PROVIDE, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.OK

    // Reset reg(7) to targetId since dispatch overwrote it with OK
    instance.setReg(7, targetId)
    instance.setReg(8, blobAddr)
    instance.setReg(9, blob.length)

    // Second provision with same blob should fail (duplicate)
    hostCalls.dispatch(HostCall.PROVIDE, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("PROVIDE: returns OK on successful provision") {
    val targetId = 200L
    val context = createTestContext()

    val blob = Array.fill[Byte](50)(0xab.toByte)
    val blobHash = io.forge.jam.core.Hashing.blake2b256(blob)

    // Add target service with solicited preimage request
    val targetAccount = createTestAccount(1000L)
    val key = PreimageKey(Hash(blobHash.bytes.toArray), blob.length)
    targetAccount.preimageRequests(key) = PreimageRequest(List.empty)
    context.x.accounts(targetId) = targetAccount

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val blobAddr = 0x10000
    instance.writeBytes(blobAddr, blob)

    instance.setReg(7, targetId)
    instance.setReg(8, blobAddr)
    instance.setReg(9, blob.length)

    hostCalls.dispatch(HostCall.PROVIDE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    context.provisions.size shouldBe 1
  }
