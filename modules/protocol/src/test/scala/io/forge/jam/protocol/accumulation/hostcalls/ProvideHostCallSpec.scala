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
    context.x.accounts = context.x.accounts.updated(targetId, createTestAccount(1000L))

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

    // Add target service whose request has already been fulfilled
    val key = PreimageKey(Hash(blobHash.bytes.toArray), blob.length)
    val targetAccount = createTestAccount(1000L).copy(
      preimageRequests = Map(key -> PreimageRequest(List(42L))),
      preimages = Map(Hash(blobHash.bytes.toArray) -> JamBytes(blob))
    )
    context.x.accounts = context.x.accounts.updated(targetId, targetAccount)

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
    val key = PreimageKey(Hash(blobHash.bytes.toArray), blob.length)
    val targetAccount = createTestAccount(1000L).copy(
      preimageRequests = Map(key -> PreimageRequest(List.empty))
    )
    context.x.accounts = context.x.accounts.updated(targetId, targetAccount)

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
    val key = PreimageKey(Hash(blobHash.bytes.toArray), blob.length)
    val targetAccount = createTestAccount(1000L).copy(
      preimageRequests = Map(key -> PreimageRequest(List.empty))
    )
    context.x.accounts = context.x.accounts.updated(targetId, targetAccount)

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

  test("PROVIDE: returns OK when request lives only in the trie (cross-block solicit)") {
    val targetId = 200L
    val context = createTestContext()

    val blob = Array.fill[Byte](50)(0xab.toByte)
    val blobHash = io.forge.jam.core.Hashing.blake2b256(blob)

    // Target service exists but the solicited (empty) request lives ONLY in raw
    // state, not in the in-memory preimageRequests map (cross-block solicit).
    context.x.accounts = context.x.accounts.updated(targetId, createTestAccount(1000L))
    val infoStateKey = StateKey.computePreimageInfoStateKey(
      targetId,
      blob.length,
      JamBytes(blobHash.bytes.toArray)
    )
    context.x.rawServiceDataByStateKey = context.x.rawServiceDataByStateKey.updated(
      infoStateKey,
      StateKey.encodePreimageInfoValue(List.empty)
    )

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val blobAddr = 0x10000
    instance.writeBytes(blobAddr, blob)

    instance.setReg(7, targetId)
    instance.setReg(8, blobAddr)
    instance.setReg(9, blob.length)

    hostCalls.dispatch(HostCall.PROVIDE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    context.provisions.contains((targetId, JamBytes(blob))) shouldBe true
  }

  test("PROVIDE: returns HUH when trie request has a non-empty timeslot list") {
    val targetId = 200L
    val context = createTestContext()

    val blob = Array.fill[Byte](50)(0xab.toByte)
    val blobHash = io.forge.jam.core.Hashing.blake2b256(blob)

    // Request exists in raw state but is already provided (non-empty list).
    context.x.accounts = context.x.accounts.updated(targetId, createTestAccount(1000L))
    val infoStateKey = StateKey.computePreimageInfoStateKey(
      targetId,
      blob.length,
      JamBytes(blobHash.bytes.toArray)
    )
    context.x.rawServiceDataByStateKey = context.x.rawServiceDataByStateKey.updated(
      infoStateKey,
      StateKey.encodePreimageInfoValue(List(42L))
    )

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

  test("PROVIDE: r7 = 2^64-1 sentinel maps to the running service (self-provide)") {
    val selfId = 100L
    val context = createTestContext(serviceIndex = selfId)

    val blob = Array.fill[Byte](50)(0xab.toByte)
    val blobHash = io.forge.jam.core.Hashing.blake2b256(blob)

    // Running service solicited the preimage for itself (empty request list).
    val key = PreimageKey(Hash(blobHash.bytes.toArray), blob.length)
    context.x.accounts = context.x.accounts.updated(
      selfId,
      context.x.accounts(selfId).copy(
        preimageRequests = context.x.accounts(selfId).preimageRequests.updated(key, PreimageRequest(List.empty))
      )
    )

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val blobAddr = 0x10000
    instance.writeBytes(blobAddr, blob)

    instance.setReg(7, 0xffffffffffffffffL) // sentinel -> self
    instance.setReg(8, blobAddr)
    instance.setReg(9, blob.length)

    hostCalls.dispatch(HostCall.PROVIDE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    context.provisions.contains((selfId, JamBytes(blob))) shouldBe true
  }
