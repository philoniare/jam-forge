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

  test("PROVIDE: returns HUH when z >= 2^32, even with an unreadable blob pointer") {
    val targetId = 200L
    val context = createTestContext()
    context.x.accounts = context.x.accounts.updated(targetId, createTestAccount(1000L))

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // No blob written, and blobAddr is out of the mapped memory range -- if
    // the HUH-z guard did not run FIRST, this would panic.
    val blobAddr = 0x7fffffff
    instance.setReg(7, targetId)
    instance.setReg(8, blobAddr)
    instance.setReg(9, 0x100000000L) // z = 2^32, not in bloblength

    noException should be thrownBy hostCalls.dispatch(HostCall.PROVIDE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
    context.provisions shouldBe empty
  }

  private class PageMapLikeInstance extends MockPvmInstance(0x100000):
    override def isMemoryReadable(address: Int, length: Int): Boolean =
      if length < 0 then super.isMemoryReadable(address, 1)
      else super.isMemoryReadable(address, length)

  test("PageMap (unit): isReadable answers true for a negative length - a PageMap-level invariant currently masked by every production PvmInstance wrapper") {
    import io.forge.jam.pvm.memory.{PageAccess, PageMap}
    import spire.math.UInt
    val pm = new PageMap(UInt(4096))
    pm.setPageAccess(UInt(0x10), PageAccess.ReadWrite) // page containing 0x10000

    pm.isReadable(UInt(0x10000), 16)._1 shouldBe true
    pm.isReadable(UInt(0x10000), -1)._1 shouldBe true
    pm.isReadable(UInt(0x10005), -1)._1 shouldBe true
  }

  test("PROVIDE: PANICs (not NegativeArraySizeException) when z = 0xFFFFFFFF") {
    val targetId = 200L
    val context = createTestContext()
    context.x.accounts = context.x.accounts.updated(targetId, createTestAccount(1000L))

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = new PageMapLikeInstance

    instance.setReg(7, targetId)
    instance.setReg(8, 0x10000)
    instance.setReg(9, 0xffffffffL) // passes the `z > 2^32-1` HUH guard; .toInt = -1

    val ex = intercept[io.forge.jam.protocol.HostCallPanic] {
      hostCalls.dispatch(HostCall.PROVIDE, instance)
    }
    ex.getMessage should include("Provide PANIC")
    context.provisions shouldBe empty
  }

  test("PROVIDE: PANICs for every z that narrows to a negative Int") {
    val targetId = 200L
    for z <- Seq(0x80000000L, 0xc0000000L, 0xfffffffeL, 0xffffffffL) do
      val context = createTestContext()
      context.x.accounts = context.x.accounts.updated(targetId, createTestAccount(1000L))
      val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
      val instance = new PageMapLikeInstance

      instance.setReg(7, targetId)
      instance.setReg(8, 0x10000)
      instance.setReg(9, z)

      withClue(s"z=0x${z.toHexString}: ") {
        intercept[io.forge.jam.protocol.HostCallPanic] {
          hostCalls.dispatch(HostCall.PROVIDE, instance)
        }
      }
  }

  test("READ: PANICs when the key length narrows to a negative Int") {
    for keyLen <- Seq(0x80000000L, 0xfffffffeL, 0xffffffffL, 0x1_80000000L) do
      val context = createTestContext()
      val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
      val instance = new PageMapLikeInstance

      instance.setReg(7, 0xffffffffffffffffL) // self
      instance.setReg(8, 0x10000)
      instance.setReg(9, keyLen)
      instance.setReg(10, 0x20000)

      withClue(s"keyLen=0x${keyLen.toHexString}: ") {
        val ex = intercept[io.forge.jam.protocol.HostCallPanic] {
          hostCalls.dispatch(HostCall.READ, instance)
        }
        ex.getMessage should include("Read PANIC")
      }
  }

  test("READ: a key length of exactly 2^32 truncates to 0, not to a panic") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = new PageMapLikeInstance

    instance.setReg(7, 0xffffffffffffffffL)
    instance.setReg(8, 0x10000)
    instance.setReg(9, 0x1_00000000L)
    instance.setReg(10, 0x20000)

    noException should be thrownBy hostCalls.dispatch(HostCall.READ, instance)
    ULong(instance.reg(7)) shouldBe HostCallResult.NONE
  }

  test("WRITE: PANICs when the key or value length narrows to a negative Int") {
    for (keyLen, valueLen) <- Seq(
        (0xffffffffL, 4L),
        (0x80000000L, 4L),
        (4L, 0xffffffffL),
        (4L, 0x80000000L)
      )
    do
      val context = createTestContext()
      val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
      val instance = new PageMapLikeInstance

      instance.writeBytes(0x10000, Array.fill[Byte](8)(1))
      instance.writeBytes(0x20000, Array.fill[Byte](8)(2))
      instance.setReg(7, 0x10000) // key addr
      instance.setReg(8, keyLen)
      instance.setReg(9, 0x20000) // value addr
      instance.setReg(10, valueLen)

      withClue(s"keyLen=0x${keyLen.toHexString} valueLen=0x${valueLen.toHexString}: ") {
        val ex = intercept[io.forge.jam.protocol.HostCallPanic] {
          hostCalls.dispatch(HostCall.WRITE, instance)
        }
        ex.getMessage should include("Write PANIC")
      }
  }

  test("BLESS: PANICs instead of allocating when 12*n overflows or is unreadable") {
    for n <- Seq(0x20000000L, 0x30000000L, 0x0fffffffL, 100000000L) do
      val context = createTestContext()
      val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
      val instance = createMockInstance()

      instance.setReg(7, 1L) // manager
      instance.setReg(8, 0x10000) // assigners ptr
      instance.setReg(9, 1L)
      instance.setReg(10, 1L)
      instance.setReg(11, 0x20000) // always-acc ptr
      instance.setReg(12, n)

      withClue(s"n=0x${n.toHexString}: ") {
        val ex = intercept[io.forge.jam.protocol.HostCallPanic] {
          hostCalls.dispatch(HostCall.BLESS, instance)
        }
        ex.getMessage should include("Bless PANIC")
      }
  }
