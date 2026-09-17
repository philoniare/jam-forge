package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

import scala.collection.mutable

/**
 * Tests for DESIGNATE (16) host call.
 * Sets validator queue.
 */
class DesignateHostCallSpec extends HostCallTestBase:

  test("DESIGNATE: returns HUH when caller is not delegator (z valid)") {
    val context = createTestContext()
    context.x.delegator = 999L // Different from current service (100)

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val z = testConfig.validatorCount // 6, a valid valcount member
    val validatorKeysAddr = 0x10000
    val validatorKeysBytes = new Array[Byte](336 * z)
    instance.writeBytes(validatorKeysAddr, validatorKeysBytes)

    instance.setReg(7, validatorKeysAddr)
    instance.setReg(8, z)

    hostCalls.dispatch(HostCall.DESIGNATE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("DESIGNATE: returns OK when caller is delegator (z = 3*coresCount = 6, valcount upper boundary)") {
    val context = createTestContext()
    context.x.delegator = context.serviceIndex // Caller is delegator

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val z = 3 * testConfig.coresCount // 6 for TINY (== config.validatorCount)
    val validatorKeysAddr = 0x10000
    val validatorKeysBytes = new Array[Byte](336 * z)
    instance.writeBytes(validatorKeysAddr, validatorKeysBytes)

    instance.setReg(7, validatorKeysAddr)
    instance.setReg(8, z)

    hostCalls.dispatch(HostCall.DESIGNATE, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    context.x.stagingSet.size shouldBe z
  }

  test("ChainConfig.FULL.isValidValidatorCount: 1023 (= 3*341) accepted, 1026 rejected") {
    ChainConfig.FULL.isValidValidatorCount(1023) shouldBe true
    ChainConfig.FULL.isValidValidatorCount(1026) shouldBe false
  }

  for z <- Seq(5, 7, 3 * (2 /* TINY coresCount */ + 1), 3 * (2 + 1) + 3) do
    test(s"DESIGNATE: returns HUH for z=$z (not in valcount) even as delegator") {
      val context = createTestContext()
      context.x.delegator = context.serviceIndex

      val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
      val instance = createMockInstance()

      val validatorKeysAddr = 0x10000
      val validatorKeysBytes = new Array[Byte](336 * z)
      instance.writeBytes(validatorKeysAddr, validatorKeysBytes)

      instance.setReg(7, validatorKeysAddr)
      instance.setReg(8, z)

      hostCalls.dispatch(HostCall.DESIGNATE, instance)

      ULong(instance.reg(7)) shouldBe HostCallResult.HUH
      context.x.stagingSet shouldBe empty
    }

  test("DESIGNATE: 336z in the (2^31, 2^32] window PANICs when the range is unreadable") {
    val context = createTestContext()
    context.x.delegator = context.serviceIndex

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val z = 6392000L // 336 * z = 2147712000 (> Int.MaxValue, < 2^32)
    instance.setReg(7, 0L) // startAddr = 0, avoids the mock's own address+length
                            // Int-overflow footgun (see isMemoryAccessible) so the
                            // "not accessible" result is genuine, not an artifact.
    instance.setReg(8, z)

    a[RuntimeException] should be thrownBy {
      hostCalls.dispatch(HostCall.DESIGNATE, instance)
    }
    context.x.stagingSet shouldBe empty
  }

  private class ExposedHostCalls(context: AccumulationContext, config: ChainConfig)
      extends AccumulationHostCalls(context, List.empty, config):
    def testIsRangeReadable(instance: PvmInstance, startAddr: Int, totalLength: Long): Boolean =
      isRangeReadable(instance, startAddr, totalLength)

  private class ScriptedReadableInstance(unreadableChunkIndex: Option[Int])
      extends MockPvmInstance(memorySize = 1, initialGas = 1000000L):
    val queriedRanges: mutable.ArrayBuffer[(Int, Int)] = mutable.ArrayBuffer.empty
    override def isMemoryReadable(address: Int, length: Int): Boolean =
      val idx = queriedRanges.size
      queriedRanges += ((address, length))
      !unreadableChunkIndex.contains(idx)

  test("HostCallSupport.isRangeReadable splits a > Int.MaxValue length into Int-sized chunks, all must be readable") {
    val context = createTestContext()
    val hostCalls = new ExposedHostCalls(context, testConfig)

    // totalLength = 0xFFFFFFFFL (the max window value: 336 * maxWindowZ's
    // remainder case) needs 3 chunks: Int.MaxValue, Int.MaxValue, 1.
    val totalLength = 0xffffffffL
    val instance = new ScriptedReadableInstance(unreadableChunkIndex = None)

    hostCalls.testIsRangeReadable(instance, startAddr = 0, totalLength) shouldBe true

    instance.queriedRanges.toSeq shouldBe Seq(
      (0, Int.MaxValue),
      (Int.MaxValue, Int.MaxValue),
      (-2, 1)
    )
  }

  test("HostCallSupport.isRangeReadable returns false and short-circuits as soon as one chunk is unreadable") {
    val context = createTestContext()
    val hostCalls = new ExposedHostCalls(context, testConfig)

    val totalLength = 0xffffffffL
    val instance = new ScriptedReadableInstance(unreadableChunkIndex = Some(1))

    hostCalls.testIsRangeReadable(instance, startAddr = 0, totalLength) shouldBe false

    instance.queriedRanges.toSeq shouldBe Seq(
      (0, Int.MaxValue),
      (Int.MaxValue, Int.MaxValue)
      // third chunk (2*Int.MaxValue, 1) never queried
    )
  }
