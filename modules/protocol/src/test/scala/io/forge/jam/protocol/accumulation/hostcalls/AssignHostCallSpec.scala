package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

/**
 * Tests for ASSIGN (15) host call.
 * Sets core assigner and auth queue.
 */
class AssignHostCallSpec extends HostCallTestBase:

  test("ASSIGN: returns HUH when caller is not current assigner") {
    val context = createTestContext()
    // Set up assigners for core 0 to a different service
    context.x.assigners += 999L // Core 0 assigned to service 999, not 100

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    // Prepare auth queue
    val authQueueAddr = 0x10000
    val authQueueBytes = new Array[Byte](32 * testConfig.authQueueSize)
    instance.writeBytes(authQueueAddr, authQueueBytes)

    instance.setReg(7, 0) // core index
    instance.setReg(8, authQueueAddr)
    instance.setReg(9, 42L) // new assigner

    hostCalls.dispatch(HostCall.ASSIGN, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.HUH
  }

  test("ASSIGN: returns CORE when core index >= coresCount") {
    val context = createTestContext()
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val authQueueAddr = 0x10000
    val authQueueBytes = new Array[Byte](32 * testConfig.authQueueSize)
    instance.writeBytes(authQueueAddr, authQueueBytes)

    instance.setReg(7, testConfig.coresCount + 10) // Invalid core index
    instance.setReg(8, authQueueAddr)
    instance.setReg(9, 42L)

    hostCalls.dispatch(HostCall.ASSIGN, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.CORE
  }

  test("ASSIGN: returns WHO when new assigner exceeds UInt32 max") {
    val context = createTestContext()
    // Make caller the current assigner for core 0
    context.x.assigners += context.serviceIndex

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val authQueueAddr = 0x10000
    val authQueueBytes = new Array[Byte](32 * testConfig.authQueueSize)
    instance.writeBytes(authQueueAddr, authQueueBytes)

    instance.setReg(7, 0) // Core 0
    instance.setReg(8, authQueueAddr)
    instance.setReg(9, 0xffffffffL + 1L) // Exceeds UInt32 max

    hostCalls.dispatch(HostCall.ASSIGN, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.WHO
  }

  test("ASSIGN: returns OK on successful assignment") {
    val context = createTestContext()
    // Make caller the current assigner for core 0
    context.x.assigners += context.serviceIndex

    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()

    val authQueueAddr = 0x10000
    val authQueueBytes = new Array[Byte](32 * testConfig.authQueueSize)
    // Fill with some test hashes
    for i <- 0 until testConfig.authQueueSize do
      val hash = Array.fill[Byte](32)((i % 256).toByte)
      System.arraycopy(hash, 0, authQueueBytes, i * 32, 32)
    instance.writeBytes(authQueueAddr, authQueueBytes)

    val newAssigner = 42L
    instance.setReg(7, 0) // Core 0
    instance.setReg(8, authQueueAddr)
    instance.setReg(9, newAssigner)

    hostCalls.dispatch(HostCall.ASSIGN, instance)

    ULong(instance.reg(7)) shouldBe HostCallResult.OK
    context.x.assigners(0) shouldBe newAssigner
    context.x.authQueue(0).size shouldBe testConfig.authQueueSize
  }

  private def queueOf(fill: Int): List[Hash] =
    List.tabulate(testConfig.authQueueSize)(i => Hash(Array.fill[Byte](32)((fill + i).toByte)))

  /** ASSIGN core `core` with a queue of all-`fill` hashes; returns the host-call result. */
  private def assign(context: AccumulationContext, core: Int, fill: Int): ULong =
    val hostCalls = new AccumulationHostCalls(context, List.empty, testConfig)
    val instance = createMockInstance()
    val authQueueAddr = 0x10000
    instance.writeBytes(authQueueAddr, Array.fill[Byte](32 * testConfig.authQueueSize)(fill.toByte))
    instance.setReg(7, core)
    instance.setReg(8, authQueueAddr)
    instance.setReg(9, 42L)
    hostCalls.dispatch(HostCall.ASSIGN, instance)
    ULong(instance.reg(7))

  test("ASSIGN: replaces only the written core; the checkpoint and the shared prior queues are untouched") {
    val context = createTestContext()
    val prior = Vector(queueOf(1), queueOf(2), queueOf(3))
    val priorContents = prior.map(_.map(_.toHex))
    context.x.authQueue = prior
    context.x.assigners ++= List.fill(3)(context.serviceIndex)
    context.checkpoint()

    assign(context, core = 1, fill = 0x77) shouldBe HostCallResult.OK

    val written = List.fill(testConfig.authQueueSize)(Hash(Array.fill[Byte](32)(0x77.toByte)))
    context.x.authQueue shouldBe Vector(prior(0), written, prior(2))
    assert(context.x.authQueue(0) eq prior(0)) // untouched cores are shared, not copied
    assert(context.x.authQueue(2) eq prior(2))
    // A write after the checkpoint is not visible in the checkpoint, nor in the structure other states share.
    assert(context.y.authQueue eq prior)
    context.y.assigners.toList shouldBe List.fill(3)(context.serviceIndex)
    prior.map(_.map(_.toHex)) shouldBe priorContents
  }

  test("ASSIGN: a state copied before the write keeps the old queues (per-service isolation)") {
    val context = createTestContext()
    val prior = Vector(queueOf(1), queueOf(2))
    context.x.authQueue = prior
    context.x.assigners ++= List.fill(2)(context.serviceIndex)
    val sibling = context.x.deepCopy()

    assign(context, core = 0, fill = 0x55) shouldBe HostCallResult.OK

    assert(sibling.authQueue eq prior)
    sibling.authQueue shouldBe Vector(queueOf(1), queueOf(2))
    context.x.authQueue(0) should not be prior(0)
  }

  test("ASSIGN: a rejected call leaves the queues as they were") {
    val context = createTestContext()
    val prior = Vector(queueOf(1), queueOf(2))
    context.x.authQueue = prior
    context.x.assigners ++= List(999L, 999L) // caller is not the assigner

    assign(context, core = 1, fill = 0x55) shouldBe HostCallResult.HUH
    assert(context.x.authQueue eq prior)
  }

  test("ASSIGN: pads missing cores with empty queues up to the written core") {
    val context = createTestContext()
    context.x.assigners ++= List.fill(2)(context.serviceIndex)

    assign(context, core = 1, fill = 0x11) shouldBe HostCallResult.OK

    context.x.authQueue.map(_.size) shouldBe Vector(0, testConfig.authQueueSize)
  }
