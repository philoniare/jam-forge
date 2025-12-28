package io.forge.jam.protocol.accumulation.hostcalls

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.protocol.accumulation._
import spire.math.ULong

import scala.collection.mutable

/**
 * Base trait for all host call tests providing common fixtures and utilities.
 */
trait HostCallTestBase extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  protected val testConfig: ChainConfig = ChainConfig.TINY

  // Override default ScalaCheck configuration for faster tests
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 30)

  // ===========================================================================
  // Test Fixtures
  // ===========================================================================

  protected def createTestServiceInfo(balance: Long): ServiceInfo =
    ServiceInfo(
      version = 0,
      codeHash = Hash(JamBytes.zeros(32).toArray),
      balance = balance,
      minItemGas = 10L,
      minMemoGas = 20L,
      bytesUsed = 100L,
      depositOffset = 0L,
      items = 5,
      creationSlot = 0L,
      lastAccumulationSlot = 0L,
      parentService = 0L
    )

  protected def createTestAccount(balance: Long): ServiceAccount =
    ServiceAccount(
      info = createTestServiceInfo(balance),
      storage = mutable.Map.empty,
      preimages = mutable.Map.empty,
      preimageRequests = mutable.Map.empty,
      lastAccumulated = 0L
    )

  protected def createTestContext(serviceIndex: Long = 100L, balance: Long = 100000L): AccumulationContext =
    val account = createTestAccount(balance)
    val state = PartialState(
      accounts = mutable.Map(serviceIndex -> account),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )

    AccumulationContext(
      initialState = state,
      serviceIndex = serviceIndex,
      timeslot = 1000L,
      entropy = JamBytes.zeros(32)
    )

  protected def createMockInstance(gas: Long = 1000000L): MockPvmInstance =
    new MockPvmInstance(memorySize = 0x100000, initialGas = gas)
