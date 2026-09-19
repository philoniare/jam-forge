package io.forge.jam.protocol.accumulation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.{ChainConfig, JamBytes, constants}
import io.forge.jam.core.primitives.{Hash, ServiceId, Timeslot, Gas, CoreIndex}
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.work.{ExecutionResult, PackageSpec}
import io.forge.jam.core.types.workresult.{WorkResult, RefineLoad}
import io.forge.jam.core.types.context.Context
import io.forge.jam.pvm.ExecutionMode
import spire.math.{UInt, UShort}

import scala.collection.mutable

class AccumulationGasAccountingSpec extends AnyFunSuite with Matchers:

  private val config: ChainConfig = ChainConfig.TINY

  private def hashOf(b: Int): Hash = Hash(Array.fill(32)(b.toByte))

  private def memo: JamBytes = JamBytes(new Array[Byte](constants.Cmemosize))

  private def buildWorkReport(
      serviceId: Long,
      accumulateGas: Long,
      packageTag: Int
  ): WorkReport =
    WorkReport(
      packageSpec = PackageSpec(
        hash = hashOf(packageTag),
        length = UInt(1000),
        erasureRoot = hashOf(0x02),
        exportsRoot = hashOf(0x03),
        exportsCount = UShort(1)
      ),
      context = Context(
        anchor = hashOf(0x04),
        stateRoot = hashOf(0x05),
        beefyRoot = hashOf(0x06),
        lookupAnchor = hashOf(0x07),
        lookupAnchorSlot = Timeslot(1),
        prerequisites = List.empty
      ),
      coreIndex = CoreIndex(0),
      authorizerHash = hashOf(0x08),
      authGasUsed = Gas(0),
      authOutput = JamBytes(Array.emptyByteArray),
      segmentRootLookup = List.empty,
      results = List(
        WorkResult(
          serviceId = ServiceId(serviceId.toInt),
          codeHash = hashOf(0x09),
          payloadHash = hashOf(0x0a),
          accumulateGas = Gas(accumulateGas),
          result = ExecutionResult.Ok(JamBytes(Array.emptyByteArray)),
          refineLoad = RefineLoad(Gas(100), UShort(0), UShort(0), UInt(0), UShort(0))
        )
      )
    )

  private def emptyPartialState(
      accounts: Map[Long, ServiceAccount] = Map.empty
  ): PartialState =
    PartialState(
      accounts = accounts,
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )

  test("reserved transfer gas excludes a report that would otherwise fit") {
    val destService = 300L
    val reportServiceId = 400L

    val report = buildWorkReport(serviceId = reportServiceId, accumulateGas = 1000L, packageTag = 0x11)
    val transfer = DeferredTransfer(
      source = destService,
      destination = destService,
      amount = 0L,
      memo = memo,
      gasLimit = 500L
    )

    val executor = new AccumulationExecutor(config):
      override def executeService(
          partialState: PartialState,
          timeslot: Long,
          serviceId: Long,
          gasLimit: Long,
          entropy: JamBytes,
          operands: List[AccumulationOperand],
          executionMode: ExecutionMode
      ): AccumulationOneResult =
        if serviceId == destService then
          AccumulationOneResult(partialState, List.empty, None, 500L, Set.empty)
        else AccumulationOneResult(partialState, List.empty, None, 0L, Set.empty)

    def run(gasLimit: Long): AccumulationTransition.OuterAccumulationResult =
      AccumulationTransition.outerAccumulate(
        partialState = emptyPartialState(),
        transfers = List(transfer),
        workReports = List(report),
        alwaysAccers = Map.empty,
        gasLimit = gasLimit,
        timeslot = 1000L,
        entropy = JamBytes.zeros(32),
        executor = executor,
        config = config
      )

    run(gasLimit = 1499L).reportsAccumulated shouldBe 0
    run(gasLimit = 1500L).reportsAccumulated shouldBe 1
  }

  test("recursion: next gasLimit sums accpar's OUTPUT transfers, not the caller's input transfers") {
    val sx = 10L
    val sy = 20L

    val report1 = buildWorkReport(serviceId = sx, accumulateGas = 100L, packageTag = 0x21)
    val markerReport = buildWorkReport(serviceId = sy, accumulateGas = 1500L, packageTag = 0x22)

    val inputTransfer = DeferredTransfer(
      source = 1L,
      destination = sx,
      amount = 0L,
      memo = memo,
      gasLimit = 1000L
    )
    val outputTransfer = DeferredTransfer(
      source = sx,
      destination = sy,
      amount = 0L,
      memo = memo,
      gasLimit = 50L
    )

    val executor = new AccumulationExecutor(config):
      override def executeService(
          partialState: PartialState,
          timeslot: Long,
          serviceId: Long,
          gasLimit: Long,
          entropy: JamBytes,
          operands: List[AccumulationOperand],
          executionMode: ExecutionMode
      ): AccumulationOneResult =
        if serviceId == sx then
          AccumulationOneResult(partialState, List(outputTransfer), None, 80L, Set.empty)
        else AccumulationOneResult(partialState, List.empty, None, 0L, Set.empty)

    val result = AccumulationTransition.outerAccumulate(
      partialState = emptyPartialState(),
      transfers = List(inputTransfer),
      workReports = List(report1, markerReport),
      alwaysAccers = Map.empty,
      gasLimit = 1150L,
      timeslot = 1000L,
      entropy = JamBytes.zeros(32),
      executor = executor,
      config = config
    )

    result.reportsAccumulated shouldBe 1
  }
