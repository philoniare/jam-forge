package io.forge.jam.protocol.pipeline

import cats.data.StateT
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.safrole.SafroleTransition
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.dispute.DisputeTransition
import io.forge.jam.protocol.dispute.DisputeTypes.*
import io.forge.jam.protocol.assurance.AssuranceTransition
import io.forge.jam.protocol.assurance.AssuranceTypes.*
import io.forge.jam.protocol.report.ReportTransition
import io.forge.jam.protocol.report.ReportTypes.*
import io.forge.jam.protocol.accumulation.{
  AccumulationTransition,
  AccumulationInput,
  AccumulationOutputData,
  AccumulationExecutor
}
import io.forge.jam.protocol.history.HistoryTransition
import io.forge.jam.protocol.authorization.AuthorizationTransition
import io.forge.jam.protocol.preimage.PreimageTransition
import io.forge.jam.protocol.preimage.PreimageTypes.PreimageErrorCode
import io.forge.jam.protocol.statistics.StatisticsTransition
import io.forge.jam.protocol.statistics.StatisticsTypes.*
import io.forge.jam.protocol.traces.InputExtractor
import io.forge.jam.protocol.pipeline.PipelineTypes.*
import io.forge.jam.protocol.pipeline.StfLifters.*

object LiftedStfs:

  // 1. Safrole STF
  val safrole: StfStepWith[SafroleOutputData] = liftStandard(
    stf = (input, view) => SafroleTransition.stfView(input, view),
    extractInput = ctx => InputExtractor.extractSafroleInput(ctx.block),
    wrapError = (e: SafroleErrorCode) => PipelineError.SafroleErr(e)
  )

  // 2. Disputes STF
  val disputes: StfStepWith[DisputeOutputMarks] = liftStandard(
    stf = (input, view) => DisputeTransition.stfView(input, view),
    extractInput = ctx => InputExtractor.extractDisputeInput(ctx.block),
    wrapError = (e: DisputeErrorCode) => PipelineError.DisputeErr(e)
  )

  val assurances: StfStepWith[AssuranceOutputMarks] = liftStandardCtx(
    stf = (input: AssuranceInput, view, ctx) =>
      AssuranceTransition.stfViewWithValidators(input, view, ctx.preSafroleValidators),
    extractInput = ctx => InputExtractor.extractAssuranceInput(ctx.block),
    wrapError = (e: AssuranceErrorCode) => PipelineError.AssuranceErr(e)
  )

  // 4. Reports STF (special: has skipAncestryValidation + ancestor-set params)
  def reports(
      skipAncestryValidation: Boolean,
      ancestry: List[AncestorHeader] = List.empty
  ): StfStepWith[ReportOutputMarks] =
    liftStandard(
      stf = (input, view) =>
        ReportTransition.stfView(input, view, skipAncestryValidation, ancestry),
      extractInput = ctx =>
        ReportInput(
          guarantees = ctx.block.extrinsic.guarantees,
          slot = ctx.block.header.slot.value.toLong
        ),
      wrapError = (e: ReportErrorCode) => PipelineError.ReportErr(e)
    )

  def accumulation(
      sharedExecutor: Option[AccumulationExecutor] = None
  ): StfStepWith[AccumulationOutputData] =
    liftStandardCtx(
      stf = (input: AccumulationInput, view, ctx) =>
        AccumulationTransition.stfView(input, view, ctx.preTransitionTau, sharedExecutor),
      extractInput = ctx =>
        InputExtractor.extractAccumulationInput(
          ctx.availableReports,
          ctx.block.header.slot.value.toLong
        ),
      wrapError = (_: Nothing) => PipelineError.AccumulationErr("Accumulation failed")
    )

  def history(accumulateRoot: Hash): StfStep =
    liftStateOnly(
      stf = (input, view) => HistoryTransition.stfView(input, view),
      extractInput = ctx => InputExtractor.extractHistoryInput(ctx.block, accumulateRoot)
    )

  // 7. Authorization STF (state-only)
  val authorization: StfStep = liftStateOnly(
    stf = (input, view) => AuthorizationTransition.stfView(input, view),
    extractInput = ctx => InputExtractor.extractAuthInput(ctx.block)
  )

  val preimages: StfStepWith[Unit] = liftStandard(
    stf = (input, view) => PreimageTransition.stfView(input, view),
    extractInput = ctx =>
      InputExtractor.extractPreimageInput(ctx.block, ctx.block.header.slot.value.toLong),
    wrapError = (e: PreimageErrorCode) => PipelineError.PreimageErr(e)
  )

  val statistics: StfStepWith[Option[StatOutput]] = StateT {
    case (view, ctx) =>
      val input = InputExtractor.extractStatInput(ctx.block)
      val output = StatisticsTransition.stfView(input, view)
      Right(((view, ctx), output))
  }
