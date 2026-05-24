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
  AccumulationOutputData,
  AccumulationExecutor
}
import io.forge.jam.protocol.history.HistoryTransition
import io.forge.jam.protocol.authorization.AuthorizationTransition
import io.forge.jam.protocol.preimage.PreimageTransition
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

  val assurances: StfStepWith[AssuranceOutputMarks] = StateT {
    case (view, ctx) =>
      val input = InputExtractor.extractAssuranceInput(ctx.block)
      val output = AssuranceTransition.stfViewWithValidators(
        input,
        view,
        ctx.preSafroleValidators
      )
      output match
        case Right(out) => Right(((view, ctx), out))
        case Left(err)  => Left(PipelineError.AssuranceErr(err))
  }

  // 4. Reports STF (special: has skipAncestryValidation param)
  def reports(skipAncestryValidation: Boolean): StfStepWith[ReportOutputMarks] = StateT {
    case (view, ctx) =>
      val input = ReportInput(
        guarantees = ctx.block.extrinsic.guarantees,
        slot = ctx.block.header.slot.value.toLong
      )
      val output = ReportTransition.stfView(input, view, skipAncestryValidation)
      output match
        case Right(out) => Right(((view, ctx), out))
        case Left(err)  => Left(PipelineError.ReportErr(err))
  }

  def accumulation(
      sharedExecutor: Option[AccumulationExecutor] = None
  ): StfStepWith[AccumulationOutputData] = StateT {
    case (view, ctx) =>
      val input = InputExtractor.extractAccumulationInput(
        ctx.availableReports,
        ctx.block.header.slot.value.toLong
      )
      val output = AccumulationTransition.stfView(
        input,
        view,
        ctx.preTransitionTau,
        sharedExecutor
      )
      output match
        case Right(out) => Right(((view, ctx), out))
        case Left(_)    => Left(PipelineError.AccumulationErr("Accumulation failed"))
  }

  def history(accumulateRoot: Hash): StfStep = StateT {
    case (view, ctx) =>
      val input = InputExtractor.extractHistoryInput(ctx.block, accumulateRoot)
      HistoryTransition.stfView(input, view)
      Right(((view, ctx), ()))
  }

  // 7. Authorization STF (state-only)
  val authorization: StfStep = liftStateOnly(
    stf = (input, view) => AuthorizationTransition.stfView(input, view),
    extractInput = ctx => InputExtractor.extractAuthInput(ctx.block)
  )

  val preimages: StfStepWith[Unit] = StateT {
    case (view, ctx) =>
      val input = InputExtractor.extractPreimageInput(
        ctx.block,
        ctx.block.header.slot.value.toLong
      )
      val output = PreimageTransition.stfView(input, view)
      output match
        case Left(err) => Left(PipelineError.PreimageErr(err))
        case Right(_)  => Right(((view, ctx), ()))
  }

  val statistics: StfStepWith[Option[StatOutput]] = StateT {
    case (view, ctx) =>
      val input = InputExtractor.extractStatInput(ctx.block)
      val output = StatisticsTransition.stfView(input, view)
      Right(((view, ctx), output))
  }
