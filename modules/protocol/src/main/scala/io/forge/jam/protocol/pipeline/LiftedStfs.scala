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
import io.forge.jam.protocol.accumulation.{AccumulationTransition, AccumulationOutputData}
import io.forge.jam.protocol.history.HistoryTransition
import io.forge.jam.protocol.history.HistoryTypes.*
import io.forge.jam.protocol.authorization.AuthorizationTransition
import io.forge.jam.protocol.authorization.AuthorizationTypes.*
import io.forge.jam.protocol.preimage.PreimageTransition
import io.forge.jam.protocol.preimage.PreimageTypes.*
import io.forge.jam.protocol.statistics.StatisticsTransition
import io.forge.jam.protocol.statistics.StatisticsTypes.*
import io.forge.jam.protocol.traces.InputExtractor
import io.forge.jam.protocol.pipeline.PipelineTypes.*
import io.forge.jam.protocol.pipeline.StfLifters.*

/**
 * All STFs lifted into the pipeline composition layer.
 */
object LiftedStfs:

  // 1. Safrole STF
  val safrole: StfStepWith[SafroleOutputData] = liftStandard(
    stf = SafroleTransition.stf,
    extractInput = ctx => InputExtractor.extractSafroleInput(ctx.block),
    wrapError = (e: SafroleErrorCode) => PipelineError.SafroleErr(e)
  )

  // 2. Disputes STF
  val disputes: StfStepWith[DisputeOutputMarks] = liftStandard(
    stf = DisputeTransition.stf,
    extractInput = ctx => InputExtractor.extractDisputeInput(ctx.block),
    wrapError = (e: DisputeErrorCode) => PipelineError.DisputeErr(e)
  )

  // 3. Assurances STF
  val assurances: StfStepWith[AssuranceOutputMarks] = liftStandard(
    stf = AssuranceTransition.stf,
    extractInput = ctx => InputExtractor.extractAssuranceInput(ctx.block),
    wrapError = (e: AssuranceErrorCode) => PipelineError.AssuranceErr(e)
  )

  // 4. Reports STF (special: has skipAncestryValidation param)
  def reports(skipAncestryValidation: Boolean): StfStepWith[ReportOutputMarks] = liftReport(
    stf = ReportTransition.stf,
    extractInput = ctx => ReportInput(
      guarantees = ctx.block.extrinsic.guarantees,
      slot = ctx.block.header.slot.value.toLong
    ),
    wrapError = (e: ReportErrorCode) => PipelineError.ReportErr(e),
    skipAncestryValidation = skipAncestryValidation
  )

  // 5. Accumulation STF (never fails - returns Either[Nothing, AccumulationOutputData])
  val accumulation: StfStepWith[AccumulationOutputData] = StateT { case (state, ctx) =>
    val input = InputExtractor.extractAccumulationInput(ctx.availableReports, ctx.block.header.slot.value.toLong)
    val (newState, result) = AccumulationTransition.stf(input, state, ctx.config)
    // Accumulation never returns Left, so we just extract Right
    result match
      case Right(output) => Right(((newState, ctx), output))
      case Left(_) => Left(PipelineError.AccumulationErr("Accumulation failed"))
  }

  // 6. History STF (state-only, needs accumulateRoot from context)
  def history(accumulateRoot: Hash): StfStep = liftStateOnly(
    stf = HistoryTransition.stf,
    extractInput = ctx => InputExtractor.extractHistoryInput(ctx.block, accumulateRoot)
  )

  // 7. Authorization STF (state-only)
  val authorization: StfStep = liftStateOnly(
    stf = AuthorizationTransition.stf,
    extractInput = ctx => InputExtractor.extractAuthInput(ctx.block)
  )

  // 8. Preimages STF (no config)
  val preimages: StfStepWith[Unit] = liftNoConfig(
    stf = PreimageTransition.stf,
    extractInput = ctx => InputExtractor.extractPreimageInput(ctx.block, ctx.block.header.slot.value.toLong),
    wrapError = (e: PreimageErrorCode) => PipelineError.PreimageErr(e)
  )

  // 9. Statistics STF (returns Option, not Either - wrapping specially)
  val statistics: StfStepWith[Option[StatOutput]] = StateT { case (state, ctx) =>
    val input = InputExtractor.extractStatInput(ctx.block)
    val (newState, output) = StatisticsTransition.stf(input, state, ctx.config)
    Right(((newState, ctx), output))
  }
