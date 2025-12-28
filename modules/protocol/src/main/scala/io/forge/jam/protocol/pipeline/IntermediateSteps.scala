package io.forge.jam.protocol.pipeline

import cats.data.StateT
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.crypto.{BandersnatchVrf, SigningContext}
import io.forge.jam.protocol.state.JamState
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.dispute.DisputeTypes.DisputeOutputMarks
import io.forge.jam.protocol.pipeline.PipelineTypes.*
import io.forge.jam.protocol.pipeline.StfLifters.*
import monocle.syntax.all.*
import _root_.scodec.Codec

/**
 * Intermediate validation and state modification steps for the pipeline.
 */
object IntermediateSteps:

  /**
   * Validate block author against sealing sequence (post-Safrole).
   * Uses IETF VRF verification with the block seal.
   */
  val validateBlockSeal: StfStep = validate { (state, ctx) =>
    val block = ctx.block
    val config = ctx.config
    val slotIndex = ((block.header.slot.value.toLong & 0xffffffffL) % config.epochLength).toInt
    val authorIndex = block.header.authorIndex.value.toInt

    // Bounds check for author index
    if authorIndex < 0 || authorIndex >= state.validators.current.length then
      Left(
        PipelineError.HeaderVerificationErr(s"InvalidAuthorIndex: $authorIndex >= ${state.validators.current.length}")
      )
    else
      val blockAuthorKey = state.validators.current(authorIndex).bandersnatch

      // Get entropy eta3 for VRF input (use post-state entropy)
      val entropy =
        if state.entropy.pool.length > 3
        then state.entropy.pool(3).bytes
        else new Array[Byte](32)

      // Encode header for aux data (unsigned header = full header minus 96-byte seal at the end)
      val fullHeaderBytes =
        summon[Codec[io.forge.jam.core.types.header.Header]].encode(block.header).require.bytes.toArray
      val encodedHeader = fullHeaderBytes.dropRight(96)

      state.gamma.s match
        case TicketsOrKeys.Keys(keys) =>
          // Fallback keys mode: verify key matches AND verify seal signature
          if slotIndex < 0 || slotIndex >= keys.length then
            Left(PipelineError.HeaderVerificationErr(s"InvalidSlotIndex: $slotIndex >= ${keys.length}"))
          else
            val expectedKey = keys(slotIndex)
            if !java.util.Arrays.equals(expectedKey.bytes, blockAuthorKey.bytes) then
              Left(PipelineError.HeaderVerificationErr("UnexpectedAuthor"))
            else
              val vrfInput = SigningContext.fallbackSealInputData(entropy)
              val vrfResult = BandersnatchVrf.ietfVrfVerify(
                blockAuthorKey.bytes,
                vrfInput,
                encodedHeader,
                block.header.seal.toArray
              )
              if vrfResult.isEmpty then
                Left(PipelineError.InvalidBlockSeal)
              else
                Right(())

        case TicketsOrKeys.Tickets(tickets) =>
          // Tickets mode: verify VRF and check ticket.id == vrfOutput
          if slotIndex < 0 || slotIndex >= tickets.length then
            Left(PipelineError.HeaderVerificationErr(s"InvalidSlotIndex: $slotIndex >= ${tickets.length}"))
          else
            val ticket = tickets(slotIndex)
            val vrfInput = SigningContext.safroleTicketInputData(entropy, ticket.attempt.toByte)

            val vrfResult = BandersnatchVrf.ietfVrfVerify(
              blockAuthorKey.bytes,
              vrfInput,
              encodedHeader,
              block.header.seal.toArray
            )

            vrfResult match
              case None => Left(PipelineError.InvalidBlockSeal)
              case Some(vrfOutput) =>
                if !java.util.Arrays.equals(ticket.id.toArray, vrfOutput) then
                  Left(PipelineError.HeaderVerificationErr("InvalidAuthorTicket"))
                else
                  Right(())
  }

  /**
   * Validate epoch mark matches Safrole output.
   */
  val validateEpochMark: StfStep = validate { (_, ctx) =>
    val safroleEpochMark = ctx.safroleOutput.flatMap(_.epochMark)
    if safroleEpochMark != ctx.block.header.epochMark then
      Left(PipelineError.InvalidEpochMark)
    else
      Right(())
  }

  /**
   * Validate tickets mark matches Safrole output.
   */
  val validateTicketsMark: StfStep = validate { (_, ctx) =>
    val safroleTicketsMark = ctx.safroleOutput.flatMap(_.ticketsMark)
    if safroleTicketsMark != ctx.block.header.ticketsMark then
      Left(PipelineError.InvalidTicketsMark)
    else
      Right(())
  }

  /**
   * Store dispute output (offenders mark) in context.
   */
  def storeDisputeOutput(output: DisputeOutputMarks): StfStep =
    modifyContext(_.copy(disputeOffendersMark = output.offenders))

  /**
   * Validate offenders mark matches dispute output.
   */
  val validateOffendersMark: StfStep = validate { (_, ctx) =>
    val computedBytes = ctx.disputeOffendersMark.map(_.toByteVector)
    val headerBytes = ctx.block.header.offendersMark.map(_.toByteVector)
    if computedBytes != headerBytes then
      Left(PipelineError.InvalidOffendersMark)
    else
      Right(())
  }

  /**
   * Update recent history's last item with parent state root (before Reports).
   */
  val updateRecentHistoryPartial: StfStep = modifyState { (state, ctx) =>
    val recentHistory = state.beta
    if recentHistory.history.isEmpty then
      state
    else
      val history = recentHistory.history.toArray
      val lastItem = history.last
      history(history.length - 1) = lastItem.copy(stateRoot = ctx.block.header.parentStateRoot)
      state.focus(_.beta).replace(recentHistory.copy(history = history.toList))
  }

  /**
   * Store Safrole output in context.
   */
  def storeSafroleOutput(output: SafroleOutputData): StfStep =
    modifyContext(_.copy(safroleOutput = Some(output)))

  /**
   * Store available reports in context (from Assurance output).
   */
  def storeAvailableReports(reports: List[WorkReport]): StfStep =
    modifyContext(_.copy(availableReports = reports))

  /**
   * Store accumulate root in context.
   */
  def storeAccumulateRoot(root: Hash): StfStep =
    modifyContext(_.copy(accumulateRoot = Some(root)))

  /**
   * Store accumulation stats in context.
   */
  def storeAccumulationStats(stats: Map[Long, (Long, Int)]): StfStep =
    modifyContext(_.copy(accumulationStats = stats))

  /**
   * Store last accumulation outputs (commitments) in state.
   */
  def storeLastAccumulationOutputs(commitments: List[(Long, JamBytes)]): StfStep =
    modifyState((state, _) => state.focus(_.lastAccumulationOutputs).replace(commitments))

  /**
   * Capture pre-accumulation rawServiceDataByStateKey for preimages validation.
   * Per GP §12.1, preimages validation uses the state before accumulation.
   */
  val capturePreAccumulationState: StfStep = StateT {
    case (state, ctx) =>
      Right(((state, ctx.copy(preAccumulationRawServiceData = Some(state.rawServiceDataByStateKey))), ()))
  }

  /**
   * Temporarily set tau to pre-transition value for Statistics.
   */
  val setPreTransitionTau: StfStep = modifyState((state, ctx) => state.focus(_.tau).replace(ctx.preTransitionTau))

  /**
   * Restore tau to post-transition value after Statistics.
   */
  def restorePostTransitionTau(postTau: Long): StfStep = modifyState { (state, _) =>
    state.focus(_.tau).replace(postTau)
  }
