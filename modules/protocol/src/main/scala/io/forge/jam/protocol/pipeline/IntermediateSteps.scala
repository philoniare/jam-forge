package io.forge.jam.protocol.pipeline

import cats.data.StateT
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.crypto.{BandersnatchVrf, SigningContext}
import io.forge.jam.protocol.state.ServiceStorageView
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.dispute.DisputeTypes.DisputeOutputMarks
import io.forge.jam.protocol.pipeline.PipelineTypes.*
import io.forge.jam.protocol.pipeline.StfLifters.*
import _root_.scodec.Codec

object IntermediateSteps:

  val validateBlockSeal: StfStep = validate { (view, ctx) =>
    val block = ctx.block
    val config = ctx.config
    val slotIndex = ((block.header.slot.value.toLong & 0xffffffffL) % config.epochLength).toInt
    val authorIndex = block.header.authorIndex.value.toInt

    val kappa = view.validators.current
    val pool = view.entropy.pool
    if authorIndex < 0 || authorIndex >= kappa.length then
      Left(
        PipelineError.HeaderVerificationErr(s"InvalidAuthorIndex: $authorIndex >= ${kappa.length}")
      )
    else if pool.length != 4 then
      Left(
        PipelineError.HeaderVerificationErr(s"CorruptEntropyPool: length ${pool.length} != 4")
      )
    else
      val blockAuthorKey = kappa(authorIndex).bandersnatch

      val entropy = pool(3).bytes

      val fullHeaderBytes =
        summon[Codec[io.forge.jam.core.types.header.Header]].encode(block.header).require.bytes.toArray
      val encodedHeader = fullHeaderBytes.dropRight(96)

      view.gamma.st match
        case TicketsOrKeys.Keys(keys) =>
          if slotIndex < 0 || slotIndex >= keys.length then
            Left(PipelineError.HeaderVerificationErr(s"InvalidSlotIndex: $slotIndex >= ${keys.length}"))
          else
            val expectedKey = keys(slotIndex)
            if expectedKey != blockAuthorKey then
              Left(PipelineError.HeaderVerificationErr("UnexpectedAuthor"))
            else
              val vrfInput = SigningContext.fallbackSealInputData(entropy)
              val vrfResult = BandersnatchVrf.ietfVrfVerify(
                blockAuthorKey.bytes,
                vrfInput,
                encodedHeader,
                block.header.seal.toArray
              )
              if vrfResult.isEmpty then Left(PipelineError.InvalidBlockSeal)
              else Right(())

        case TicketsOrKeys.Tickets(tickets) =>
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
                else Right(())
  }

  val validateExtrinsicHash: StfStep = validate { (_, ctx) =>
    val block = ctx.block
    try
      // Shared with block authoring (ExtrinsicHashing) — logic unchanged.
      val computedExtrinsicHash =
        ExtrinsicHashing.computeExtrinsicHash(block.extrinsic, ctx.config)
      if computedExtrinsicHash != block.header.extrinsicHash then
        Left(PipelineError.HeaderVerificationErr("Invalid extrinsic hash"))
      else Right(())
    catch
      case e: Exception =>
        Left(PipelineError.HeaderVerificationErr(s"Extrinsic hash validation failed: ${e.getMessage}"))
  }

  val validateEntropyVrf: StfStep = validate { (view, ctx) =>
    val block = ctx.block
    val authorIndex = block.header.authorIndex.value.toInt
    val kappa = view.validators.current
    if authorIndex < 0 || authorIndex >= kappa.length then
      Left(PipelineError.HeaderVerificationErr(s"InvalidAuthorIndex: $authorIndex >= ${kappa.length}"))
    else
      val blockAuthorKey = kappa(authorIndex).bandersnatch

      val sealVrfOutput = BandersnatchVrf.getVrfOutput(block.header.seal.toArray)
      if sealVrfOutput.isEmpty then
        Left(PipelineError.HeaderVerificationErr("Cannot extract VRF output from seal"))
      else
        val vrfInput = SigningContext.entropyInputData(sealVrfOutput.get)
        val vrfResult = BandersnatchVrf.ietfVrfVerify(
          blockAuthorKey.bytes,
          vrfInput,
          Array.empty[Byte],
          block.header.entropySource.toArray
        )
        if vrfResult.isEmpty then
          Left(PipelineError.HeaderVerificationErr("Invalid entropy VRF signature"))
        else
          Right(())
  }

  val validateEpochMark: StfStep = validate { (_, ctx) =>
    val safroleEpochMark = ctx.safroleOutput.flatMap(_.epochMark)
    if safroleEpochMark != ctx.block.header.epochMark then
      Left(PipelineError.InvalidEpochMark)
    else
      Right(())
  }

  val validateTicketsMark: StfStep = validate { (_, ctx) =>
    val safroleTicketsMark = ctx.safroleOutput.flatMap(_.ticketsMark)
    if safroleTicketsMark != ctx.block.header.ticketsMark then
      Left(PipelineError.InvalidTicketsMark)
    else
      Right(())
  }

  def storeDisputeOutput(output: DisputeOutputMarks): StfStep =
    modifyContext(_.copy(disputeOffendersMark = output.offenders))

  val validateOffendersMark: StfStep = validate { (_, ctx) =>
    val computedBytes = ctx.disputeOffendersMark.map(_.toByteVector)
    val headerBytes = ctx.block.header.offendersMark.map(_.toByteVector)
    if computedBytes != headerBytes then
      Left(PipelineError.InvalidOffendersMark)
    else
      Right(())
  }

  val updateRecentHistoryPartial: StfStep = modifyState { (view, ctx) =>
    val recentHistory = view.beta
    val history = recentHistory.history
    if history.nonEmpty then
      val updatedHistory = history.updated(
        history.length - 1,
        history.last.copy(stateRoot = ctx.block.header.parentStateRoot)
      )
      view.beta = recentHistory.copy(history = updatedHistory)
  }

  def storeSafroleOutput(output: SafroleOutputData): StfStep =
    modifyContext(_.copy(safroleOutput = Some(output)))

  def storeAvailableReports(reports: List[WorkReport]): StfStep =
    modifyContext(_.copy(availableReports = reports))

  def storeAccumulateRoot(root: Hash): StfStep =
    modifyContext(_.copy(accumulateRoot = Some(root)))

  def storeAccumulationStats(stats: Map[Long, (Long, Int)]): StfStep =
    modifyContext(_.copy(accumulationStats = stats))

  def storeLastAccumulationOutputs(
      commitments: List[(Long, JamBytes)]
  ): StfStep =
    modifyState((view, _) => view.lastAccumulationOutputs = commitments)

  def savepointStorageView(view: Option[ServiceStorageView]): StfStep =
    StateT { case (state, ctx) =>
      view.foreach(_.savepoint())
      Right(((state, ctx), ()))
    }

  def discardStorageViewCheckpoint(view: Option[ServiceStorageView]): StfStep =
    StateT { case (state, ctx) =>
      view.foreach(_.discardCheckpoint())
      Right(((state, ctx), ()))
    }

  val setPreTransitionTau: StfStep = modifyState { (view, ctx) =>
    view.timeslot = ctx.preTransitionTau
  }

  val setPosteriorOffenders: StfStep = modifyState { (view, ctx) =>
    view.postOffenders = ctx.posteriorOffenders
  }

  def restorePostTransitionTau(postTau: Long): StfStep = modifyState {
    (view, _) => view.timeslot = postTau
  }
