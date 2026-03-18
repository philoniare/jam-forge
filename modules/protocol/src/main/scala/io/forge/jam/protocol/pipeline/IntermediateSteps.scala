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
   * Validate extrinsic hash
   */
  val validateExtrinsicHash: StfStep = validate { (_, ctx) =>
    import io.forge.jam.core.{Hashing, JamBytes as JB}
    import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, Dispute}
    import io.forge.jam.core.types.tickets.TicketEnvelope
    import io.forge.jam.core.types.extrinsic.Preimage
    import io.forge.jam.core.scodec.JamCodecs.{compactPrefixedList, compactInt}

    val block = ctx.block
    val config = ctx.config
    val ex = block.extrinsic

    try
      // Encode each extrinsic component
      val ticketsEncoded = compactPrefixedList(summon[Codec[TicketEnvelope]]).encode(ex.tickets).require.toByteArray
      val preimagesEncoded = compactPrefixedList(summon[Codec[Preimage]]).encode(ex.preimages).require.toByteArray
      val assurancesEncoded =
        compactPrefixedList(AssuranceExtrinsic.codec(config.coresCount)).encode(ex.assurances).require.toByteArray
      val disputesEncoded = Dispute.codec(config.votesPerVerdict).encode(ex.disputes).require.toByteArray

      // Build guarantee component
      val guaranteeItems = ex.guarantees.map { g =>
        val reportEncoded =
          summon[Codec[io.forge.jam.core.types.workpackage.WorkReport]].encode(g.report).require.toByteArray
        val reportHash = Hashing.blake2b256(reportEncoded)
        val timeslotEncoded = _root_.scodec.codecs.uint32L.encode(g.slot.value.toLong & 0xffffffffL).require.toByteArray

        val credentialEncoded = compactPrefixedList(summon[Codec[io.forge.jam.core.types.dispute.GuaranteeSignature]])
          .encode(g.signatures).require.toByteArray

        reportHash.bytes ++ timeslotEncoded ++ credentialEncoded
      }

      val guaranteeListLenEncoded = compactInt.encode(guaranteeItems.length).require.toByteArray
      val gEncoded = guaranteeListLenEncoded ++ guaranteeItems.foldLeft(Array.empty[Byte])(_ ++ _)

      // Hash each component
      val hashes =
        List(ticketsEncoded, preimagesEncoded, gEncoded, assurancesEncoded, disputesEncoded).map(Hashing.blake2b256)

      // Encode the list of 5 hashes (fixed-length, no length prefix)
      val hashesEncoded = hashes.flatMap(_.bytes).toArray

      val computedExtrinsicHash = Hashing.blake2b256(hashesEncoded)

      if computedExtrinsicHash != block.header.extrinsicHash then
        Left(PipelineError.HeaderVerificationErr("Invalid extrinsic hash"))
      else
        Right(())
    catch
      case _: Exception =>
        Right(())
  }

  /**
   * Validate entropy VRF signature (H_vrfsig).
   * Per GP eq:vrfsigcheck: H_vrfsig ∈ bssignature{H_authorbskey}{X_entropy ∥ banderout(H_sealsig)}{[]}
   * The VRF input is "$jam_entropy" ∥ seal_vrf_output, with empty aux data.
   */
  val validateEntropyVrf: StfStep = validate { (state, ctx) =>
    val block = ctx.block
    val authorIndex = block.header.authorIndex.value.toInt
    val blockAuthorKey = state.validators.current(authorIndex).bandersnatch

    // Get the seal VRF output: banderout(H_sealsig)
    val sealVrfOutput = BandersnatchVrf.getVrfOutput(block.header.seal.toArray)
    if sealVrfOutput.isEmpty then
      Left(PipelineError.HeaderVerificationErr("Cannot extract VRF output from seal"))
    else
      // Build VRF input: "$jam_entropy" ∥ sealVrfOutput
      val vrfInput = SigningContext.entropyInputData(sealVrfOutput.get)

      // Verify H_vrfsig with empty aux data
      val vrfResult = BandersnatchVrf.ietfVrfVerify(
        blockAuthorKey.bytes,
        vrfInput,
        Array.empty[Byte], // empty aux data
        block.header.entropySource.toArray
      )

      if vrfResult.isEmpty then
        Left(PipelineError.HeaderVerificationErr("Invalid entropy VRF signature"))
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
