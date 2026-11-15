package io.forge.jam.protocol.pipeline

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}
import io.forge.jam.core.types.block.Block
import io.forge.jam.crypto.{BandersnatchVrf, SigningContext}
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.state.TrieBackedJamState

object HeaderChecks:

  def blockSeal(
      block: Block,
      header: EncodedHeader,
      view: TrieBackedJamState,
      checks: PendingChecks
  ): Either[PipelineError, Unit] =
    val config = view.config
    val slotIndex = ((block.header.slot.toLong & 0xffffffffL) % config.epochLength).toInt
    val authorIndex = block.header.authorIndex.toInt

    val kappa = view.validators.current
    val pool = view.entropy.pool
    if authorIndex < 0 || authorIndex >= kappa.length then
      Left(PipelineError.HeaderErr(HeaderErrorCode.InvalidAuthorIndex))
    else if pool.length != 4 then
      throw new IllegalStateException(s"Corrupt entropy pool: length ${pool.length} != 4")
    else
      val blockAuthorKey = kappa(authorIndex).bandersnatch

      val entropy = pool(3).bytes

      val encodedHeader = header.unsealed

      view.gamma.st match
        case TicketsOrKeys.Keys(keys) =>
          if slotIndex < 0 || slotIndex >= keys.length then
            throw new IllegalStateException(s"Sealer sequence too short: $slotIndex >= ${keys.length}")
          else
            val expectedKey = keys(slotIndex)
            if expectedKey != blockAuthorKey then
              Left(PipelineError.HeaderErr(HeaderErrorCode.UnexpectedAuthor))
            else
              val vrfInput = SigningContext.fallbackSealInputData(entropy)
              checks.start {
                val vrfResult = BandersnatchVrf.ietfVrfVerify(
                  blockAuthorKey.bytes,
                  vrfInput,
                  encodedHeader,
                  block.header.seal.toArray
                )
                if vrfResult.isEmpty then Some(PipelineError.HeaderErr(HeaderErrorCode.BadSealSignature)) else None
              }
              Right(())

        case TicketsOrKeys.Tickets(tickets) =>
          if slotIndex < 0 || slotIndex >= tickets.length then
            throw new IllegalStateException(s"Sealer sequence too short: $slotIndex >= ${tickets.length}")
          else
            val ticket = tickets(slotIndex)
            val vrfInput = SigningContext.safroleTicketInputData(entropy, ticket.attempt.toByte)
            checks.start {
              val vrfResult = BandersnatchVrf.ietfVrfVerify(
                blockAuthorKey.bytes,
                vrfInput,
                encodedHeader,
                block.header.seal.toArray
              )
              vrfResult match
                case None => Some(PipelineError.HeaderErr(HeaderErrorCode.BadSealSignature))
                case Some(vrfOutput) =>
                  if !java.util.Arrays.equals(ticket.id.toArray, vrfOutput) then
                    Some(PipelineError.HeaderErr(HeaderErrorCode.InvalidAuthorTicket))
                  else None
            }
            Right(())

  def extrinsicHash(block: Block, config: ChainConfig): Either[PipelineError, Unit] =
    try
      // Shared with block authoring (ExtrinsicHashing) — logic unchanged.
      val computedExtrinsicHash =
        ExtrinsicHashing.computeExtrinsicHash(block.extrinsic, config)
      if computedExtrinsicHash != block.header.extrinsicHash then
        Left(PipelineError.HeaderErr(HeaderErrorCode.InvalidExtrinsicHash))
      else Right(())
    catch case _: Exception => Left(PipelineError.HeaderErr(HeaderErrorCode.InvalidExtrinsicHash))

  def entropyVrf(block: Block, view: TrieBackedJamState, checks: PendingChecks): Either[PipelineError, Unit] =
    val authorIndex = block.header.authorIndex.toInt
    val kappa = view.validators.current
    if authorIndex < 0 || authorIndex >= kappa.length then
      Left(PipelineError.HeaderErr(HeaderErrorCode.InvalidAuthorIndex))
    else
      val blockAuthorKey = kappa(authorIndex).bandersnatch

      // The seal's output point is decoded in the check too, off the importing thread. A seal without one fails the
      // seal check, which was started first and so is the error reported, as when this was decided here.
      checks.start {
        BandersnatchVrf.getVrfOutput(block.header.seal.toArray) match
          case None => Some(PipelineError.HeaderErr(HeaderErrorCode.BadSealSignature))
          case Some(sealVrfOutput) =>
            val vrfResult = BandersnatchVrf.ietfVrfVerify(
              blockAuthorKey.bytes,
              SigningContext.entropyInputData(sealVrfOutput),
              Array.empty[Byte],
              block.header.entropySource.toArray
            )
            if vrfResult.isEmpty then Some(PipelineError.HeaderErr(HeaderErrorCode.BadEntropySignature)) else None
      }
      Right(())

  def epochMark(block: Block, safrole: SafroleOutputData): Either[PipelineError, Unit] =
    if safrole.epochMark != block.header.epochMark then
      Left(PipelineError.HeaderErr(HeaderErrorCode.InvalidEpochMark))
    else
      Right(())

  def ticketsMark(block: Block, safrole: SafroleOutputData): Either[PipelineError, Unit] =
    if safrole.ticketsMark != block.header.ticketsMark then
      Left(PipelineError.HeaderErr(HeaderErrorCode.InvalidTicketsMark))
    else
      Right(())

  /** The header's offenders mark must be the keys of this block's culprits and faults */
  def offendersMark(block: Block, newOffenders: List[Ed25519PublicKey]): Either[PipelineError, Unit] =
    // The mark holds the keys as hashes; a key never equals a hash, so compare the bytes.
    val expected = newOffenders.map(k => Hash(k.toArrayUnsafe))
    if expected != block.header.offendersMark then
      Left(PipelineError.InvalidOffendersMark)
    else
      Right(())
