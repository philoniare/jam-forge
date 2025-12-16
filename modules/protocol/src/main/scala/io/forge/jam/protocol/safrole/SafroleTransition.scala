package io.forge.jam.protocol.safrole

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing, codec, StfResult}
import io.forge.jam.core.JamBytes.compareUnsigned
import io.forge.jam.core.codec.encode
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey}
import io.forge.jam.core.types.epoch.{ValidatorKey, EpochMark, EpochValidatorKey}
import io.forge.jam.core.types.tickets.{TicketEnvelope, TicketMark}
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.state.{JamState, ValidatorState, EntropyBuffer, SafroleGamma}
import monocle.syntax.all.*
import spire.math.UByte
import scodec.bits.ByteVector

/**
 * Safrole State Transition Function.
 *
 * Processes slot/epoch transitions, entropy accumulation, ticket processing
 * with Bandersnatch ring VRF verification, and validator set rotation.
 *
 * Key operations:
 * - Validate slot transitions (timeslot must be strictly monotonic)
 * - Process entropy accumulation using Blake2b-256 hashing
 * - Handle epoch transitions with validator set rotation
 * - Process ticket submissions with ring VRF proof verification
 * - Generate epoch marks on epoch boundary crossings
 * - Generate tickets marks when accumulator is full at cutoff
 * - Implement fallback sequence generation when insufficient tickets
 */
object SafroleTransition:

  /**
   * Execute the Safrole STF using unified JamState.
   *
   * Reads: tau, eta, validators (kappa, lambda, iota, gammaK), gamma (gammaA, gammaS, gammaZ), postOffenders
   * Writes: tau, eta, validators (kappa, lambda, iota, gammaK), gamma (gammaA, gammaS, gammaZ)
   *
   * @param input The safrole input containing slot, entropy, and tickets extrinsic.
   * @param state The unified JamState.
   * @param config The chain configuration.
   * @return Tuple of (updated JamState, output).
   */
  def stf(
    input: SafroleInput,
    state: JamState,
    config: ChainConfig
  ): (JamState, SafroleOutput) =
    // Extract SafroleState using lens bundle
    val preState = JamState.SafroleLenses.extract(state)

    // Execute the internal STF logic
    val (postState, output) = stfInternal(input, preState, config)

    // Apply results back using lens bundle
    val updatedState = JamState.SafroleLenses.apply(state, postState)

    (updatedState, output)

  /**
   * Internal Safrole STF implementation using SafroleState.
   *
   * @param input The safrole input containing slot, entropy, and tickets extrinsic.
   * @param preState The pre-transition state.
   * @param config The chain configuration.
   * @return Tuple of (post-transition state, output).
   */
  def stfInternal(
    input: SafroleInput,
    preState: SafroleState,
    config: ChainConfig
  ): (SafroleState, SafroleOutput) =
    try
      if input.slot <= preState.tau then
        (preState, StfResult.error(SafroleErrorCode.BadSlot))
      else
        processValidSlot(input, preState, config)
    catch
      case _: Exception =>
        (preState, StfResult.error(SafroleErrorCode.Reserved))

  /** Epoch timing context extracted from slot values */
  private case class EpochContext(
    prevEpoch: Long,
    prevPhase: Long,
    newEpoch: Long,
    newPhase: Long
  ):
    def crossingEpochBoundary: Boolean = newEpoch > prevEpoch
    def crossingTicketCutoff(cutoff: Int): Boolean =
      newEpoch == prevEpoch && prevPhase < cutoff && newPhase >= cutoff

  private def epochContext(preState: SafroleState, slot: Long, epochLength: Int): EpochContext =
    EpochContext(
      prevEpoch = preState.tau / epochLength,
      prevPhase = preState.tau % epochLength,
      newEpoch = slot / epochLength,
      newPhase = slot % epochLength
    )

  /**
   * Process a valid slot transition.
   */
  private def processValidSlot(
    input: SafroleInput,
    preState: SafroleState,
    config: ChainConfig
  ): (SafroleState, SafroleOutput) =
    val ctx = epochContext(preState, input.slot, config.epochLength)

    // Check for tickets_mark condition: same epoch, crossing cutoff, accumulator full
    val ticketsMark = Option.when(
      ctx.crossingTicketCutoff(config.ticketCutoff) &&
        preState.gammaA.size == config.epochLength
    )(transformTicketsSequence(preState.gammaA))

    // Save pre-epoch gammaZ for ticket verification
    val ticketVerificationGammaZ = preState.gammaZ

    // Handle epoch transition if needed
    val (stateAfterEpoch, epochMark) =
      if ctx.crossingEpochBoundary then
        val (newState, mark) = handleEpochTransition(preState, ctx, config)
        (newState, Some(mark))
      else
        (preState, None)

    // Process ticket submissions
    val ticketResult =
      if input.extrinsic.isEmpty then Right(stateAfterEpoch)
      else processExtrinsics(stateAfterEpoch, input.extrinsic, ctx.newPhase, ticketVerificationGammaZ, config)

    ticketResult match
      case Left(error) =>
        (stateAfterEpoch, StfResult.error(error))
      case Right(stateAfterTickets) =>
        // Process entropy accumulation and update timeslot
        val newEta0 = Hashing.blake2b256(preState.eta.head.bytes ++ input.entropy.bytes)
        val finalState = stateAfterTickets.copy(
          eta = newEta0 :: stateAfterTickets.eta.tail,
          tau = input.slot
        )
        (finalState, StfResult.success(SafroleOutputData(epochMark, ticketsMark)))

  /**
   * Handle epoch transition.
   *
   * @param preState The pre-transition state
   * @param ctx The epoch timing context
   * @param config The chain configuration
   * @return Tuple of (updated state, epoch mark)
   */
  private def handleEpochTransition(
    preState: SafroleState,
    ctx: EpochContext,
    config: ChainConfig
  ): (SafroleState, EpochMark) =
    val originalEta0 = preState.eta.head

    // Rotate entropy values
    val newEta = List(
      originalEta0, // eta[0] stays (will be updated later by entropy accumulation)
      originalEta0, // eta[1] = old eta[0]
      preState.eta(1), // eta[2] = old eta[1]
      preState.eta(2) // eta[3] = old eta[2]
    )

    // Rotate validator sets
    val newLambda = preState.kappa
    val newKappa = preState.gammaK

    // Load new pending validators with offender filtering
    val offenderSet = preState.postOffenders.map(o => JamBytes(o.bytes)).toSet
    val newGammaK = preState.iota.map { validator =>
      if offenderSet.contains(JamBytes(validator.ed25519.bytes)) then
        validator.copy(
          bandersnatch = BandersnatchPublicKey(new Array[Byte](32)),
          ed25519 = Ed25519PublicKey(new Array[Byte](32)),
          metadata = ByteVector.fill(ValidatorKey.MetadataSize.toLong)(0)
        )
      else
        validator
    }

    // Generate new ring root
    val newGammaZ = BandersnatchWrapper
      .generateRingRoot(newGammaK.map(_.bandersnatch), config.validatorCount)
      .getOrElse(preState.gammaZ)

    // Determine sealing sequence
    val useTickets = ctx.newEpoch == ctx.prevEpoch + 1 &&
      ctx.prevPhase >= config.ticketCutoff &&
      preState.gammaA.size == config.epochLength

    val newGammaS: TicketsOrKeys =
      if useTickets then
        TicketsOrKeys.Tickets(transformTicketsSequence(preState.gammaA))
      else
        // Fallback entropy is the NEW eta[2], which is old eta[1] after rotation
        TicketsOrKeys.Keys(generateFallbackSequence(preState.eta(1), newKappa, config.epochLength))

    // Generate epoch mark
    val epochMark = EpochMark(
      entropy = originalEta0,
      ticketsEntropy = preState.eta(1),
      validators = newGammaK.map(v => EpochValidatorKey(v.bandersnatch, v.ed25519))
    )

    val updatedState = preState.copy(
      eta = newEta,
      lambda = newLambda,
      kappa = newKappa,
      gammaK = newGammaK,
      gammaA = Nil,
      gammaS = newGammaS,
      gammaZ = newGammaZ
    )

    (updatedState, epochMark)

  /**
   * Process ticket submissions.
   *
   * @param postState The current state
   * @param tickets The list of ticket envelopes to process
   * @param phase The current phase within epoch
   * @param verificationGammaZ The ring commitment to use for verification
   * @param config The chain configuration
   * @return Either an error or the updated state
   */
  private def processExtrinsics(
    postState: SafroleState,
    tickets: List[TicketEnvelope],
    phase: Long,
    verificationGammaZ: JamBytes,
    config: ChainConfig
  ): Either[SafroleErrorCode, SafroleState] =
    // Skip if in epoch tail
    if phase >= config.ticketCutoff then
      Left(SafroleErrorCode.UnexpectedTicket)
    else
      processTickets(postState, tickets, verificationGammaZ, config, None, List.empty)

  /**
   * Process tickets recursively using tail recursion.
   */
  @scala.annotation.tailrec
  private def processTickets(
    postState: SafroleState,
    remainingTickets: List[TicketEnvelope],
    verificationGammaZ: JamBytes,
    config: ChainConfig,
    previousId: Option[JamBytes],
    accumulatedTickets: List[TicketMark]
  ): Either[SafroleErrorCode, SafroleState] =
    remainingTickets match
      case Nil =>
        // All tickets processed, update accumulator
        if accumulatedTickets.nonEmpty then
          val combinedTickets = (postState.gammaA ++ accumulatedTickets)
            .sortBy(_.id)
            .take(config.epochLength)
          Right(postState.copy(gammaA = combinedTickets))
        else
          Right(postState)

      case ticket :: rest =>
        // Check attempt value - valid attempts are 0 to (ticketsPerValidator - 1)
        // ticketsPerValidator is the A parameter from Gray Paper
        if ticket.attempt.toInt >= config.ticketsPerValidator then
          Left(SafroleErrorCode.BadTicketAttempt)
        else
          // Verify ring VRF proof using pre-epoch ring root
          BandersnatchWrapper.verifyRingProof(
            ticket.signature,
            verificationGammaZ,
            postState.eta(2),
            ticket.attempt,
            config.validatorCount
          ) match
            case None =>
              Left(SafroleErrorCode.BadTicketProof)
            case Some(ticketBody) =>
              val currentId = ticketBody.id

              // Check for duplicates in accumulator
              if postState.gammaA.exists(t => t.id == currentId) then
                Left(SafroleErrorCode.DuplicateTicket)
              // Check for duplicates in new tickets being added
              else if accumulatedTickets.exists(t => t.id == currentId) then
                Left(SafroleErrorCode.DuplicateTicket)
              // Check ordering against previous ticket
              else if previousId.exists(_ >= currentId) then
                Left(SafroleErrorCode.BadTicketOrder)
              else
                processTickets(
                  postState,
                  rest,
                  verificationGammaZ,
                  config,
                  Some(currentId),
                  accumulatedTickets :+ ticketBody
                )

  /**
   * Generate fallback sealing sequence when epoch changes without sufficient tickets.
   * Uses deterministic validator selection based on entropy.
   */
  private def generateFallbackSequence(
    entropy: Hash,
    validators: List[ValidatorKey],
    epochLength: Int
  ): List[BandersnatchPublicKey] =
    val keys = validators.map(_.bandersnatch)
    (0 until epochLength).map { i =>
      val slotEntropy = Hashing.blake2b256(entropy.bytes ++ codec.encodeU32LE(spire.math.UInt(i)))
      val index = (codec.decodeU32LE(slotEntropy.bytes.take(4).toArray).toLong % validators.size).toInt
      keys(index)
    }.toList

  /**
   * Transform ticket accumulator to sealing sequence using outside-in interleaving.
   * Pattern: [s_0, s_|s|-1, s_1, s_|s|-2, ...]
   */
  private def transformTicketsSequence(tickets: List[TicketMark]): List[TicketMark] =
    val indexed = tickets.toIndexedSeq
    val n = indexed.size
    (0 until n).flatMap { i =>
      val left = i / 2
      val right = n - 1 - i / 2
      if i % 2 == 0 then Some(indexed(left))
      else if left != right then Some(indexed(right))
      else None
    }.toList
