package io.forge.jam.protocol.safrole

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing, StfResult}
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey}
import io.forge.jam.core.codec.codecs.uint32L
import io.forge.jam.core.types.epoch.{ValidatorKey, EpochMark, EpochValidatorKey}
import io.forge.jam.core.types.tickets.{TicketEnvelope, TicketMark}
import io.forge.jam.core.Parallel
import io.forge.jam.crypto.BandersnatchVrf
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.SafroleBridge
import java.util.concurrent.CompletableFuture
import scala.math.Ordering.Implicits.infixOrderingOps
import com.typesafe.scalalogging.StrictLogging

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
object SafroleTransition extends StrictLogging:

  /**
   * Signals an unrecoverable native-crypto fault
   */
  final class SafroleCryptoFailure(message: String) extends RuntimeException(message)

  /**
   * Receives the ticket proof checks of a successful transition, still running. Each completes with whether its proof
   * verifies; the caller must reject the block with [[SafroleErrorCode.BadTicketProof]] if any is false.
   */
  type DeferredProofs = List[CompletableFuture[Boolean]] => Unit

  /**
   * Execute the Safrole STF over a typed view.
   *
   * Reads: tau, eta, validators (kappa, lambda, iota, gammaK), gamma (gammaA, gammaS, gammaZ), postOffenders
   * Writes: tau, eta, validators (kappa, lambda, iota, gammaK), gamma (gammaA, gammaS, gammaZ)
   *
   * @param input The safrole input containing slot, entropy, and tickets extrinsic.
   * @param state The unified JamState.
   * @param deferProofs If given, ticket proofs are verified concurrently with the rest of the block and handed to it.
   * @return Tuple of (updated JamState, output).
   */
  def stfView(
    input: SafroleInput,
    view: TrieBackedJamState,
    deferProofs: Option[DeferredProofs] = None
  ): SafroleOutput =
    val preState = SafroleBridge.extract(view)
    val (postState, output) = stfInternal(input, preState, view.config, deferProofs)
    SafroleBridge.apply(view, postState)
    output

  def epochStfView(input: SafroleInput, view: TrieBackedJamState): SafroleOutput =
    val preState = SafroleBridge.extract(view)
    guarded(advance(input, preState, view.config)) match
      case Right((postState, output)) =>
        SafroleBridge.apply(view, postState)
        StfResult.success(output)
      case Left(error) => StfResult.error(error)

  def ticketsStfView(
    input: SafroleInput,
    view: TrieBackedJamState,
    deferProofs: Option[DeferredProofs] = None
  ): StfResult[Unit, SafroleErrorCode] =
    val state = SafroleBridge.extract(view)
    guarded(applyTickets(input, state, view.config, deferProofs)) match
      case Right(postState) =>
        SafroleBridge.apply(view, postState)
        StfResult.success(())
      case Left(error) => StfResult.error(error)

  /**
   * Internal Safrole STF implementation using SafroleState.
   *
   * @param input The safrole input containing slot, entropy, and tickets extrinsic.
   * @param preState The pre-transition state.
   * @param config The chain configuration.
   * @param deferProofs If given, ticket proofs are verified concurrently with the rest of the block and handed to it.
   * @return Tuple of (post-transition state, output).
   */
  def stfInternal(
    input: SafroleInput,
    preState: SafroleState,
    config: ChainConfig,
    deferProofs: Option[DeferredProofs] = None
  ): (SafroleState, SafroleOutput) =
    val result = guarded(
      advance(input, preState, config).flatMap((advanced, output) =>
        applyTickets(input, advanced, config, deferProofs).map(_ -> output)
      )
    )
    result match
      case Right((postState, output)) => (postState, StfResult.success(output))
      case Left(error)                => (preState, StfResult.error(error))

  private def guarded[A](run: => Either[SafroleErrorCode, A]): Either[SafroleErrorCode, A] =
    try run
    catch
      case e: SafroleCryptoFailure => throw e
      case e @ (_: io.forge.jam.core.codec.CodecDecodingException | _: IllegalArgumentException) =>
        logger.error("safrole input/state rejected, reporting Reserved", e)
        Left(SafroleErrorCode.Reserved)

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

  private def advance(
    input: SafroleInput,
    preState: SafroleState,
    config: ChainConfig
  ): Either[SafroleErrorCode, (SafroleState, SafroleOutputData)] =
    if input.slot <= preState.tau then Left(SafroleErrorCode.BadSlot)
    else
      val ctx = epochContext(preState, input.slot, config.epochLength)

      // Check for tickets_mark condition: same epoch, crossing cutoff, accumulator full
      val ticketsMark = Option.when(
        ctx.crossingTicketCutoff(config.ticketCutoff) &&
          preState.gammaA.size == config.epochLength
      )(transformTicketsSequence(preState.gammaA))

      // Handle epoch transition if needed
      val (stateAfterEpoch, epochMark) =
        if ctx.crossingEpochBoundary then
          val (newState, mark) = handleEpochTransition(preState, preState.postOffenders, ctx, config)
          (newState, Some(mark))
        else
          (preState, None)

      // Process entropy accumulation and update timeslot
      val newEta0 = Hashing.blake2b256(preState.eta.head.bytes ++ input.entropy.bytes)
      val advanced = stateAfterEpoch.copy(eta = newEta0 :: stateAfterEpoch.eta.tail, tau = input.slot)
      Right((advanced, SafroleOutputData(epochMark, ticketsMark)))

  private def applyTickets(
    input: SafroleInput,
    advanced: SafroleState,
    config: ChainConfig,
    deferProofs: Option[DeferredProofs]
  ): Either[SafroleErrorCode, SafroleState] =
    if input.extrinsic.isEmpty then Right(advanced)
    else
      val phase = input.slot % config.epochLength
      processExtrinsics(advanced, input.extrinsic, phase, advanced.gammaZ, config, deferProofs)

  /**
   * Handle epoch transition.
   *
   * @param preState The pre-transition state
   * @param posteriorOffenders The list of posterior offenders for the epoch transition
   * @param ctx The epoch timing context
   * @param config The chain configuration
   * @return Tuple of (updated state, epoch mark)
   */
  private def handleEpochTransition(
    preState: SafroleState,
    posteriorOffenders: List[Ed25519PublicKey],
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
    val offenderSet = posteriorOffenders.map(o => JamBytes(o.bytes)).toSet
    val newGammaK = preState.iota.map { validator =>
      if offenderSet.contains(JamBytes(validator.ed25519.bytes)) then
        validator.copy(
          bandersnatch = BandersnatchPublicKey(new Array[Byte](32)),
          ed25519 = Ed25519PublicKey(new Array[Byte](32)),
          bls = BlsPublicKey(new Array[Byte](BlsPublicKey.Size)),
          metadata = JamBytes.zeros(ValidatorKey.MetadataSize)
        )
      else
        validator
    }

    // Generate new ring root.
    val ringKeys = newGammaK
      .map(_.bandersnatch)
      .padTo(config.validatorCount, BandersnatchPublicKey(new Array[Byte](32)))
      .toList
    val newGammaZ = BandersnatchWrapper
      .generateRingRoot(ringKeys, config.validatorCount)
      .getOrElse(
        throw new SafroleCryptoFailure("Bandersnatch ring-root generation failed during epoch transition")
      )

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
      validators = newGammaK.map(v => EpochValidatorKey(v.bandersnatch, v.ed25519)).toList
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
   * @param deferProofs If given, receives the still-running proof checks when every other check passes
   * @return Either an error or the updated state
   */
  private def processExtrinsics(
    postState: SafroleState,
    tickets: List[TicketEnvelope],
    phase: Long,
    verificationGammaZ: JamBytes,
    config: ChainConfig,
    deferProofs: Option[DeferredProofs]
  ): Either[SafroleErrorCode, SafroleState] =
    // Skip if in epoch tail
    if phase >= config.ticketCutoff then
      Left(SafroleErrorCode.UnexpectedTicket)
    else if tickets.length > config.maxTicketsPerExtrinsic then
      Left(SafroleErrorCode.Reserved)
    else
      val ids = tickets.map(t =>
        JamBytes(BandersnatchVrf.ringVrfOutput(t.signature.toArray).getOrElse(t.signature.toArray.take(32)))
      )
      checkTickets(postState, tickets, ids, config).flatMap { marks =>
        val eta2 = postState.eta(2)
        def proofHolds(ticket: TicketEnvelope, mark: TicketMark): Boolean =
          BandersnatchWrapper
            .verifyRingProof(ticket.signature, verificationGammaZ, eta2, ticket.attempt, config.validatorCount)
            .exists(_.id == mark.id)
        val accumulated = accumulate(postState, marks, config)
        deferProofs match
          case None =>
            if !Parallel.map(tickets.zip(marks))(proofHolds).forall(identity) then Left(SafroleErrorCode.BadTicketProof)
            else accumulated
          case Some(handOver) =>
            val checks = tickets.zip(marks).map((ticket, mark) => Parallel.async(proofHolds(ticket, mark)))
            accumulated match
              case Right(state) =>
                handOver(checks)
                Right(state)
              case Left(error) =>
                if checks.exists(c => !Parallel.await(c)) then Left(SafroleErrorCode.BadTicketProof) else Left(error)
      }

  private def checkTickets(
    postState: SafroleState,
    tickets: List[TicketEnvelope],
    ids: List[JamBytes],
    config: ChainConfig
  ): Either[SafroleErrorCode, List[TicketMark]] =
    val accumulatorIds = postState.gammaA.iterator.map(_.id).toSet
    val maxAttempts = maxTicketEntries(postState.gammaK.size, config)
    if ids.exists(accumulatorIds.contains) || ids.distinct.size != ids.size then
      Left(SafroleErrorCode.DuplicateTicket)
    else if ids.zip(ids.drop(1)).exists((previous, current) => previous >= current) then
      Left(SafroleErrorCode.BadTicketOrder)
    else if tickets.exists(_.attempt.toInt >= maxAttempts) then
      Left(SafroleErrorCode.BadTicketAttempt)
    else
      Right(tickets.zip(ids).map((ticket, id) => TicketMark(id, ticket.attempt)))

  private def accumulate(
    postState: SafroleState,
    marks: List[TicketMark],
    config: ChainConfig
  ): Either[SafroleErrorCode, SafroleState] =
    val combined = (postState.gammaA ++ marks).sortBy(_.id).take(config.epochLength)
    val survivingIds = combined.map(_.id).toSet
    if marks.exists(t => !survivingIds.contains(t.id)) then Left(SafroleErrorCode.Reserved)
    else Right(postState.copy(gammaA = combined))

  private def maxTicketEntries(pendingSetSize: Int, config: ChainConfig): Int =
    if pendingSetSize <= 0 then config.ticketsPerValidator
    else (2 * config.epochLength + pendingSetSize - 1) / pendingSetSize

  /**
   * Generate fallback sealing sequence when epoch changes without sufficient tickets.
   * Uses deterministic validator selection based on entropy.
   */
  private def generateFallbackSequence(
    entropy: Hash,
    validators: IndexedSeq[ValidatorKey],
    epochLength: Int
  ): List[BandersnatchPublicKey] =
    val keys: Array[BandersnatchPublicKey] = validators.iterator.map(_.bandersnatch).toArray
    val validatorCount = validators.size
    (0 until epochLength).map { i =>
      val iEncoded = uint32L.encode(i.toLong & 0xffffffffL)
      val slotEntropy = Hashing.blake2b256(entropy.bytes ++ iEncoded)
      val index = (uint32L.decodeUnsafe(slotEntropy.bytes) % validatorCount).toInt
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
