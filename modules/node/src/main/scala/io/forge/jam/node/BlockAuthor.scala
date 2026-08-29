package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.{Hash, Timeslot, ValidatorIndex}
import io.forge.jam.core.types.block.{Block, Extrinsic}
import io.forge.jam.core.types.extrinsic.Dispute
import io.forge.jam.core.types.header.Header
import io.forge.jam.crypto.SigningContext
import io.forge.jam.protocol.pipeline.ExtrinsicHashing
import io.forge.jam.protocol.safrole.SafroleTransition
import io.forge.jam.protocol.safrole.SafroleTypes.{SafroleInput, TicketsOrKeys}
import io.forge.jam.vrfs.BandersnatchWrapper
import spire.math.UInt

/** Authors blocks when one of this node's validator keys owns the sealing
  * slot
  */
final class BlockAuthor(
    chain: ChainManager,
    validators: Seq[ValidatorKeySet],
    pools: ExtrinsicPools = new ExtrinsicPools,
    /** Own tickets (id bytes → keys, attempt), populated by TicketService. */
    ownTickets: java.util.concurrent.ConcurrentHashMap[Seq[Byte], (ValidatorKeySet, spire.math.UByte)] =
      new java.util.concurrent.ConcurrentHashMap()
) extends LazyLogging:

  private val ourBandersnatchKeys: Map[Seq[Byte], ValidatorKeySet] =
    validators.map(v => v.bandersnatchPublic.toSeq -> v).toMap

  /** Try to author a block for `slot` on top of the current best head.
    * Returns the block when one of our keys owns the slot.
    */
  def tryAuthor(slot: Long): Option[Block] =
    val best = chain.best
    if slot <= best.slot then return None

    val preView = chain.stateView()
    if slot <= preView.timeslot then return None

    // Candidate tickets while the submission window is open: ascending id,
    // not already accumulated, at most Cmaxblocktickets.
    val phase = (slot % chain.config.epochLength).toInt
    val accumulatedIds = preView.gamma.a.map(_.id).toSet
    val candidateTickets =
      if phase < chain.config.ticketCutoff then
        pools.takeTickets(accumulatedIds, chain.config.maxTicketsPerExtrinsic)
      else List.empty

    // Run Safrole for the target slot (with the real ticket extrinsic) to
    // obtain the posterior sealer sequence and header markers. The view is a
    // throwaway (never committed). A rejected ticket set falls back to an
    // empty extrinsic rather than stalling authoring.
    def runSafrole(tickets: List[(Hash, io.forge.jam.core.types.tickets.TicketEnvelope)]) =
      val v = chain.stateView()
      SafroleTransition
        .stfView(
          SafroleInput(
            slot = slot,
            entropy = Hash(new Array[Byte](32)),
            extrinsic = tickets.map(_._2)
          ),
          v
        )
        .map(out => (v, out, tickets))

    val (view, markers, includedTickets) =
      runSafrole(candidateTickets) match
        case Right(res) => res
        case Left(err1) if candidateTickets.nonEmpty =>
          logger.warn(s"ticket extrinsic rejected ($err1); authoring without tickets")
          pools.removeTickets(candidateTickets.map(_._1))
          runSafrole(List.empty) match
            case Right(res) => res
            case Left(err) =>
              logger.debug(s"safrole rejected slot $slot: $err")
              return None
        case Left(err) =>
          logger.debug(s"safrole rejected slot $slot: $err")
          return None

    val slotIndex = (slot % chain.config.epochLength).toInt

    // Posterior sealer entry for the slot.
    val sealEntropy =
      if view.entropy.pool.length > 3 then view.entropy.pool(3).bytes
      else new Array[Byte](32)
    val (keys, sealVrfInput) = view.gamma.st match
      case TicketsOrKeys.Keys(sealerKeys) =>
        val sealerKey = sealerKeys(slotIndex).bytes
        ourBandersnatchKeys.get(sealerKey.toSeq) match
          case Some(ks) => (ks, SigningContext.fallbackSealInputData(sealEntropy))
          case None     => return None
      case TicketsOrKeys.Tickets(marks) =>
        val mark = marks(slotIndex)
        ownTickets.get(mark.id.toArray.toSeq) match
          case null => return None
          case (ks, attempt) =>
            if attempt != mark.attempt then return None
            (ks, SigningContext.safroleTicketInputData(sealEntropy, attempt.toByte))

    // authorIndex is the position of the sealing key in the active set.
    val authorIndex = view.validators.current.indexWhere(vk =>
      java.util.Arrays.equals(vk.bandersnatch.bytes.toArray, keys.bandersnatchPublic)
    )
    if authorIndex < 0 then
      logger.warn("sealing key not in the active validator set")
      return None

    // Fill from the pools.
    val extrinsic = Extrinsic(
      tickets = includedTickets.map(_._2),
      preimages = pools.takePreimages(),
      guarantees = pools.takeGuarantees(),
      assurances = pools.takeAssurances(best.hash),
      disputes = Dispute(List.empty, List.empty, List.empty)
    )
    val extrinsicHash = ExtrinsicHashing.computeExtrinsicHash(extrinsic, chain.config)

    // H_vrfsig: VRF over Xentropy ++ Y(sealsig); the seal's VRF output is
    // fixed by (context, key) alone, so a message-less pre-signature yields it.
    val preSeal = BandersnatchWrapper.ietfVrfSign(keys.bandersnatchSecret, sealVrfInput, Array.empty)
    val sealVrfOutput = BandersnatchWrapper.getIetfVrfOutput(preSeal)
    val entropyVrf = BandersnatchWrapper.ietfVrfSign(
      keys.bandersnatchSecret,
      SigningContext.entropyInputData(sealVrfOutput),
      Array.empty
    )

    val headerCodec =
      Header.headerCodec(chain.config.validatorCount, chain.config.epochLength)
    val unsealed = Header(
      parent = best.hash,
      parentStateRoot = best.stateRoot,
      extrinsicHash = extrinsicHash,
      slot = Timeslot(UInt(slot.toInt)),
      epochMark = markers.epochMark,
      ticketsMark = markers.ticketsMark,
      offendersMark = List.empty,
      authorIndex = ValidatorIndex(authorIndex),
      entropySource = JamBytes(entropyVrf),
      seal = JamBytes.zeros(96)
    )

    // Seal over the serialized header sans seal.
    val unsignedHeaderBytes =
      headerCodec.encode(unsealed).require.bytes.toArray.dropRight(96)
    val seal =
      BandersnatchWrapper.ietfVrfSign(keys.bandersnatchSecret, sealVrfInput, unsignedHeaderBytes)
    val block = Block(unsealed.copy(seal = JamBytes(seal)), extrinsic)

    pools.removeTickets(includedTickets.map(_._1))
    logger.info(
      s"authored block for slot $slot (author index $authorIndex, " +
        s"${extrinsic.tickets.size} tickets, ${extrinsic.guarantees.size} guarantees, " +
        s"${extrinsic.assurances.size} assurances)"
    )
    Some(block)
