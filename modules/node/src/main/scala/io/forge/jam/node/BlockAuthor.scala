package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.{BandersnatchPublicKey, Hash, Timeslot, ValidatorIndex}
import io.forge.jam.core.types.block.{Block, Extrinsic}
import io.forge.jam.core.types.extrinsic.Dispute
import io.forge.jam.core.types.header.Header
import io.forge.jam.core.types.tickets.TicketEnvelope
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
    ownTickets: java.util.concurrent.ConcurrentHashMap[Hash, (ValidatorKeySet, spire.math.UByte)] =
      new java.util.concurrent.ConcurrentHashMap()
) extends LazyLogging:

  private val ourBandersnatchKeys: Map[BandersnatchPublicKey, ValidatorKeySet] =
    validators.map(v => v.bandersnatchKey -> v).toMap

  /** Try to author a block for `slot` on top of the current best head.
    * Returns the block when one of our keys owns the slot.
    */
  def tryAuthor(slot: Long): Option[Block] =
    val best = chain.best
    if slot <= best.slot then return None

    val preView = chain.stateView()
    if slot <= preView.timeslot then return None

    // Candidate tickets while the submission window is open: entered for
    // this slot's epoch, ascending id, not already accumulated, only those
    // the accumulator will retain, at most Cmaxblocktickets. A new epoch
    // starts with an empty accumulator and rotates η_1 into η_2
    val phase = (slot % chain.config.epochLength).toInt
    val epoch = slot / chain.config.epochLength
    val sameEpoch = epoch == preView.timeslot / chain.config.epochLength
    val accumulatedIds = if sameEpoch then preView.gamma.a.map(_.id).toSet else Set.empty
    val ticketEntropy = preView.entropy.pool.lift(if sameEpoch then 2 else 1).getOrElse(Hash.zero)
    val candidateTickets =
      if phase < chain.config.ticketCutoff then
        pools.takeTickets(
          epoch,
          ticketEntropy,
          accumulatedIds,
          chain.config.maxTicketsPerExtrinsic,
          chain.config.epochLength
        )
      else List.empty

    // Run Safrole for the target slot (with the real ticket extrinsic) to
    // obtain the posterior sealer sequence and header markers. The view is a
    // throwaway (never committed). A rejected ticket set falls back to an
    // empty extrinsic rather than stalling authoring.
    def runSafrole(tickets: List[(Hash, TicketEnvelope)]) =
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
          // Keep the tickets that pass on their own (ascending, so each kept
          // set stays ordered) and drop only the failures from the pool.
          val kept = candidateTickets.foldLeft(List.empty[(Hash, TicketEnvelope)]) {
            (acc, t) => if runSafrole(acc :+ t).isRight then acc :+ t else acc
          }
          val dropped = candidateTickets.filterNot(kept.contains)
          logger.warn(
            s"ticket extrinsic rejected ($err1); dropped ${dropped.size} of ${candidateTickets.size} ticket(s)"
          )
          pools.removeTickets(dropped.map(_._1))
          runSafrole(kept) match
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
        ourBandersnatchKeys.get(sealerKeys(slotIndex)) match
          case Some(ks) => (ks, SigningContext.fallbackSealInputData(sealEntropy))
          case None     => return None
      case TicketsOrKeys.Tickets(marks) =>
        val mark = marks(slotIndex)
        ownTickets.get(Hash(mark.id.toArray)) match
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

    // Guarantees that can no longer go in a block on `best` leave the pool, so they don't fail the import.
    val ancestors = chain.ancestorsOf(best.hash, best.stateRoot, slot)
    val recent = view.beta.history.map(_.headerHash).toSet + best.hash
    val stale = pools.pendingGuarantees.filterNot(BlockAuthor.guaranteeUsable(_, slot, recent, ancestors, chain.config))
    if stale.nonEmpty then
      logger.info(s"dropping ${stale.size} guarantee(s) no longer valid at slot $slot")
      pools.removeGuarantees(stale)

    // A preimage another block already provided would fail the import (PreimageUnneeded); one not requested yet waits.
    val preimageState = pools.pendingPreimages.map(p => p -> BlockAuthor.preimageRequest(p, preView)).toMap
    val provided = preimageState.collect { case (p, Some(false)) => p }
    if provided.nonEmpty then
      logger.info(s"dropping ${provided.size} preimage(s) already provided")
      pools.removePreimages(provided)

    // Fill from the pools.
    val extrinsic = Extrinsic(
      tickets = includedTickets.map(_._2),
      preimages = pools.takePreimages(p => preimageState.get(p).forall(_.contains(true))),
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
      Header.headerCodec(chain.config.epochLength)
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
      headerCodec.encode(unsealed).dropRight(96)
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

object BlockAuthor:

  /** The reporting checks that a guarantee fails just by ageing: its rotation is the current or the
    * previous one, its anchor is in recent history, and its lookup anchor is an ancestor at most L slots old.
    */
  def guaranteeUsable(
      g: io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic,
      slot: Long,
      recent: Set[Hash],
      ancestors: List[io.forge.jam.protocol.report.ReportTypes.AncestorHeader],
      config: io.forge.jam.core.ChainConfig
  ): Boolean =
    val ctx = g.report.context
    val rotation = g.slot.toLong / config.rotationPeriod
    val current = slot / config.rotationPeriod
    val lookupSlot = ctx.lookupAnchorSlot.toLong
    rotation >= current - 1 && g.slot.toLong <= slot &&
    recent.contains(ctx.anchor) &&
    slot - lookupSlot <= config.maxLookupAnchorAge &&
    ancestors.exists(a => a.headerHash == ctx.lookupAnchor && a.slot == lookupSlot)

  /** Some(true) when `p` is requested and not provided yet, Some(false) when already provided, None when not
    * requested (Y(a_l)).
    */
  def preimageRequest(
      p: io.forge.jam.core.types.extrinsic.Preimage,
      state: io.forge.jam.protocol.state.TrieBackedJamState
  ): Option[Boolean] =
    import io.forge.jam.protocol.accumulation.StateKey
    val blob = p.blob.toArray
    val hash = JamBytes(Hashing.blake2b256(blob).bytes)
    val key = StateKey.computePreimageInfoStateKey(p.requester.toLong, blob.length, hash)
    state.storage.readTrie(key).map(StateKey.isUnprovidedRequest)
