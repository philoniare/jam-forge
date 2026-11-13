package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.{BandersnatchPublicKey, Hash, Timeslot, ValidatorIndex}
import io.forge.jam.core.types.block.{Block, Extrinsic}
import io.forge.jam.core.primitives.Ed25519Signature
import io.forge.jam.protocol.dispute.DisputeBuilder
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.DisputeBridge
import io.forge.jam.core.types.header.Header
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.forge.jam.crypto.{BandersnatchVrf, SigningContext}
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

  /** Ticket ids our keys win with under one seal entropy, by id: (keys, attempt). A ticket's id is the VRF output
    * of its key over `jam_ticket_seal ++ entropy ++ attempt`, which is also what the seal for its slot outputs, so
    * the ids follow from the keys alone. Remembered for the last entropy (one epoch's sealing).
    */
  @volatile private var derivedTickets: (JamBytes, Map[Hash, (ValidatorKeySet, spire.math.UByte)]) = null

  private def derivedTicketIds(sealEntropy: Array[Byte]): Map[Hash, (ValidatorKeySet, spire.math.UByte)] =
    val key = JamBytes(sealEntropy)
    val cached = derivedTickets
    if cached != null && cached._1 == key then cached._2
    else
      val ids = (for
        ks <- validators
        attempt <- 0 until chain.config.ticketsPerValidator
        output <- BandersnatchVrf.getVrfOutput(
          BandersnatchWrapper.ietfVrfSign(
            ks.bandersnatchSecret,
            SigningContext.safroleTicketInputData(sealEntropy, attempt.toByte),
            Array.emptyByteArray
          )
        )
      yield Hash(output) -> (ks, spire.math.UByte(attempt))).toMap
      derivedTickets = (key, ids)
      ids

  /** Our keys and attempt for ticket `id`: from the tickets this process generated, or else derived from our keys,
    * so a ticket made before a restart (or by another process holding the same keys) still seals its slot.
    */
  private[node] def ownTicket(id: Hash, sealEntropy: Array[Byte]): Option[(ValidatorKeySet, spire.math.UByte)] =
    Option(ownTickets.get(id)).orElse(derivedTicketIds(sealEntropy).get(id))

  /** Try to author a block for `slot` on top of the current best head.
    * Returns the block when one of our keys owns the slot.
    */
  def tryAuthor(slot: Long, withDisputes: Boolean = true): Option[Block] =
    val best = chain.best
    if slot <= best.slot then return None

    val preView = chain.stateViewOf(best)
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
      val v = chain.stateViewOf(best)
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
        ownTicket(Hash(mark.id.toArray), sealEntropy) match
          case None => return None
          case Some((ks, attempt)) =>
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

    // Verdicts on reports someone judged invalid, with their culprits and faults; checked against the Disputes STF.
    val disputes = if withDisputes then BlockAuthor.disputes(pools, preView, chain) else DisputeBuilder.empty
    if !disputes.isEmpty then
      logger.warn(
        s"authoring ${disputes.dispute.verdicts.size} verdict(s), ${disputes.dispute.culprits.size} culprit(s), " +
          s"${disputes.dispute.faults.size} fault(s)"
      )

    // Assurances and guarantees that pass the pipeline's checks on a throwaway posterior-Safrole view; the rest are
    // left out, and those that cannot pass later leave the pool.
    val plan = AuthoringPlan.plan(
      runSafrole(includedTickets).fold(_ => chain.stateViewOf(best), _._1),
      priorTau = preView.timeslot,
      priorKappa = preView.validators.current,
      priorLambda = preView.validators.previous,
      disputes = disputes.dispute,
      slot = slot,
      parent = best.hash,
      assurances = pools.takeAssurances(best.hash),
      guarantees = pools.guaranteesByArrival,
      ancestry = ancestors,
      config = chain.config
    )
    if plan.droppedGuarantees.nonEmpty || plan.droppedAssurances.nonEmpty then
      logger.warn(
        s"leaving out ${plan.droppedGuarantees.size} guarantee(s) and ${plan.droppedAssurances.size} assurance(s) " +
          s"that no longer pass at slot $slot"
      )
      pools.removeGuarantees(plan.droppedGuarantees)
      pools.removeAssurances(plan.droppedAssurances)

    // Fill from the pools.
    val extrinsic = Extrinsic(
      tickets = includedTickets.map(_._2),
      preimages = pools.takePreimages(p => preimageState.get(p).forall(_.contains(true))),
      guarantees = plan.guarantees,
      assurances = plan.assurances,
      disputes = disputes.dispute
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
      offendersMark = disputes.offendersMark.map(k => Hash(k.bytes)),
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

  /** The disputes extrinsic for a block on the state `prior`: verdicts built from the pooled judgments on reports
    * someone judged invalid, the guarantors of each (from the credentials of the block that included it, resolved
    * against that block's validator sets) as culprits. Evidence too old for a verdict is pruned first.
    */
  def disputes(pools: ExtrinsicPools, prior: io.forge.jam.protocol.state.TrieBackedJamState, chain: ChainManager)
      : DisputeBuilder.Built =
    val state = DisputeBridge.extract(prior)
    val epochLength = chain.config.epochLength
    pools.pruneDisputes(state.tau / epochLength, epochLength, (state.psi.good ++ state.psi.bad ++ state.psi.wonky).toSet)
    val disputed = pools.disputed
    if disputed.isEmpty then DisputeBuilder.empty
    else
      val evidence = disputed.map { d =>
        val guarantors = d.credentials.toList.flatMap { (parent, _, sigs) =>
          chain.stateViewAt(parent).toList.flatMap { pv =>
            // An index is into the current set, or the previous one for a report of the last rotation before an
            // epoch change; the builder keeps the key the signature verifies under.
            val sets = List(pv.validators.current, pv.validators.previous)
            sigs.flatMap((i, sig) =>
              sets.flatMap(_.lift(i)).map(vk => DisputeBuilder.Credential(vk.ed25519, Ed25519Signature(sig)))
            )
          }
        }
        DisputeBuilder.Evidence(
          d.reportHash,
          d.epoch,
          d.judgments.map((v, valid, sig) => DisputeBuilder.Judgment(v, valid, Ed25519Signature(sig))),
          guarantors
        )
      }
      DisputeBuilder.build(evidence, state, chain.config)


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
