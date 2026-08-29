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
  * slot.
  *
  * Current scope: fallback-key sealing and empty extrinsics — with no ticket
  * extrinsics submitted, every epoch's slot-sealer sequence remains the
  * fallback key sequence, so a devnet chains indefinitely. Ticketed sealing
  * arrives with the ring-VRF prover work.
  */
final class BlockAuthor(
    chain: ChainManager,
    validators: Seq[ValidatorKeySet],
    pools: ExtrinsicPools = new ExtrinsicPools
) extends LazyLogging:

  private val ourBandersnatchKeys: Map[Seq[Byte], ValidatorKeySet] =
    validators.map(v => v.bandersnatchPublic.toSeq -> v).toMap

  /** Try to author a block for `slot` on top of the current best head.
    * Returns the block when one of our keys owns the slot.
    */
  def tryAuthor(slot: Long): Option[Block] =
    val best = chain.best
    if slot <= best.slot then return None

    val view = chain.stateView()
    if slot <= view.timeslot then return None

    // Run Safrole for the target slot to obtain the posterior sealer sequence
    // and the header markers. The view is a throwaway (never committed).
    val safroleInput =
      SafroleInput(slot = slot, entropy = Hash(new Array[Byte](32)), extrinsic = List.empty)
    val markers = SafroleTransition.stfView(safroleInput, view) match
      case Right(output) => output
      case Left(err) =>
        logger.debug(s"safrole rejected slot $slot: $err")
        return None

    val slotIndex = (slot % chain.config.epochLength).toInt

    // Posterior sealer entry for the slot. Ticketed epochs need the ring
    // prover (we cannot know which ticket is ours without having generated
    // tickets), so only fallback-key sealing authors for now.
    val (keys, sealVrfInput) = view.gamma.st match
      case TicketsOrKeys.Keys(sealerKeys) =>
        val sealerKey = sealerKeys(slotIndex).bytes
        ourBandersnatchKeys.get(sealerKey.toSeq) match
          case Some(ks) =>
            val sealEntropy =
              if view.entropy.pool.length > 3 then view.entropy.pool(3).bytes
              else new Array[Byte](32)
            (ks, SigningContext.fallbackSealInputData(sealEntropy))
          case None => return None
      case TicketsOrKeys.Tickets(_) =>
        logger.debug("ticketed epoch: sealing requires the ring prover (not yet available)")
        return None

    // authorIndex is the position of the sealing key in the active set.
    val authorIndex = view.validators.current.indexWhere(vk =>
      java.util.Arrays.equals(vk.bandersnatch.bytes.toArray, keys.bandersnatchPublic)
    )
    if authorIndex < 0 then
      logger.warn("sealing key not in the active validator set")
      return None

    // Fill from the pools (tickets await the ring prover).
    val extrinsic = Extrinsic(
      tickets = List.empty,
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

    logger.info(
      s"authored block for slot $slot (author index $authorIndex, " +
        s"${extrinsic.guarantees.size} guarantees, ${extrinsic.assurances.size} assurances)"
    )
    Some(block)
