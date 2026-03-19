package io.forge.jam.protocol.benchmark

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Hash, ValidatorIndex, Timeslot}
import io.forge.jam.core.types.block.{Block, Extrinsic}
import io.forge.jam.core.types.header.Header
import io.forge.jam.core.types.extrinsic.{Dispute, AssuranceExtrinsic}
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.forge.jam.crypto.SigningContext
import io.forge.jam.protocol.safrole.SafroleTransition
import io.forge.jam.protocol.safrole.SafroleTypes.*
import io.forge.jam.protocol.state.JamState
import io.forge.jam.protocol.traces.{FullJamState, RawState}
import io.forge.jam.vrfs.BandersnatchWrapper
import spire.math.UInt
import _root_.scodec.Codec

/**
 * Generates valid blocks that chain indefinitely from any state.
 * Produces empty-extrinsic blocks exercising Safrole, epoch transitions,
 * entropy rotation, history growth, and the full encode/decode/merklize pipeline.
 */
class BlockGenerator(config: ChainConfig, validators: Array[DevKeyStore.ValidatorKeys]):

  private var lastHeaderHash: Option[Hash] = None

  private val bsKeyToIndex: Map[String, Int] =
    validators.zipWithIndex.map { case (v, i) => v.bandersnatchPublic.map("%02x".format(_)).mkString -> i }.toMap

  def generateBlock(preState: RawState, slot: Long): Block =
    val fullState = FullJamState.fromKeyvals(preState.keyvals, config)
    val jamState = JamState.fromFullJamState(fullState, config)

    val dummyEntropy = Hash(new Array[Byte](32))
    val safroleInput = SafroleInput(slot = slot, entropy = dummyEntropy, extrinsic = List.empty)
    val (postSafroleJamState, safroleOutput) = SafroleTransition.stf(safroleInput, jamState, config)

    val outputData = safroleOutput match
      case Right(data) => data
      case Left(_) => throw new RuntimeException(s"Safrole failed for slot $slot")

    val slotIndex = (slot % config.epochLength).toInt
    val (authorIndex, authorBsSecret) = findSealingValidator(postSafroleJamState, slotIndex)

    val emptyExtrinsic = Extrinsic(
      tickets = List.empty,
      preimages = List.empty,
      guarantees = List.empty,
      assurances = List.empty,
      disputes = Dispute(List.empty, List.empty, List.empty)
    )
    val extrinsicHash = computeExtrinsicHash(emptyExtrinsic)

    val parentHash = lastHeaderHash.getOrElse {
      jamState.beta.history.lastOption.map(_.headerHash).getOrElse(Hash.zero)
    }

    // Determine seal VRF input based on sealing mode
    val sealEntropy = if postSafroleJamState.entropy.pool.length > 3
      then postSafroleJamState.entropy.pool(3).bytes
      else new Array[Byte](32)

    val sealVrfInput = postSafroleJamState.gamma.s match
      case TicketsOrKeys.Keys(_) =>
        SigningContext.fallbackSealInputData(sealEntropy)
      case TicketsOrKeys.Tickets(tickets) =>
        SigningContext.safroleTicketInputData(sealEntropy, tickets(slotIndex).attempt.toByte)

    // VRF output is deterministic on (secret, input) regardless of aux data,
    // so we can get it from a dummy sign and use it for the entropy VRF
    val dummySeal = BandersnatchWrapper.ietfVrfSign(authorBsSecret, sealVrfInput, Array.empty)
    val sealVrfOutput = BandersnatchWrapper.getIetfVrfOutput(dummySeal)

    // Sign entropy VRF (empty aux data)
    val entropyVrfInput = SigningContext.entropyInputData(sealVrfOutput)
    val entropyVrf = BandersnatchWrapper.ietfVrfSign(authorBsSecret, entropyVrfInput, Array.empty)

    // Build header with entropy source but placeholder seal
    val headerCodec = Header.headerCodec(config.validatorCount, config.epochLength)
    val headerWithEntropy = Header(
      parent = parentHash,
      parentStateRoot = preState.stateRoot,
      extrinsicHash = extrinsicHash,
      slot = Timeslot(UInt(slot.toInt)),
      epochMark = outputData.epochMark,
      ticketsMark = outputData.ticketsMark,
      offendersMark = List.empty,
      authorIndex = ValidatorIndex(authorIndex),
      entropySource = JamBytes(entropyVrf),
      seal = JamBytes.zeros(96)
    )

    // Encode unsigned header (everything except the 96-byte seal at the end)
    val fullHeaderBytes = headerCodec.encode(headerWithEntropy).require.bytes.toArray
    val encodedUnsignedHeader = fullHeaderBytes.dropRight(96)

    // Sign the real seal with the unsigned header as aux data
    val seal = BandersnatchWrapper.ietfVrfSign(authorBsSecret, sealVrfInput, encodedUnsignedHeader)
    val finalHeader = headerWithEntropy.copy(seal = JamBytes(seal))

    // Track header hash for next block's parent reference
    val finalHeaderBytes = headerCodec.encode(finalHeader).require.bytes.toArray
    lastHeaderHash = Some(Hashing.blake2b256(finalHeaderBytes))

    Block(finalHeader, emptyExtrinsic)

  private def findSealingValidator(state: JamState, slotIndex: Int): (Int, Array[Byte]) =
    state.gamma.s match
      case TicketsOrKeys.Keys(keys) =>
        val keyHex = keys(slotIndex).bytes.map("%02x".format(_)).mkString
        bsKeyToIndex.get(keyHex) match
          case Some(idx) => (idx, validators(idx).bandersnatchSecret)
          case None =>
            val fallbackIdx = validators.indices.find(i =>
              java.util.Arrays.equals(validators(i).bandersnatchPublic, keys(slotIndex).bytes)
            ).getOrElse(0)
            (fallbackIdx, validators(fallbackIdx).bandersnatchSecret)
      case TicketsOrKeys.Tickets(_) =>
        // Empty blocks never submit tickets, so we stay in fallback mode
        (0, validators(0).bandersnatchSecret)

  private def computeExtrinsicHash(ex: Extrinsic): Hash =
    import io.forge.jam.core.scodec.JamCodecs.{compactPrefixedList, compactInt}
    val ticketsEncoded = compactPrefixedList(summon[Codec[TicketEnvelope]]).encode(ex.tickets).require.toByteArray
    val preimagesEncoded = compactPrefixedList(summon[Codec[io.forge.jam.core.types.extrinsic.Preimage]]).encode(ex.preimages).require.toByteArray
    val assurancesEncoded = compactPrefixedList(AssuranceExtrinsic.codec(config.coresCount)).encode(ex.assurances).require.toByteArray
    val disputesEncoded = Dispute.codec(config.votesPerVerdict).encode(ex.disputes).require.toByteArray
    val gEncoded = compactInt.encode(0).require.toByteArray

    val hashes = List(ticketsEncoded, preimagesEncoded, gEncoded, assurancesEncoded, disputesEncoded).map(Hashing.blake2b256)
    Hashing.blake2b256(hashes.flatMap(_.bytes).toArray)

  def reset(): Unit =
    lastHeaderHash = None
