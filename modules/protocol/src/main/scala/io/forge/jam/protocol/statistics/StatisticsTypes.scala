package io.forge.jam.protocol.statistics

import io.forge.jam.core.{ChainConfig, JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encodeFixedList, decodeFixedList, listDecoder, encode}
import io.forge.jam.core.primitives.{BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.extrinsic.{Preimage, AssuranceExtrinsic, Dispute, GuaranteeExtrinsic}
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.forge.jam.core.json.JsonHelpers.{parseHex, parseHexBytesFixed}
import io.circe.Decoder
import spire.math.{UShort, UInt}

/**
 * Types for the Statistics State Transition Function.
 *
 * The Statistics STF tracks validator performance statistics including:
 * blocks authored, tickets submitted, preimages, guarantees, and assurances.
 */
object StatisticsTypes:

  /**
   * Statistics counter for a single validator.
   * Each field is a 4-byte unsigned integer (u32).
   * Fixed size: 24 bytes (6 * 4 bytes)
   */
  final case class StatCount(
    blocks: Long,
    tickets: Long,
    preImages: Long,
    preImagesSize: Long,
    guarantees: Long,
    assurances: Long
  )

  object StatCount:
    val Size: Int = 24 // 6 * 4 bytes

    def zero: StatCount = StatCount(0, 0, 0, 0, 0, 0)

    given JamEncoder[StatCount] with
      def encode(a: StatCount): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeU32LE(UInt(a.blocks.toInt))
        builder ++= codec.encodeU32LE(UInt(a.tickets.toInt))
        builder ++= codec.encodeU32LE(UInt(a.preImages.toInt))
        builder ++= codec.encodeU32LE(UInt(a.preImagesSize.toInt))
        builder ++= codec.encodeU32LE(UInt(a.guarantees.toInt))
        builder ++= codec.encodeU32LE(UInt(a.assurances.toInt))
        builder.result()

    given JamDecoder[StatCount] with
      def decode(bytes: JamBytes, offset: Int): (StatCount, Int) =
        val arr = bytes.toArray
        var pos = offset
        val blocks = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        val tickets = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        val preImages = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        val preImagesSize = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        val guarantees = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        val assurances = codec.decodeU32LE(arr, pos).toLong
        (StatCount(blocks, tickets, preImages, preImagesSize, guarantees, assurances), Size)

    given Decoder[StatCount] =
      Decoder.instance { cursor =>
        for
          blocks <- cursor.get[Long]("blocks")
          tickets <- cursor.get[Long]("tickets")
          preImages <- cursor.get[Long]("pre_images")
          preImagesSize <- cursor.get[Long]("pre_images_size")
          guarantees <- cursor.get[Long]("guarantees")
          assurances <- cursor.get[Long]("assurances")
        yield StatCount(blocks, tickets, preImages, preImagesSize, guarantees, assurances)
      }

  // JSON decoder for ValidatorKey (the type is imported from core)
  given Decoder[ValidatorKey] =
    Decoder.instance { cursor =>
      for
        bandersnatchHex <- cursor.get[String]("bandersnatch")
        ed25519Hex <- cursor.get[String]("ed25519")
        blsHex <- cursor.get[String]("bls")
        metadataHex <- cursor.get[String]("metadata")
        bandersnatch <- parseHexBytesFixed(bandersnatchHex, BandersnatchPublicKey.Size).map(BandersnatchPublicKey(_))
        ed25519 <- parseHexBytesFixed(ed25519Hex, Ed25519PublicKey.Size).map(Ed25519PublicKey(_))
        bls <- parseHexBytesFixed(blsHex, BlsPublicKey.Size).map(BlsPublicKey(_))
        metadata <- parseHexBytesFixed(metadataHex, ValidatorKey.MetadataSize).map(JamBytes(_))
      yield ValidatorKey(bandersnatch, ed25519, bls, metadata)
    }

  /**
   * Extrinsic data relevant to statistics.
   */
  final case class StatExtrinsic(
    tickets: List[TicketEnvelope],
    preimages: List[Preimage],
    guarantees: List[GuaranteeExtrinsic],
    assurances: List[AssuranceExtrinsic],
    disputes: Dispute
  )

  object StatExtrinsic:
    given JamEncoder[StatExtrinsic] with
      def encode(a: StatExtrinsic): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.tickets.encode
        builder ++= a.preimages.encode
        builder ++= a.guarantees.encode
        builder ++= a.assurances.encode
        builder ++= a.disputes.encode
        builder.result()

    /** Create a decoder that knows the chain config */
    def decoder(config: ChainConfig): JamDecoder[StatExtrinsic] = new JamDecoder[StatExtrinsic]:
      private val coreCount = config.coresCount
      private val validatorCount = config.validatorCount
      def decode(bytes: JamBytes, offset: Int): (StatExtrinsic, Int) =
        val arr = bytes.toArray
        var pos = offset

        // tickets - use listDecoder
        val (tickets, ticketsBytes) = listDecoder[TicketEnvelope].decode(bytes, pos)
        pos += ticketsBytes

        // preimages - use listDecoder
        val (preimages, preimagesBytes) = listDecoder[Preimage].decode(bytes, pos)
        pos += preimagesBytes

        // guarantees - use listDecoder
        val (guarantees, guaranteesBytes) = listDecoder[GuaranteeExtrinsic].decode(bytes, pos)
        pos += guaranteesBytes

        // assurances - need custom decoder due to config-dependent size
        val (assurancesLength, assurancesLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += assurancesLengthBytes
        val assuranceDecoder = AssuranceExtrinsic.decoder(coreCount)
        val assurances = (0 until assurancesLength.toInt).map { _ =>
          val (assurance, consumed) = assuranceDecoder.decode(bytes, pos)
          pos += consumed
          assurance
        }.toList

        // disputes - variable size, votesPerVerdict is 2/3 + 1 of validators
        val votesPerVerdict = (validatorCount * 2 / 3) + 1
        val disputeDecoder = Dispute.decoder(votesPerVerdict)
        val (disputes, disputesBytes) = disputeDecoder.decode(bytes, pos)
        pos += disputesBytes

        (StatExtrinsic(tickets, preimages, guarantees, assurances, disputes), pos - offset)

    given Decoder[StatExtrinsic] =
      Decoder.instance { cursor =>
        for
          tickets <- cursor.get[List[TicketEnvelope]]("tickets")
          preimages <- cursor.get[List[Preimage]]("preimages")
          guarantees <- cursor.get[List[GuaranteeExtrinsic]]("guarantees")
          assurances <- cursor.get[List[AssuranceExtrinsic]]("assurances")
          disputes <- cursor.get[Dispute]("disputes")
        yield StatExtrinsic(tickets, preimages, guarantees, assurances, disputes)
      }

  /**
   * Input to the Statistics STF.
   */
  final case class StatInput(
    slot: Long,
    authorIndex: Long,
    extrinsic: StatExtrinsic
  )

  object StatInput:
    given JamEncoder[StatInput] with
      def encode(a: StatInput): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeU32LE(UInt(a.slot.toInt))
        builder ++= codec.encodeU16LE(UShort(a.authorIndex.toInt))
        builder ++= a.extrinsic.encode
        builder.result()

    /** Create a decoder that knows the chain config */
    def decoder(config: ChainConfig): JamDecoder[StatInput] = new JamDecoder[StatInput]:
      def decode(bytes: JamBytes, offset: Int): (StatInput, Int) =
        val arr = bytes.toArray
        var pos = offset
        // slot - 4 bytes (u32)
        val slot = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        // authorIndex - 2 bytes (u16)
        val authorIndex = codec.decodeU16LE(arr, pos).toLong
        pos += 2
        // extrinsic
        val extrinsicDecoder = StatExtrinsic.decoder(config)
        val (extrinsic, extrinsicBytes) = extrinsicDecoder.decode(bytes, pos)
        pos += extrinsicBytes
        (StatInput(slot, authorIndex, extrinsic), pos - offset)

    given Decoder[StatInput] =
      Decoder.instance { cursor =>
        for
          slot <- cursor.get[Long]("slot")
          authorIndex <- cursor.get[Long]("author_index")
          extrinsic <- cursor.get[StatExtrinsic]("extrinsic")
        yield StatInput(slot, authorIndex, extrinsic)
      }

  /**
   * Statistics state containing validator stats and metadata.
   */
  final case class StatState(
    valsCurrStats: List[StatCount],
    valsLastStats: List[StatCount],
    slot: Long,
    currValidators: List[ValidatorKey]
  )

  object StatState:
    given JamEncoder[StatState] with
      def encode(a: StatState): JamBytes =
        val builder = JamBytes.newBuilder
        // valsCurrStats - fixed size list (no length prefix)
        builder ++= encodeFixedList(a.valsCurrStats)
        // valsLastStats - fixed size list (no length prefix)
        builder ++= encodeFixedList(a.valsLastStats)
        // slot - 4 bytes (u32)
        builder ++= codec.encodeU32LE(UInt(a.slot.toInt))
        // currValidators - fixed size list (no length prefix)
        builder ++= encodeFixedList(a.currValidators)
        builder.result()

    /** Create a decoder that knows the chain config */
    def decoder(config: ChainConfig): JamDecoder[StatState] = new JamDecoder[StatState]:
      private val validatorCount = config.validatorCount
      def decode(bytes: JamBytes, offset: Int): (StatState, Int) =
        val arr = bytes.toArray
        var pos = offset
        // valsCurrStats - fixed size list
        val (valsCurrStats, currStatsBytes) = decodeFixedList[StatCount](bytes, pos, validatorCount)
        pos += currStatsBytes
        // valsLastStats - fixed size list
        val (valsLastStats, lastStatsBytes) = decodeFixedList[StatCount](bytes, pos, validatorCount)
        pos += lastStatsBytes
        // slot - 4 bytes (u32)
        val slot = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        // currValidators - fixed size list
        val (currValidators, validatorsBytes) = decodeFixedList[ValidatorKey](bytes, pos, validatorCount)
        pos += validatorsBytes
        (StatState(valsCurrStats, valsLastStats, slot, currValidators), pos - offset)

    given Decoder[StatState] =
      Decoder.instance { cursor =>
        for
          valsCurrStats <- cursor.get[List[StatCount]]("vals_curr_stats")
          valsLastStats <- cursor.get[List[StatCount]]("vals_last_stats")
          slot <- cursor.get[Long]("slot")
          currValidators <- cursor.get[List[ValidatorKey]]("curr_validators")
        yield StatState(valsCurrStats, valsLastStats, slot, currValidators)
      }

  /**
   * Output from the Statistics STF (always null in encoding).
   */
  final case class StatOutput(id: Long)

  object StatOutput:
    given Decoder[Option[StatOutput]] =
      Decoder.decodeOption(Decoder.instance { cursor =>
        cursor.get[Long]("id").map(StatOutput(_))
      })

  /**
   * Test case for Statistics STF containing input, pre-state, and post-state.
   */
  final case class StatCase(
    input: StatInput,
    preState: StatState,
    output: Option[StatOutput],
    postState: StatState
  )

  object StatCase:
    /** Create a config-aware decoder for StatCase */
    def decoder(config: ChainConfig): JamDecoder[StatCase] = new JamDecoder[StatCase]:
      def decode(bytes: JamBytes, offset: Int): (StatCase, Int) =
        var pos = offset
        val inputDecoder = StatInput.decoder(config)
        val (input, inputBytes) = inputDecoder.decode(bytes, pos)
        pos += inputBytes
        val stateDecoder = StatState.decoder(config)
        val (preState, preStateBytes) = stateDecoder.decode(bytes, pos)
        pos += preStateBytes
        // output is always null in encoding
        val (postState, postStateBytes) = stateDecoder.decode(bytes, pos)
        pos += postStateBytes
        (StatCase(input, preState, None, postState), pos - offset)

    given JamEncoder[StatCase] with
      def encode(a: StatCase): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.input.encode
        builder ++= a.preState.encode
        builder ++= a.postState.encode
        builder.result()

    given Decoder[StatCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[StatInput]("input")
          preState <- cursor.get[StatState]("pre_state")
          output <- cursor.get[Option[StatOutput]]("output")
          postState <- cursor.get[StatState]("post_state")
        yield StatCase(input, preState, output, postState)
      }
