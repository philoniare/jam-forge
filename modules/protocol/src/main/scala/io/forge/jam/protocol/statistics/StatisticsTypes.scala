package io.forge.jam.protocol.statistics

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.extrinsic.{Preimage, AssuranceExtrinsic, Dispute, GuaranteeExtrinsic}
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.circe.Decoder
import spire.math.{UShort, UInt}
import _root_.scodec.{Codec, Attempt, DecodeResult}
import _root_.scodec.bits.{BitVector, ByteVector}
import _root_.scodec.codecs.*
import io.forge.jam.core.scodec.JamCodecs

/**
 * Types for the Statistics State Transition Function.
 *
 * The Statistics STF tracks validator performance statistics including:
 * blocks authored, tickets submitted, preimages, guarantees, and assurances.
 *
 * v0.7.0+ format includes:
 * - accumulator: current epoch validator stats (fixed-size array)
 * - previous: previous epoch validator stats (fixed-size array)
 * - core: per-core statistics (fixed-size array)
 * - service: per-service statistics (sorted map)
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

    given Codec[StatCount] =
      (uint32L :: uint32L :: uint32L :: uint32L :: uint32L :: uint32L).xmap(
        { case (blocks, tickets, preImages, preImagesSize, guarantees, assurances) =>
          StatCount(blocks & 0xFFFFFFFFL, tickets & 0xFFFFFFFFL, preImages & 0xFFFFFFFFL,
                   preImagesSize & 0xFFFFFFFFL, guarantees & 0xFFFFFFFFL, assurances & 0xFFFFFFFFL)
        },
        sc => (sc.blocks & 0xFFFFFFFFL, sc.tickets & 0xFFFFFFFFL, sc.preImages & 0xFFFFFFFFL,
               sc.preImagesSize & 0xFFFFFFFFL, sc.guarantees & 0xFFFFFFFFL, sc.assurances & 0xFFFFFFFFL)
      )

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
    def codec(config: ChainConfig): Codec[StatExtrinsic] =
      (JamCodecs.compactPrefixedList(summon[Codec[TicketEnvelope]]) ::
       JamCodecs.compactPrefixedList(summon[Codec[Preimage]]) ::
       JamCodecs.compactPrefixedList(summon[Codec[GuaranteeExtrinsic]]) ::
       JamCodecs.compactPrefixedList(AssuranceExtrinsic.codec(config.coresCount)) ::
       Dispute.codec(config.votesPerVerdict)).xmap(
        { case (tickets, preimages, guarantees, assurances, disputes) =>
          StatExtrinsic(tickets, preimages, guarantees, assurances, disputes)
        },
        se => (se.tickets, se.preimages, se.guarantees, se.assurances, se.disputes)
      )

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
    def codec(config: ChainConfig): Codec[StatInput] =
      (uint32L :: uint16L :: StatExtrinsic.codec(config)).xmap(
        { case (slot, authorIndex, extrinsic) =>
          StatInput(slot & 0xFFFFFFFFL, authorIndex.toLong, extrinsic)
        },
        si => (si.slot & 0xFFFFFFFFL, si.authorIndex.toInt, si.extrinsic)
      )


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
    currValidators: List[ValidatorKey],
    prevValidators: List[ValidatorKey] = List.empty
  )

  object StatState:
    def codec(validatorCount: Int): Codec[StatState] =
      (JamCodecs.fixedSizeList(summon[Codec[StatCount]], validatorCount) ::
       JamCodecs.fixedSizeList(summon[Codec[StatCount]], validatorCount) ::
       uint32L ::
       JamCodecs.fixedSizeList(summon[Codec[ValidatorKey]], validatorCount)).xmap(
        { case (valsCurrStats, valsLastStats, slot, currValidators) =>
          StatState(valsCurrStats, valsLastStats, slot & 0xFFFFFFFFL, currValidators)
        },
        ss => (ss.valsCurrStats, ss.valsLastStats, ss.slot & 0xFFFFFFFFL, ss.currValidators)
      )

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
    def codec(config: ChainConfig): Codec[StatCase] =
      (StatInput.codec(config) :: StatState.codec(config.validatorCount) :: StatState.codec(config.validatorCount)).xmap(
        { case (input, preState, postState) =>
          StatCase(input, preState, None, postState)
        },
        sc => (sc.input, sc.preState, sc.postState)
      )

    given Decoder[StatCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[StatInput]("input")
          preState <- cursor.get[StatState]("pre_state")
          output <- cursor.get[Option[StatOutput]]("output")
          postState <- cursor.get[StatState]("post_state")
        yield StatCase(input, preState, output, postState)
      }
