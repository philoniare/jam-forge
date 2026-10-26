package io.forge.jam.protocol.statistics

import io.circe.derivation.ConfiguredDecoder
import io.forge.jam.core.json.JsonConfig.given
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.extrinsic.{Preimage, AssuranceExtrinsic, Dispute, GuaranteeExtrinsic}
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.circe.Decoder
import io.forge.jam.core.codec.Codec
import io.forge.jam.core.codec.codecs.*
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.codec.FullJamStateCodecs

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

    /** Per-validator counters: 6 x u32 LE (GP 0.8.0 appendix D, pi_V). */
    given Codec[StatCount] = Codec.of(
      (w, sc) =>
        w.u32(sc.blocks)
        w.u32(sc.tickets)
        w.u32(sc.preImages)
        w.u32(sc.preImagesSize)
        w.u32(sc.guarantees)
        w.u32(sc.assurances),
      r => StatCount(r.u32(), r.u32(), r.u32(), r.u32(), r.u32(), r.u32()),
      Size
    )

    given Decoder[StatCount] = ConfiguredDecoder.derived

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
       Dispute.codec).xmap(
        { case (tickets, preimages, guarantees, assurances, disputes) =>
          StatExtrinsic(tickets, preimages, guarantees, assurances, disputes)
        },
        se => (se.tickets, se.preimages, se.guarantees, se.assurances, se.disputes)
      )

    given Decoder[StatExtrinsic] = ConfiguredDecoder.derived

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


    given Decoder[StatInput] = ConfiguredDecoder.derived

  /**
   * Statistics state containing validator stats and metadata.
   */
  final case class StatState(
    valsCurrStats: List[StatCount],
    valsLastStats: List[StatCount],
    slot: Long,
    currValidators: IndexedSeq[ValidatorKey],
    prevValidators: IndexedSeq[ValidatorKey] = IndexedSeq.empty
  )

  object StatState:
    given codec: Codec[StatState] =
      // 0.8.0: ValidatorsStatistics is a bounded sequence (compact length prefix).
      (JamCodecs.compactPrefixedList(summon[Codec[StatCount]]) ::
       JamCodecs.compactPrefixedList(summon[Codec[StatCount]]) ::
       uint32L ::
       FullJamStateCodecs.validatorListCodec).xmap(
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
          currValidators <- cursor.get[IndexedSeq[ValidatorKey]]("curr_validators")
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
      (StatInput.codec(config) :: StatState.codec :: StatState.codec).xmap(
        { case (input, preState, postState) =>
          StatCase(input, preState, None, postState)
        },
        sc => (sc.input, sc.preState, sc.postState)
      )

    given Decoder[StatCase] = ConfiguredDecoder.derived
