package io.forge.jam.core.types

import io.circe.Decoder
import scodec.*
import scodec.bits.*
import scodec.codecs.*
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.scodec.JamCodecs.compactPrefixedList
import io.forge.jam.core.types.header.Header
import io.forge.jam.core.types.tickets.TicketEnvelope
import io.forge.jam.core.types.extrinsic.{Preimage, GuaranteeExtrinsic, AssuranceExtrinsic, Dispute}

/**
 * Block and Extrinsic container types.
 */
object block:

  /**
   * Block extrinsic containing all transaction data.
   *
   * Encoding order:
   * - tickets: compact length prefix + fixed-size items
   * - preimages: compact length prefix + variable-size items
   * - guarantees: compact length prefix + variable-size items
   * - assurances: compact length prefix + config-dependent items
   * - disputes: variable size (config-dependent)
   */
  final case class Extrinsic(
    tickets: List[TicketEnvelope],
    preimages: List[Preimage],
    guarantees: List[GuaranteeExtrinsic],
    assurances: List[AssuranceExtrinsic],
    disputes: Dispute
  )

  object Extrinsic:
    /**
     * Create a codec for Extrinsic with config-dependent sizes.
     *
     * @param coresCount The number of cores (affects assurance bitfield size)
     * @param votesPerVerdict The number of votes per verdict (affects dispute encoding)
     */
    def extrinsicCodec(coresCount: Int, votesPerVerdict: Int): Codec[Extrinsic] =
      (compactPrefixedList(summon[Codec[TicketEnvelope]]) ::
       compactPrefixedList(summon[Codec[Preimage]]) ::
       compactPrefixedList(summon[Codec[GuaranteeExtrinsic]]) ::
       compactPrefixedList(AssuranceExtrinsic.codec(coresCount)) ::
       Dispute.codec(votesPerVerdict)).xmap(
        { case (tickets, preimages, guarantees, assurances, disputes) =>
          Extrinsic(tickets, preimages, guarantees, assurances, disputes)
        },
        e => (e.tickets, e.preimages, e.guarantees, e.assurances, e.disputes)
      )

    /**
     * Create a codec that knows the config-dependent sizes.
     */
    def extrinsicCodec(config: ChainConfig): Codec[Extrinsic] =
      extrinsicCodec(config.coresCount, config.votesPerVerdict)

    given Decoder[Extrinsic] = Decoder.instance { cursor =>
      for
        tickets <- cursor.get[List[TicketEnvelope]]("tickets")
        preimages <- cursor.get[List[Preimage]]("preimages")
        guarantees <- cursor.get[List[GuaranteeExtrinsic]]("guarantees")
        assurances <- cursor.get[List[AssuranceExtrinsic]]("assurances")
        disputes <- cursor.get[Dispute]("disputes")
      yield Extrinsic(tickets, preimages, guarantees, assurances, disputes)
    }

  /**
   * A complete block containing header and extrinsic.
   */
  final case class Block(
    header: Header,
    extrinsic: Extrinsic
  )

  object Block:
    /**
     * Create a codec for Block with config-dependent sizes.
     *
     * @param validatorCount The number of validators
     * @param epochLength The epoch length in slots
     * @param coresCount The number of cores
     * @param votesPerVerdict The number of votes per verdict
     */
    def blockCodec(validatorCount: Int, epochLength: Int, coresCount: Int, votesPerVerdict: Int): Codec[Block] =
      (Header.headerCodec(validatorCount, epochLength) :: Extrinsic.extrinsicCodec(coresCount, votesPerVerdict)).xmap(
        { case (header, extrinsic) => Block(header, extrinsic) },
        b => (b.header, b.extrinsic)
      )

    /**
     * Create a codec that knows the config-dependent sizes.
     */
    def blockCodec(config: ChainConfig): Codec[Block] =
      blockCodec(config.validatorCount, config.epochLength, config.coresCount, config.votesPerVerdict)

    /**
     * Convenience method to decode with config.
     */
    def fromBytes(bytes: ByteVector, config: ChainConfig): Attempt[DecodeResult[Block]] =
      blockCodec(config).decode(bytes.bits)

    given Decoder[Block] = Decoder.instance { cursor =>
      for
        header <- cursor.get[Header]("header")
        extrinsic <- cursor.get[Extrinsic]("extrinsic")
      yield Block(header, extrinsic)
    }
