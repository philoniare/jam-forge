package io.forge.jam.core.types

import io.circe.derivation.ConfiguredDecoder
import io.forge.jam.core.json.JsonConfig.given
import io.circe.Decoder
import io.forge.jam.core.codec.*
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.compactPrefixedList
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
    private val codecs = new java.util.concurrent.ConcurrentHashMap[Int, Codec[Extrinsic]]()

    /** Built once per core count. */
    def extrinsicCodec(coresCount: Int): Codec[Extrinsic] = codecs.computeIfAbsent(coresCount, build)

    private def build(coresCount: Int): Codec[Extrinsic] =
      (compactPrefixedList(summon[Codec[TicketEnvelope]]) ::
       compactPrefixedList(summon[Codec[Preimage]]) ::
       compactPrefixedList(summon[Codec[GuaranteeExtrinsic]]) ::
       compactPrefixedList(AssuranceExtrinsic.codec(coresCount)) ::
       Dispute.codec).xmap(
        { case (tickets, preimages, guarantees, assurances, disputes) =>
          Extrinsic(tickets, preimages, guarantees, assurances, disputes)
        },
        e => (e.tickets, e.preimages, e.guarantees, e.assurances, e.disputes)
      )

    /**
     * Create a codec that knows the config-dependent sizes.
     */
    def extrinsicCodec(config: ChainConfig): Codec[Extrinsic] =
      extrinsicCodec(config.coresCount)

    given Decoder[Extrinsic] = ConfiguredDecoder.derived

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
     * @param epochLength The epoch length in slots
     * @param coresCount The number of cores
     */
    private val codecs = new java.util.concurrent.ConcurrentHashMap[(Int, Int), Codec[Block]]()

    /** Built once per (epoch length, core count). */
    def blockCodec(epochLength: Int, coresCount: Int): Codec[Block] =
      codecs.computeIfAbsent((epochLength, coresCount), _ => build(epochLength, coresCount))

    private def build(epochLength: Int, coresCount: Int): Codec[Block] =
      (Header.headerCodec(epochLength) :: Extrinsic.extrinsicCodec(coresCount)).xmap(
        { case (header, extrinsic) => Block(header, extrinsic) },
        b => (b.header, b.extrinsic)
      )

    /**
     * Create a codec that knows the config-dependent sizes.
     */
    def blockCodec(config: ChainConfig): Codec[Block] =
      blockCodec(config.epochLength, config.coresCount)

    /**
     * Convenience method to decode with config.
     */
    def fromBytes(bytes: Array[Byte], config: ChainConfig): Either[String, Block] =
      blockCodec(config).decode(bytes)

    given Decoder[Block] = ConfiguredDecoder.derived
