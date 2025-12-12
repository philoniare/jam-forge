package io.forge.jam.core.types

import io.forge.jam.core.{ChainConfig, JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode}
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
    given JamEncoder[Extrinsic] with
      def encode(a: Extrinsic): JamBytes =
        val builder = JamBytes.newBuilder
        // tickets - compact length prefix + fixed-size items
        builder ++= codec.encodeCompactInteger(a.tickets.length.toLong)
        for ticket <- a.tickets do
          builder ++= ticket.encode
        // preimages - compact length prefix + variable-size items
        builder ++= codec.encodeCompactInteger(a.preimages.length.toLong)
        for preimage <- a.preimages do
          builder ++= preimage.encode
        // guarantees - compact length prefix + variable-size items
        builder ++= codec.encodeCompactInteger(a.guarantees.length.toLong)
        for guarantee <- a.guarantees do
          builder ++= guarantee.encode
        // assurances - compact length prefix + config-dependent items
        builder ++= codec.encodeCompactInteger(a.assurances.length.toLong)
        for assurance <- a.assurances do
          builder ++= assurance.encode
        // disputes - variable size
        builder ++= a.disputes.encode
        builder.result()

    /**
     * Create a decoder that knows the config-dependent sizes.
     */
    def decoder(config: ChainConfig): JamDecoder[Extrinsic] =
      decoder(config.coresCount, config.votesPerVerdict)

    /**
     * Create a decoder that knows the cores count and votes per verdict.
     */
    def decoder(coresCount: Int, votesPerVerdict: Int): JamDecoder[Extrinsic] = new JamDecoder[Extrinsic]:
      def decode(bytes: JamBytes, offset: Int): (Extrinsic, Int) =
        val arr = bytes.toArray
        var pos = offset

        // tickets - compact length prefix + fixed-size items
        val (ticketsLength, ticketsLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += ticketsLengthBytes
        val tickets = (0 until ticketsLength.toInt).map { _ =>
          val (ticket, consumed) = TicketEnvelope.given_JamDecoder_TicketEnvelope.decode(bytes, pos)
          pos += consumed
          ticket
        }.toList

        // preimages - compact length prefix + variable-size items
        val (preimagesLength, preimagesLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += preimagesLengthBytes
        val preimages = (0 until preimagesLength.toInt).map { _ =>
          val (preimage, consumed) = Preimage.given_JamDecoder_Preimage.decode(bytes, pos)
          pos += consumed
          preimage
        }.toList

        // guarantees - compact length prefix + variable-size items
        val (guaranteesLength, guaranteesLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += guaranteesLengthBytes
        val guarantees = (0 until guaranteesLength.toInt).map { _ =>
          val (guarantee, consumed) = GuaranteeExtrinsic.given_JamDecoder_GuaranteeExtrinsic.decode(bytes, pos)
          pos += consumed
          guarantee
        }.toList

        // assurances - compact length prefix + config-dependent items
        val (assurancesLength, assurancesLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += assurancesLengthBytes
        val assuranceDecoder = AssuranceExtrinsic.decoder(coresCount)
        val assurances = (0 until assurancesLength.toInt).map { _ =>
          val (assurance, consumed) = assuranceDecoder.decode(bytes, pos)
          pos += consumed
          assurance
        }.toList

        // disputes - variable size (config-dependent)
        val disputeDecoder = Dispute.decoder(votesPerVerdict)
        val (disputes, disputesConsumed) = disputeDecoder.decode(bytes, pos)
        pos += disputesConsumed

        (Extrinsic(tickets, preimages, guarantees, assurances, disputes), pos - offset)

  /**
   * A complete block containing header and extrinsic.
   */
  final case class Block(
    header: Header,
    extrinsic: Extrinsic
  )

  object Block:
    /**
     * Encoder that works for any block (config-independent encoding).
     */
    given JamEncoder[Block] with
      def encode(a: Block): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.header.encode
        builder ++= a.extrinsic.encode
        builder.result()

    /**
     * Create a decoder that knows the config-dependent sizes.
     */
    def decoder(config: ChainConfig): JamDecoder[Block] =
      decoder(config.validatorCount, config.epochLength, config.coresCount, config.votesPerVerdict)

    /**
     * Create a decoder that knows all config-dependent parameters.
     */
    def decoder(validatorCount: Int, epochLength: Int, coresCount: Int, votesPerVerdict: Int): JamDecoder[Block] =
      new JamDecoder[Block]:
        def decode(bytes: JamBytes, offset: Int): (Block, Int) =
          var pos = offset

          // header - variable size (config-dependent)
          val headerDecoder = Header.decoder(validatorCount, epochLength)
          val (header, headerBytes) = headerDecoder.decode(bytes, pos)
          pos += headerBytes

          // extrinsic - variable size (config-dependent)
          val extrinsicDecoder = Extrinsic.decoder(coresCount, votesPerVerdict)
          val (extrinsic, extrinsicBytes) = extrinsicDecoder.decode(bytes, pos)
          pos += extrinsicBytes

          (Block(header, extrinsic), pos - offset)

    /**
     * Convenience method to decode with config.
     */
    def fromBytes(bytes: JamBytes, offset: Int, config: ChainConfig): (Block, Int) =
      decoder(config).decode(bytes, offset)
