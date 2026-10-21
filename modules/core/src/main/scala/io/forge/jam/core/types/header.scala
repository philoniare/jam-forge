package io.forge.jam.core.types

import io.circe.Decoder
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.forge.jam.core.primitives.{Hash, ValidatorIndex, Timeslot}
import io.forge.jam.core.types.epoch.EpochMark
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.codec.PrimitiveCodecs
import io.forge.jam.core.codec.{ByteReader, ByteWriter, Codec}

/**
 * Block header type.
 */
object header:

  /** Entropy source size */
  val EntropySourceSize: Int = 96

  /** Seal size (Ed25519 signature 64 bytes + VRF output 32 bytes) */
  val SealSize: Int = 96

  /**
   * Block header containing all block metadata.
   *
   * Encoding order:
   * - parent: 32 bytes
   * - parentStateRoot: 32 bytes
   * - extrinsicHash: 32 bytes
   * - slot: 4 bytes (little-endian)
   * - epochMark: 0/1 prefix + EpochMark (validatorCount-dependent)
   * - ticketsMark: 0/1 prefix + epochLength * TicketMark (33 bytes each)
   * - authorIndex: 2 bytes (little-endian)
   * - entropySource: 96 bytes
   * - offendersMark: compact length prefix + hashes
   * - seal: 96 bytes
   */
  final case class Header(
    parent: Hash,
    parentStateRoot: Hash,
    extrinsicHash: Hash,
    slot: Timeslot,
    epochMark: Option[EpochMark],
    ticketsMark: Option[List[TicketMark]],
    authorIndex: ValidatorIndex,
    entropySource: JamBytes,
    offendersMark: List[Hash],
    seal: JamBytes
  ):
    require(
      entropySource.length == EntropySourceSize,
      s"Entropy source must be $EntropySourceSize bytes, got ${entropySource.length}"
    )
    require(seal.length == SealSize, s"Seal must be $SealSize bytes, got ${seal.length}")

  object Header:
    /**
     * Create a header codec that knows the config-dependent sizes.
     */
    def headerCodec(validatorCount: Int, epochLength: Int): Codec[Header] =
      val epochMarkOpt = JamCodecs.optionCodec(EpochMark.epochMarkCodec(validatorCount))
      val ticketsMarkOpt = JamCodecs.optionCodec(JamCodecs.fixedSizeList(summon[Codec[TicketMark]], epochLength))
      val offendersCodec = JamCodecs.compactPrefixedList(JamCodecs.hashCodec)
      Codec.of(
        (w, h) =>
          JamCodecs.hashCodec.write(w, h.parent)
          JamCodecs.hashCodec.write(w, h.parentStateRoot)
          JamCodecs.hashCodec.write(w, h.extrinsicHash)
          PrimitiveCodecs.timeslot.write(w, h.slot)
          epochMarkOpt.write(w, h.epochMark)
          ticketsMarkOpt.write(w, h.ticketsMark)
          PrimitiveCodecs.validatorIndex.write(w, h.authorIndex)
          w.bytes(h.entropySource)
          offendersCodec.write(w, h.offendersMark)
          w.bytes(h.seal),
        r =>
          val parent = JamCodecs.hashCodec.read(r)
          val parentStateRoot = JamCodecs.hashCodec.read(r)
          val extrinsicHash = JamCodecs.hashCodec.read(r)
          val slot = PrimitiveCodecs.timeslot.read(r)
          val epochMark = epochMarkOpt.read(r)
          val ticketsMark = ticketsMarkOpt.read(r)
          val authorIndex = PrimitiveCodecs.validatorIndex.read(r)
          val entropySource = JamBytes.wrap(r.bytes(EntropySourceSize))
          val offendersMark = offendersCodec.read(r)
          val seal = JamBytes.wrap(r.bytes(SealSize))
          Header(parent, parentStateRoot, extrinsicHash, slot, epochMark, ticketsMark, authorIndex, entropySource,
            offendersMark, seal),
        256
      )

    /**
     * Create a codec with config.
     */
    def headerCodec(config: ChainConfig): Codec[Header] =
      headerCodec(config.validatorCount, config.epochLength)

    /**
     * Default codec for encoding (can infer sizes from data).
     * For decoding, use headerCodec with config parameters.
     */
    given Codec[Header] = new Codec[Header] {
      def write(out: ByteWriter, header: Header): Unit =
        // For encoding, we can infer the sizes from the data
        val validatorCount = header.epochMark.map(_.validators.length).getOrElse(0)
        val epochLength = header.ticketsMark.map(_.length).getOrElse(0)
        // Use a reasonable default if we can't infer (encoding doesn't need exact counts)
        headerCodec(
          if validatorCount > 0 then validatorCount else 6,
          if epochLength > 0 then epochLength else 12
        ).write(out, header)

      def read(in: ByteReader): Header =
        in.fail("Use headerCodec(validatorCount, epochLength) for decoding")
    }

    /**
     * Convenience method to decode with config.
     */
    def fromBytes(bytes: Array[Byte], config: ChainConfig): Either[String, Header] =
      headerCodec(config).decode(bytes)

    /**
     * Convenience method to decode with config at offset, returning the header and the bytes consumed.
     */
    def fromBytes(bytes: Array[Byte], offset: Int, config: ChainConfig): Either[String, (Header, Int)] =
      headerCodec(config).decodeAt(bytes, offset)


    // ============================================================================
    // JSON Decoder
    // ============================================================================

    given Decoder[Header] = Decoder.instance { cursor =>
      for
        parent <- cursor.get[Hash]("parent")
        parentStateRoot <- cursor.get[Hash]("parent_state_root")
        extrinsicHash <- cursor.get[Hash]("extrinsic_hash")
        slot <- cursor.get[Long]("slot")
        epochMark <- cursor.get[Option[EpochMark]]("epoch_mark")
        ticketsMark <- cursor.get[Option[List[TicketMark]]]("tickets_mark")
        authorIndex <- cursor.get[Int]("author_index")
        entropySourceHex <- cursor.get[String]("entropy_source")
        offendersMark <- cursor.get[List[Hash]]("offenders_mark")
        sealHex <- cursor.get[String]("seal")
      yield Header(
        parent,
        parentStateRoot,
        extrinsicHash,
        Timeslot(slot.toInt),
        epochMark,
        ticketsMark,
        ValidatorIndex(authorIndex),
        JamBytes(parseHex(entropySourceHex)),
        offendersMark,
        JamBytes(parseHex(sealHex))
      )
    }
