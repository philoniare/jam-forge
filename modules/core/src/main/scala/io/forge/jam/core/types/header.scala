package io.forge.jam.core.types

import io.circe.Decoder
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.forge.jam.core.primitives.{Hash, ValidatorIndex, Timeslot}
import io.forge.jam.core.types.epoch.EpochMark
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.scodec.JamCodecs
import _root_.scodec.*
import _root_.scodec.bits.*
import _root_.scodec.codecs.*
import spire.math.{UInt, UShort}

/**
 * Block header type.
 */
object header:

  /** Entropy source size */
  val EntropySourceSize: Int = 96

  /** Seal size (Ed25519 signature 64 bytes + VRF output 32 bytes) */
  val SealSize: Int = 96

  // Private codecs for primitive types
  private val timeslotCodec: Codec[Timeslot] = uint32L.xmap(
    v => Timeslot(UInt(v.toInt)),
    ts => ts.value.toLong & 0xFFFFFFFFL
  )

  private val validatorIndexCodec: Codec[ValidatorIndex] = uint16L.xmap(
    v => ValidatorIndex(UShort(v)),
    vi => vi.value.toInt
  )

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
      val ticketsMarkOpt = JamCodecs.optionCodec(
        vectorOfN(provide(epochLength), summon[Codec[TicketMark]]).xmap(_.toList, _.toVector)
      )
      val offendersCodec = JamCodecs.compactInteger.flatZip { count =>
        vectorOfN(provide(count.toInt), JamCodecs.hashCodec).xmap(_.toList, _.toVector)
      }.xmap(_._2, list => (list.length.toLong, list))

      (JamCodecs.hashCodec ::
        JamCodecs.hashCodec ::
        JamCodecs.hashCodec ::
        timeslotCodec ::
        epochMarkOpt ::
        ticketsMarkOpt ::
        validatorIndexCodec ::
        fixedSizeBytes(EntropySourceSize.toLong, bytes) ::
        offendersCodec ::
        fixedSizeBytes(SealSize.toLong, bytes)).xmap(
        { case (parent, parentStateRoot, extrinsicHash, slot, epochMark, ticketsMark,
                authorIndex, entropySource, offendersMark, seal) =>
          Header(parent, parentStateRoot, extrinsicHash, slot, epochMark, ticketsMark,
            authorIndex, JamBytes.fromByteVector(entropySource), offendersMark, JamBytes.fromByteVector(seal))
        },
        h => (h.parent, h.parentStateRoot, h.extrinsicHash, h.slot, h.epochMark, h.ticketsMark,
          h.authorIndex, h.entropySource.toByteVector, h.offendersMark, h.seal.toByteVector)
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
      def sizeBound: SizeBound = SizeBound.unknown

      def encode(header: Header): Attempt[BitVector] =
        // For encoding, we can infer the sizes from the data
        val validatorCount = header.epochMark.map(_.validators.length).getOrElse(0)
        val epochLength = header.ticketsMark.map(_.length).getOrElse(0)
        // Use a reasonable default if we can't infer (encoding doesn't need exact counts)
        headerCodec(
          if validatorCount > 0 then validatorCount else 6,
          if epochLength > 0 then epochLength else 12
        ).encode(header)

      def decode(bits: BitVector): Attempt[DecodeResult[Header]] =
        Attempt.failure(Err("Use headerCodec(validatorCount, epochLength) for decoding"))
    }

    /**
     * Convenience method to decode with config.
     */
    def fromBytes(bytes: ByteVector, config: ChainConfig): Attempt[DecodeResult[Header]] =
      headerCodec(config).decode(bytes.bits)

    /**
     * Convenience method to decode with config at offset.
     */
    def fromBytes(bytes: ByteVector, offset: Int, config: ChainConfig): Attempt[DecodeResult[Header]] =
      headerCodec(config).decode(bytes.drop(offset.toLong).bits)


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
