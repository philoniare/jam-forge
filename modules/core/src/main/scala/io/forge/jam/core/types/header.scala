package io.forge.jam.core.types

import io.circe.Decoder
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder}
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.forge.jam.core.primitives.{Hash, ValidatorIndex, Timeslot}
import io.forge.jam.core.types.epoch.EpochMark
import io.forge.jam.core.types.tickets.TicketMark
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
  private val hashCodec: Codec[Hash] = fixedSizeBytes(32L, bytes).xmap(
    bv => Hash.fromByteVectorUnsafe(bv),
    h => h.toByteVector
  )

  private val timeslotCodec: Codec[Timeslot] = uint32L.xmap(
    v => Timeslot(UInt(v.toInt)),
    ts => ts.value.toLong & 0xFFFFFFFFL
  )

  private val validatorIndexCodec: Codec[ValidatorIndex] = uint16L.xmap(
    v => ValidatorIndex(UShort(v)),
    vi => vi.value.toInt
  )

  /**
   * JAM compact integer codec.
   * Encodes non-negative integers using variable-length format.
   */
  private val compactIntCodec: Codec[Long] = new Codec[Long] {
    def sizeBound: SizeBound = SizeBound.bounded(8, 72) // 1 to 9 bytes

    def encode(value: Long): Attempt[BitVector] =
      if value < 0 then
        Attempt.failure(Err("No negative values allowed"))
      else if value == 0 then
        Attempt.successful(BitVector(0x00))
      else
        // Find l such that 2^(7l) <= x < 2^(7(l+1)) for l in [0..8]
        var l = 0
        var found = false
        var result: BitVector = BitVector.empty
        while l <= 8 && !found do
          val lowerBound = 1L << (7 * l)
          val upperBound = 1L << (7 * (l + 1))
          if value >= lowerBound && value < upperBound then
            val prefixVal = (256 - (1 << (8 - l))) + (value >>> (8 * l))
            val remainder = value & ((1L << (8 * l)) - 1)
            val arr = new Array[Byte](1 + l)
            arr(0) = prefixVal.toByte
            for i <- 0 until l do
              arr(1 + i) = ((remainder >> (8 * i)) & 0xFF).toByte
            result = BitVector(arr)
            found = true
          l += 1
        if !found then
          // Fallback: [255] ++ E_8(x)
          val arr = new Array[Byte](9)
          arr(0) = 0xFF.toByte
          for i <- 0 until 8 do
            arr(1 + i) = ((value >> (8 * i)) & 0xFF).toByte
          result = BitVector(arr)
        Attempt.successful(result)

    def decode(bits: BitVector): Attempt[DecodeResult[Long]] =
      if bits.isEmpty then
        Attempt.failure(Err.insufficientBits(8, 0))
      else
        val prefix = (bits.take(8).toByteVector.head & 0xFF)
        if prefix == 0 then
          Attempt.successful(DecodeResult(0L, bits.drop(8)))
        else
          val l =
            if prefix < 128 then 0
            else if prefix < 192 then 1
            else if prefix < 224 then 2
            else if prefix < 240 then 3
            else if prefix < 248 then 4
            else if prefix < 252 then 5
            else if prefix < 254 then 6
            else if prefix < 255 then 7
            else 8

          if prefix == 255 then
            val needed = 72 // 8 bits prefix + 64 bits value
            if bits.size < needed then
              Attempt.failure(Err.insufficientBits(needed, bits.size))
            else
              val valueBytes = bits.drop(8).take(64).toByteVector
              var value = 0L
              for i <- 0 until 8 do
                value = value | ((valueBytes(i).toLong & 0xFF) << (8 * i))
              Attempt.successful(DecodeResult(value, bits.drop(72)))
          else if l == 0 then
            Attempt.successful(DecodeResult(prefix.toLong, bits.drop(8)))
          else
            val needed = 8 + l * 8
            if bits.size < needed then
              Attempt.failure(Err.insufficientBits(needed, bits.size))
            else
              val base = 256 - (1 << (8 - l))
              val highBits = (prefix - base).toLong << (8 * l)
              val lowBytes = bits.drop(8).take(l * 8).toByteVector
              var lowBits = 0L
              for i <- 0 until l do
                lowBits = lowBits | ((lowBytes(i).toLong & 0xFF) << (8 * i))
              Attempt.successful(DecodeResult(highBits | lowBits, bits.drop(needed)))
  }

  /**
   * Option codec using 0/1 discriminator byte.
   */
  private def optionCodec[A](codec: Codec[A]): Codec[Option[A]] =
    discriminated[Option[A]].by(byte)
      .subcaseP(0) { case None => None }(provide(None))
      .subcaseP(1) { case Some(v) => Some(v) }(codec.xmap(Some(_), _.get))

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
      val epochMarkOpt = optionCodec(EpochMark.epochMarkCodec(validatorCount))
      val ticketsMarkOpt = optionCodec(
        vectorOfN(provide(epochLength), summon[Codec[TicketMark]]).xmap(_.toList, _.toVector)
      )
      val offendersCodec = compactIntCodec.flatZip { count =>
        vectorOfN(provide(count.toInt), hashCodec).xmap(_.toList, _.toVector)
      }.xmap(_._2, list => (list.length.toLong, list))

      (hashCodec ::
        hashCodec ::
        hashCodec ::
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
    // Legacy JamEncoder/JamDecoder compatibility
    // ============================================================================

    /**
     * Legacy JamEncoder for backward compatibility.
     * Uses scodec under the hood.
     */
    given JamEncoder[Header] with
      def encode(h: Header): JamBytes =
        val codec = summon[Codec[Header]]
        JamBytes(codec.encode(h).require.toByteVector.toArray)

    /**
     * Legacy decoder factory for backward compatibility.
     * Returns a JamDecoder that wraps the scodec codec.
     */
    def decoder(config: ChainConfig): JamDecoder[Header] =
      decoder(config.validatorCount, config.epochLength)

    /**
     * Legacy decoder factory for backward compatibility.
     * Returns a JamDecoder that wraps the scodec codec.
     */
    def decoder(validatorCount: Int, epochLength: Int): JamDecoder[Header] = new JamDecoder[Header]:
      def decode(bytes: JamBytes, offset: Int): (Header, Int) =
        val codec = headerCodec(validatorCount, epochLength)
        val bv = ByteVector(bytes.toArray).drop(offset.toLong)
        val result = codec.decode(bv.bits).require
        val bytesConsumed = ((bv.bits.size - result.remainder.size) / 8).toInt
        (result.value, bytesConsumed)

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
