package io.forge.jam.core.types

import io.circe.Decoder
import io.forge.jam.core.{ChainConfig, JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode, decodeAs}
import io.forge.jam.core.primitives.{Hash, ValidatorIndex, Timeslot}
import io.forge.jam.core.types.epoch.EpochMark
import io.forge.jam.core.types.tickets.TicketMark

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
    given JamEncoder[Header] with
      def encode(a: Header): JamBytes =
        val builder = JamBytes.newBuilder
        // parent - 32 bytes
        builder ++= a.parent.bytes
        // parentStateRoot - 32 bytes
        builder ++= a.parentStateRoot.bytes
        // extrinsicHash - 32 bytes
        builder ++= a.extrinsicHash.bytes
        // slot - 4 bytes
        builder ++= codec.encodeU32LE(a.slot.value)
        // epochMark - 0/1 prefix + data
        a.epochMark match
          case None =>
            builder += 0x00.toByte
          case Some(em) =>
            builder += 0x01.toByte
            builder ++= em.encode
        // ticketsMark - 0/1 prefix + epochLength items (no length prefix, fixed count)
        a.ticketsMark match
          case None =>
            builder += 0x00.toByte
          case Some(marks) =>
            builder += 0x01.toByte
            for mark <- marks do
              builder ++= mark.encode
        // authorIndex - 2 bytes
        builder ++= codec.encodeU16LE(a.authorIndex.value)
        // entropySource - 96 bytes
        builder ++= a.entropySource
        // offendersMark - compact length prefix + hashes
        builder ++= codec.encodeCompactInteger(a.offendersMark.length.toLong)
        for offender <- a.offendersMark do
          builder ++= offender.bytes
        // seal - 96 bytes
        builder ++= a.seal
        builder.result()

    /**
     * Create a decoder that knows the config-dependent sizes.
     */
    def decoder(config: ChainConfig): JamDecoder[Header] = new JamDecoder[Header]:
      def decode(bytes: JamBytes, offset: Int): (Header, Int) =
        decoder(config.validatorCount, config.epochLength).decode(bytes, offset)

    /**
     * Create a decoder that knows the validator count and epoch length.
     */
    def decoder(validatorCount: Int, epochLength: Int): JamDecoder[Header] = new JamDecoder[Header]:
      def decode(bytes: JamBytes, offset: Int): (Header, Int) =
        val arr = bytes.toArray
        var pos = offset

        // parent - 32 bytes
        val parent = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // parentStateRoot - 32 bytes
        val parentStateRoot = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // extrinsicHash - 32 bytes
        val extrinsicHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size

        // slot - 4 bytes
        val slot = Timeslot(codec.decodeU32LE(arr, pos))
        pos += 4

        // epochMark - 0/1 prefix
        val epochMarkFlag = arr(pos) & 0xff
        pos += 1
        val epochMark = if epochMarkFlag == 1 then
          val epochMarkDecoder = EpochMark.decoder(validatorCount)
          val (em, consumed) = epochMarkDecoder.decode(bytes, pos)
          pos += consumed
          Some(em)
        else None

        // ticketsMark - 0/1 prefix + epochLength items
        val ticketsMarkFlag = arr(pos) & 0xff
        pos += 1
        val ticketsMark = if ticketsMarkFlag == 1 then
          val marks = (0 until epochLength).map { _ =>
            val (mark, consumed) = bytes.decodeAs[TicketMark](pos)
            pos += consumed
            mark
          }.toList
          Some(marks)
        else None

        // authorIndex - 2 bytes
        val authorIndex = ValidatorIndex(codec.decodeU16LE(arr, pos))
        pos += 2

        // entropySource - 96 bytes
        val entropySource = bytes.slice(pos, pos + EntropySourceSize)
        pos += EntropySourceSize

        // offendersMark - compact length prefix + hashes
        val (offendersLength, offendersLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += offendersLengthBytes
        val offendersMark = (0 until offendersLength.toInt).map { _ =>
          val hash = Hash(arr.slice(pos, pos + Hash.Size))
          pos += Hash.Size
          hash
        }.toList

        // seal - 96 bytes
        val seal = bytes.slice(pos, pos + SealSize)
        pos += SealSize

        (
          Header(
            parent,
            parentStateRoot,
            extrinsicHash,
            slot,
            epochMark,
            ticketsMark,
            authorIndex,
            entropySource,
            offendersMark,
            seal
          ),
          pos - offset
        )

    /**
     * Convenience method to decode with config.
     */
    def fromBytes(bytes: JamBytes, offset: Int, config: ChainConfig): (Header, Int) =
      decoder(config).decode(bytes, offset)

    given Decoder[Header] = Decoder.instance { cursor =>
      for
        parent <- cursor.get[Hash]("parent")
        parentStateRoot <- cursor.get[Hash]("parent_state_root")
        extrinsicHash <- cursor.get[Hash]("extrinsic_hash")
        slot <- cursor.get[Long]("slot")
        epochMark <- cursor.get[Option[EpochMark]]("epoch_mark")
        ticketsMark <- cursor.get[Option[List[TicketMark]]]("tickets_mark")
        authorIndex <- cursor.get[Int]("author_index")
        entropySource <- cursor.get[JamBytes]("entropy_source")
        offendersMark <- cursor.get[List[Hash]]("offenders_mark")
        seal <- cursor.get[JamBytes]("seal")
      yield Header(
        parent,
        parentStateRoot,
        extrinsicHash,
        Timeslot(slot.toInt),
        epochMark,
        ticketsMark,
        ValidatorIndex(authorIndex),
        entropySource,
        offendersMark,
        seal
      )
    }
