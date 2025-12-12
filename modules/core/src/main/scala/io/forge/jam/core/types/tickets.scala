package io.forge.jam.core.types

import io.forge.jam.core.{JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder
import spire.math.UByte

/**
 * Ticket-related types
 */
object tickets:

  /** Ring VRF signature size in bytes */
  val RingVrfSignatureSize: Int = 784

  /**
   * A ticket envelope containing an attempt index and ring VRF signature.
   */
  final case class TicketEnvelope(
    attempt: UByte,
    signature: JamBytes
  ):
    require(signature.length == RingVrfSignatureSize,
      s"Signature must be $RingVrfSignatureSize bytes, got ${signature.length}")

  object TicketEnvelope:
    val Size: Int = 1 + RingVrfSignatureSize // 785 bytes

    given JamEncoder[TicketEnvelope] with
      def encode(a: TicketEnvelope): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeU8(a.attempt)
        builder ++= a.signature
        builder.result()

    given JamDecoder[TicketEnvelope] with
      def decode(bytes: JamBytes, offset: Int): (TicketEnvelope, Int) =
        val attempt = codec.decodeU8(bytes.toArray, offset)
        val signature = bytes.slice(offset + 1, offset + 1 + RingVrfSignatureSize)
        (TicketEnvelope(attempt, signature), Size)

    given Decoder[TicketEnvelope] = Decoder.instance { cursor =>
      for
        attempt <- cursor.get[Int]("attempt")
        signature <- cursor.get[String]("signature")
      yield TicketEnvelope(UByte(attempt), JamBytes(parseHex(signature)))
    }

  /**
   * A ticket mark identifying a ticket by its ID and attempt index.
   * Fixed size: 33 bytes (32 bytes id + 1 byte attempt)
   */
  final case class TicketMark(
    id: JamBytes,
    attempt: UByte
  ):
    require(id.length == Hash.Size,
      s"ID must be ${Hash.Size} bytes, got ${id.length}")

  object TicketMark:
    val Size: Int = Hash.Size + 1 // 33 bytes

    given JamEncoder[TicketMark] with
      def encode(a: TicketMark): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.id
        builder ++= codec.encodeU8(a.attempt)
        builder.result()

    given JamDecoder[TicketMark] with
      def decode(bytes: JamBytes, offset: Int): (TicketMark, Int) =
        val id = bytes.slice(offset, offset + Hash.Size)
        val attempt = codec.decodeU8(bytes.toArray, offset + Hash.Size)
        (TicketMark(id, attempt), Size)
