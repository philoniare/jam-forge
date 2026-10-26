package io.forge.jam.core.types

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.json.JsonHelpers.{parseHex, parseHexBytesFixed}
import io.circe.Decoder
import io.forge.jam.core.codec.*
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
    /** On-wire size: 1-byte attempt + signature. */
    val Size: Int = 1 + RingVrfSignatureSize // 785 bytes

    /** The attempt is a fixed one-octet E_1, not a general natural, so an attempt of
      * 128..255 is still one byte on the wire.
      */
    given Codec[TicketEnvelope] = Codec.of(
      (w, te) =>
        w.u8(te.attempt.toInt)
        w.bytes(te.signature),
      r =>
        val attempt = UByte(r.u8())
        TicketEnvelope(attempt, JamBytes.wrap(r.bytes(RingVrfSignatureSize)))
      ,
      Size
    )

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

    given Codec[TicketMark] = Codec.of(
      (w, tm) =>
        w.bytes(tm.id)
        w.u8(tm.attempt.toInt),
      r =>
        val id = JamBytes.wrap(r.bytes(Hash.Size))
        TicketMark(id, UByte(r.u8())),
      Size
    )

    given Decoder[TicketMark] = Decoder.instance { cursor =>
      for
        idHex <- cursor.get[String]("id")
        attempt <- cursor.get[Int]("attempt")
        id <- parseHexBytesFixed(idHex, Hash.Size).map(bytes => JamBytes(bytes))
      yield TicketMark(id, UByte(attempt))
    }
