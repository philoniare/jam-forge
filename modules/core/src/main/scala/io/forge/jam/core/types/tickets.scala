package io.forge.jam.core.types

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.json.JsonHelpers.{parseHex, parseHexBytesFixed}
import io.forge.jam.core.scodec.JamCodecs
import io.circe.Decoder
import _root_.scodec.*
import _root_.scodec.codecs.*
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
    /** Minimum on-wire size: 1-byte compact attempt (value < 128) + signature. */
    val MinSize: Int = 1 + RingVrfSignatureSize // 785 bytes
    /** Maximum on-wire size: 2-byte compact attempt (value >= 128) + signature. */
    val MaxSize: Int = 2 + RingVrfSignatureSize // 786 bytes

    private val attemptCodec: Codec[UByte] =
      JamCodecs.compactInt.exmap[UByte](
        i =>
          if i < 0 || i > 255 then
            Attempt.failure(Err(s"attempt $i out of UByte range"))
          else Attempt.successful(UByte(i)),
        ub => Attempt.successful(ub.toInt)
      )

    given Codec[TicketEnvelope] =
      (attemptCodec :: fixedSizeBytes(RingVrfSignatureSize.toLong, bytes)).xmap(
        { case (attempt, sigBytes) =>
          TicketEnvelope(attempt, JamBytes.fromByteVector(sigBytes))
        },
        te => (te.attempt, te.signature.toByteVector)
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

    given Codec[TicketMark] =
      (fixedSizeBytes(Hash.Size.toLong, bytes) :: byte).xmap(
        { case (idBytes, attemptByte) =>
          TicketMark(JamBytes.fromByteVector(idBytes), UByte(attemptByte))
        },
        tm => (tm.id.toByteVector, tm.attempt.toByte)
      )

    given Decoder[TicketMark] = Decoder.instance { cursor =>
      for
        idHex <- cursor.get[String]("id")
        attempt <- cursor.get[Int]("attempt")
        id <- parseHexBytesFixed(idHex, Hash.Size).map(bytes => JamBytes(bytes))
      yield TicketMark(id, UByte(attempt))
    }
