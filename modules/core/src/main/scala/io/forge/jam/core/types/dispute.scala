package io.forge.jam.core.types

import scodec.*
import scodec.bits.*
import scodec.codecs.*
import io.forge.jam.core.primitives.{Hash, ValidatorIndex, Ed25519PublicKey, Ed25519Signature}
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.forge.jam.core.scodec.JamCodecs.{hashCodec, ed25519PublicKeyCodec}
import io.circe.Decoder

/**
 * Dispute-related types
 */
object dispute:

  private val ed25519SigCodec: Codec[Ed25519Signature] =
    fixedSizeBytes(Ed25519Signature.Size.toLong, bytes).xmap(
      bv => Ed25519Signature(bv.toArray),
      sig => ByteVector(sig.bytes)
    )

  /**
   * A culprit in a dispute - a validator who made a false guarantee.
   * Fixed size: 128 bytes (32 bytes target + 32 bytes key + 64 bytes signature)
   */
  final case class Culprit(
    target: Hash,
    key: Ed25519PublicKey,
    signature: Ed25519Signature
  )

  object Culprit:
    val Size: Int = Hash.Size + Ed25519PublicKey.Size + Ed25519Signature.Size // 128 bytes

    given Codec[Culprit] =
      (hashCodec :: ed25519PublicKeyCodec :: ed25519SigCodec).as[Culprit]

    given Decoder[Culprit] = Decoder.instance { cursor =>
      for
        target <- cursor.get[String]("target")
        key <- cursor.get[String]("key")
        signature <- cursor.get[String]("signature")
      yield Culprit(Hash(parseHex(target)), Ed25519PublicKey(parseHex(key)), Ed25519Signature(parseHex(signature)))
    }

  /**
   * A fault in a dispute - a validator who voted incorrectly.
   * Fixed size: 129 bytes (32 bytes target + 1 byte vote + 32 bytes key + 64 bytes signature)
   */
  final case class Fault(
    target: Hash,
    vote: Boolean,
    key: Ed25519PublicKey,
    signature: Ed25519Signature
  )

  object Fault:
    val Size: Int = Hash.Size + 1 + Ed25519PublicKey.Size + Ed25519Signature.Size // 129 bytes

    given Codec[Fault] =
      (hashCodec :: byte :: ed25519PublicKeyCodec :: ed25519SigCodec).xmap(
        { case (target, voteByte, key, sig) =>
          Fault(target, voteByte != 0, key, sig)
        },
        f => (f.target, (if f.vote then 1 else 0).toByte, f.key, f.signature)
      )

    given Decoder[Fault] = Decoder.instance { cursor =>
      for
        target <- cursor.get[String]("target")
        vote <- cursor.get[Boolean]("vote")
        key <- cursor.get[String]("key")
        signature <- cursor.get[String]("signature")
      yield Fault(Hash(parseHex(target)), vote, Ed25519PublicKey(parseHex(key)), Ed25519Signature(parseHex(signature)))
    }

  /**
   * A guarantee signature from a validator.
   * Fixed size: 66 bytes (2 bytes validator index + 64 bytes signature)
   */
  final case class GuaranteeSignature(
    validatorIndex: ValidatorIndex,
    signature: Ed25519Signature
  )

  object GuaranteeSignature:
    val Size: Int = 2 + Ed25519Signature.Size // 66 bytes

    given Codec[GuaranteeSignature] =
      (uint16L :: ed25519SigCodec).xmap(
        { case (idx, sig) => GuaranteeSignature(ValidatorIndex(idx), sig) },
        gs => (gs.validatorIndex.value.toInt, gs.signature)
      )

    given Decoder[GuaranteeSignature] = Decoder.instance { cursor =>
      for
        validatorIndex <- cursor.get[Int]("validator_index")
        signature <- cursor.get[String]("signature")
      yield GuaranteeSignature(ValidatorIndex(validatorIndex), Ed25519Signature(parseHex(signature)))
    }
