package io.forge.jam.core.types

import io.forge.jam.core.{JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder}
import io.forge.jam.core.primitives.{Hash, ValidatorIndex, Ed25519PublicKey, Ed25519Signature}
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.Decoder

/**
 * Dispute-related types
 */
object dispute:

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

    given JamEncoder[Culprit] with
      def encode(a: Culprit): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.target.bytes
        builder ++= a.key.bytes
        builder ++= a.signature.bytes
        builder.result()

    given JamDecoder[Culprit] with
      def decode(bytes: JamBytes, offset: Int): (Culprit, Int) =
        val arr = bytes.toArray
        val target = Hash(arr.slice(offset, offset + Hash.Size))
        val key = Ed25519PublicKey(arr.slice(offset + Hash.Size, offset + Hash.Size + Ed25519PublicKey.Size))
        val signature = Ed25519Signature(arr.slice(offset + 64, offset + Size))
        (Culprit(target, key, signature), Size)

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

    given JamEncoder[Fault] with
      def encode(a: Fault): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.target.bytes
        builder += (if a.vote then 1.toByte else 0.toByte)
        builder ++= a.key.bytes
        builder ++= a.signature.bytes
        builder.result()

    given JamDecoder[Fault] with
      def decode(bytes: JamBytes, offset: Int): (Fault, Int) =
        val arr = bytes.toArray
        val target = Hash(arr.slice(offset, offset + Hash.Size))
        val vote = arr(offset + Hash.Size) != 0
        val key = Ed25519PublicKey(arr.slice(offset + 33, offset + 65))
        val signature = Ed25519Signature(arr.slice(offset + 65, offset + Size))
        (Fault(target, vote, key, signature), Size)

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

    given JamEncoder[GuaranteeSignature] with
      def encode(a: GuaranteeSignature): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeU16LE(a.validatorIndex.value)
        builder ++= a.signature.bytes
        builder.result()

    given JamDecoder[GuaranteeSignature] with
      def decode(bytes: JamBytes, offset: Int): (GuaranteeSignature, Int) =
        val arr = bytes.toArray
        val validatorIndex = ValidatorIndex(codec.decodeU16LE(arr, offset))
        val signature = Ed25519Signature(arr.slice(offset + 2, offset + Size))
        (GuaranteeSignature(validatorIndex, signature), Size)

    given Decoder[GuaranteeSignature] = Decoder.instance { cursor =>
      for
        validatorIndex <- cursor.get[Int]("validator_index")
        signature <- cursor.get[String]("signature")
      yield GuaranteeSignature(ValidatorIndex(validatorIndex), Ed25519Signature(parseHex(signature)))
    }
