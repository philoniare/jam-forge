package io.forge.jam.core.types

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey}
import io.forge.jam.core.json.JsonHelpers.parseHexBytesFixed
import io.circe.Decoder
import _root_.scodec.*
import _root_.scodec.bits.*
import _root_.scodec.codecs.*

/**
 * Epoch-related types
 */
object epoch:

  // Private codecs for primitive types
  private val hashCodec: Codec[Hash] = fixedSizeBytes(32L, bytes).xmap(
    bv => Hash.fromByteVectorUnsafe(bv),
    h => h.toByteVector
  )

  private val bandersnatchKeyCodec: Codec[BandersnatchPublicKey] =
    fixedSizeBytes(32L, bytes).xmap(
      bv => BandersnatchPublicKey.fromByteVectorUnsafe(bv),
      k => k.toByteVector
    )

  private val ed25519KeyCodec: Codec[Ed25519PublicKey] =
    fixedSizeBytes(32L, bytes).xmap(
      bv => Ed25519PublicKey.fromByteVectorUnsafe(bv),
      k => k.toByteVector
    )

  private val blsKeyCodec: Codec[BlsPublicKey] =
    fixedSizeBytes(144L, bytes).xmap(
      bv => BlsPublicKey.fromByteVectorUnsafe(bv),
      k => k.toByteVector
    )

  /**
   * Short-form validator key used in EpochMark.
   * Contains bandersnatch and ed25519 public keys.
   * Fixed size: 64 bytes (32 + 32)
   */
  final case class EpochValidatorKey(
    bandersnatch: BandersnatchPublicKey,
    ed25519: Ed25519PublicKey
  )

  object EpochValidatorKey:
    val Size: Int = BandersnatchPublicKey.Size + Ed25519PublicKey.Size // 64 bytes

    given Codec[EpochValidatorKey] =
      (bandersnatchKeyCodec :: ed25519KeyCodec).xmap(
        { case (b, e) => EpochValidatorKey(b, e) },
        evk => (evk.bandersnatch, evk.ed25519)
      )

    given Decoder[EpochValidatorKey] =
      Decoder.instance { cursor =>
        for
          bandersnatchHex <- cursor.get[String]("bandersnatch")
          ed25519Hex <- cursor.get[String]("ed25519")
          bandersnatch <- parseHexBytesFixed(bandersnatchHex, BandersnatchPublicKey.Size).map(BandersnatchPublicKey(_))
          ed25519 <- parseHexBytesFixed(ed25519Hex, Ed25519PublicKey.Size).map(Ed25519PublicKey(_))
        yield EpochValidatorKey(bandersnatch, ed25519)
      }

  /**
   * Epoch mark containing entropy values and validator keys.
   * Encoding: entropy (32) + ticketsEntropy (32) + validators (validatorCount * 64)
   */
  final case class EpochMark(
    entropy: Hash,
    ticketsEntropy: Hash,
    validators: List[EpochValidatorKey]
  )

  object EpochMark:
    /** Calculate size based on validator count */
    def size(validatorCount: Int): Int =
      Hash.Size + Hash.Size + validatorCount * EpochValidatorKey.Size

    /**
     * Create a codec that knows the expected validator count.
     * EpochMark has a config-dependent size, so we need the validator count.
     */
    def epochMarkCodec(validatorCount: Int): Codec[EpochMark] =
      (hashCodec :: hashCodec :: vectorOfN(provide(validatorCount), summon[Codec[EpochValidatorKey]]).xmap(_.toList, _.toVector)).xmap(
        { case (e, te, vs) => EpochMark(e, te, vs) },
        em => (em.entropy, em.ticketsEntropy, em.validators)
      )

    /**
     * Convenience method to decode with config.
     */
    def fromBytes(bytes: ByteVector, config: ChainConfig): Attempt[DecodeResult[EpochMark]] =
      epochMarkCodec(config.validatorCount).decode(bytes.bits)

    given Decoder[EpochMark] =
      Decoder.instance { cursor =>
        for
          entropyHex <- cursor.get[String]("entropy")
          ticketsEntropyHex <- cursor.get[String]("tickets_entropy")
          validators <- cursor.get[List[EpochValidatorKey]]("validators")
          entropy <- parseHexBytesFixed(entropyHex, Hash.Size).map(Hash(_))
          ticketsEntropy <- parseHexBytesFixed(ticketsEntropyHex, Hash.Size).map(Hash(_))
        yield EpochMark(entropy, ticketsEntropy, validators)
      }

  /**
   * Full validator key containing all key material.
   * Size: 336 bytes (32 + 32 + 144 + 128)
   */
  final case class ValidatorKey(
    bandersnatch: BandersnatchPublicKey,
    ed25519: Ed25519PublicKey,
    bls: BlsPublicKey,
    metadata: JamBytes
  )

  object ValidatorKey:
    val Size: Int = 336 // 32 + 32 + 144 + 128
    val MetadataSize: Int = 128

    given Codec[ValidatorKey] =
      (bandersnatchKeyCodec :: ed25519KeyCodec :: blsKeyCodec :: fixedSizeBytes(128L, bytes)).xmap(
        { case (b, e, bls, meta) => ValidatorKey(b, e, bls, JamBytes.fromByteVector(meta)) },
        vk => (vk.bandersnatch, vk.ed25519, vk.bls, vk.metadata.toByteVector)
      )

    given Decoder[ValidatorKey] =
      Decoder.instance { cursor =>
        for
          bandersnatchHex <- cursor.get[String]("bandersnatch")
          ed25519Hex <- cursor.get[String]("ed25519")
          blsHex <- cursor.get[String]("bls")
          metadataHex <- cursor.get[String]("metadata")
          bandersnatch <- parseHexBytesFixed(bandersnatchHex, BandersnatchPublicKey.Size).map(BandersnatchPublicKey(_))
          ed25519 <- parseHexBytesFixed(ed25519Hex, Ed25519PublicKey.Size).map(Ed25519PublicKey(_))
          bls <- parseHexBytesFixed(blsHex, BlsPublicKey.Size).map(BlsPublicKey(_))
          metadata <- parseHexBytesFixed(metadataHex, MetadataSize).map(bytes => JamBytes(bytes))
        yield ValidatorKey(bandersnatch, ed25519, bls, metadata)
      }
