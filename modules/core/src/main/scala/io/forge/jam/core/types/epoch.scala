package io.forge.jam.core.types

import io.forge.jam.core.{ChainConfig, JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode}
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey}

/**
 * Epoch-related types
 */
object epoch:

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

    given JamEncoder[EpochValidatorKey] with
      def encode(a: EpochValidatorKey): JamBytes =
        JamBytes(a.bandersnatch.bytes ++ a.ed25519.bytes)

    given JamDecoder[EpochValidatorKey] with
      def decode(bytes: JamBytes, offset: Int): (EpochValidatorKey, Int) =
        val arr = bytes.toArray
        val bandersnatch = BandersnatchPublicKey(arr.slice(offset, offset + BandersnatchPublicKey.Size))
        val ed25519 = Ed25519PublicKey(arr.slice(offset + BandersnatchPublicKey.Size, offset + Size))
        (EpochValidatorKey(bandersnatch, ed25519), Size)

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

    given JamEncoder[EpochMark] with
      def encode(a: EpochMark): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.entropy.bytes
        builder ++= a.ticketsEntropy.bytes
        for validator <- a.validators do
          builder ++= validator.encode
        builder.result()

    /**
     * Create a decoder that knows the expected validator count.
     * EpochMark has a config-dependent size, so we need the validator count.
     */
    def decoder(validatorCount: Int): JamDecoder[EpochMark] = new JamDecoder[EpochMark]:
      def decode(bytes: JamBytes, offset: Int): (EpochMark, Int) =
        val arr = bytes.toArray
        val entropy = Hash(arr.slice(offset, offset + Hash.Size))
        val ticketsEntropy = Hash(arr.slice(offset + Hash.Size, offset + Hash.Size * 2))

        var pos = offset + Hash.Size * 2
        val validators = (0 until validatorCount).map { _ =>
          val (validator, consumed) = EpochValidatorKey.given_JamDecoder_EpochValidatorKey.decode(bytes, pos)
          pos += consumed
          validator
        }.toList

        val totalSize = Hash.Size * 2 + validatorCount * EpochValidatorKey.Size
        (EpochMark(entropy, ticketsEntropy, validators), totalSize)

    /**
     * Convenience method to decode with config.
     */
    def fromBytes(bytes: JamBytes, offset: Int, config: ChainConfig): (EpochMark, Int) =
      decoder(config.validatorCount).decode(bytes, offset)

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

    given JamEncoder[ValidatorKey] with
      def encode(a: ValidatorKey): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.bandersnatch.bytes
        builder ++= a.ed25519.bytes
        builder ++= a.bls.bytes
        builder ++= a.metadata
        builder.result()

    given JamDecoder[ValidatorKey] with
      def decode(bytes: JamBytes, offset: Int): (ValidatorKey, Int) =
        val arr = bytes.toArray
        var pos = offset
        val bandersnatch = BandersnatchPublicKey(arr.slice(pos, pos + BandersnatchPublicKey.Size))
        pos += BandersnatchPublicKey.Size
        val ed25519 = Ed25519PublicKey(arr.slice(pos, pos + Ed25519PublicKey.Size))
        pos += Ed25519PublicKey.Size
        val bls = BlsPublicKey(arr.slice(pos, pos + BlsPublicKey.Size))
        pos += BlsPublicKey.Size
        val metadata = JamBytes(arr.slice(pos, pos + MetadataSize))
        (ValidatorKey(bandersnatch, ed25519, bls, metadata), Size)
