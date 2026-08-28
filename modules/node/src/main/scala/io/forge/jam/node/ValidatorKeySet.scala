package io.forge.jam.node

import io.forge.jam.core.Hashing
import io.forge.jam.crypto.Ed25519ZebraWrapper
import io.forge.jam.vrfs.BandersnatchWrapper

/** A validator's signing keys: Bandersnatch (sealing/VRF) and Ed25519
  * (guarantees, assurances, judgments, network identity).
  */
final case class ValidatorKeySet(
    bandersnatchSecret: Array[Byte],
    bandersnatchPublic: Array[Byte],
    ed25519Secret: Array[Byte],
    ed25519Public: Array[Byte]
)

object ValidatorKeySet:

  /** JIP-5 trivial seed for dev validator `index`: the 4-byte LE index
    * repeated 8 times.
    */
  def trivialSeed(index: Int): Array[Byte] =
    val le = Array(
      (index & 0xff).toByte,
      ((index >> 8) & 0xff).toByte,
      ((index >> 16) & 0xff).toByte,
      ((index >> 24) & 0xff).toByte
    )
    Array.fill(8)(le).flatten

  /** JIP-5 key derivation from a 32-byte seed: per-algorithm seeds are
    * blake2b("jam_val_key_<algo>" ++ seed).
    */
  def fromSeed(seed: Array[Byte]): ValidatorKeySet =
    require(seed.length == 32, "validator seed must be 32 bytes")
    BandersnatchWrapper.ensureLibraryLoaded()
    Ed25519ZebraWrapper.ensureLibraryLoaded()

    val ed25519Seed =
      Hashing.blake2b256("jam_val_key_ed25519".getBytes("UTF-8") ++ seed).bytes
    val bandersnatchSeed =
      Hashing.blake2b256("jam_val_key_bandersnatch".getBytes("UTF-8") ++ seed).bytes

    val bsSecret = BandersnatchWrapper.secretFromSeed(bandersnatchSeed)
    val bsPublic = BandersnatchWrapper.publicFromSecret(bsSecret)
    val edPublic = Ed25519ZebraWrapper.publicFromSecret(ed25519Seed)

    ValidatorKeySet(bsSecret, bsPublic, ed25519Seed, edPublic)

  /** Dev validator `index` (JIP-5 trivial seed). */
  def dev(index: Int): ValidatorKeySet = fromSeed(trivialSeed(index))
