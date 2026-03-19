package io.forge.jam.protocol.benchmark

import io.forge.jam.core.Hashing
import io.forge.jam.crypto.Ed25519ZebraWrapper
import io.forge.jam.vrfs.BandersnatchWrapper

/** Deterministic validator key derivation using JIP-5 trivial seeds. */
object DevKeyStore:

  case class ValidatorKeys(
    bandersnatchSecret: Array[Byte],
    bandersnatchPublic: Array[Byte],
    ed25519Secret: Array[Byte],
    ed25519Public: Array[Byte]
  )

  def trivialSeed(index: Int): Array[Byte] =
    val le = Array(
      (index & 0xff).toByte,
      ((index >> 8) & 0xff).toByte,
      ((index >> 16) & 0xff).toByte,
      ((index >> 24) & 0xff).toByte
    )
    Array.fill(8)(le).flatten

  def deriveKeys(seed: Array[Byte]): ValidatorKeys =
    BandersnatchWrapper.ensureLibraryLoaded()
    Ed25519ZebraWrapper.ensureLibraryLoaded()

    val ed25519Seed = Hashing.blake2b256("jam_val_key_ed25519".getBytes("UTF-8") ++ seed).bytes
    val bandersnatchSeed = Hashing.blake2b256("jam_val_key_bandersnatch".getBytes("UTF-8") ++ seed).bytes

    val bsSecret = BandersnatchWrapper.secretFromSeed(bandersnatchSeed)
    val bsPublic = BandersnatchWrapper.publicFromSecret(bsSecret)
    val edPublic = Ed25519ZebraWrapper.publicFromSecret(ed25519Seed)

    ValidatorKeys(bsSecret, bsPublic, ed25519Seed, edPublic)

  def getValidator(index: Int): ValidatorKeys =
    deriveKeys(trivialSeed(index))

  def getAllTinyValidators(): Array[ValidatorKeys] =
    (0 until 6).map(getValidator).toArray
