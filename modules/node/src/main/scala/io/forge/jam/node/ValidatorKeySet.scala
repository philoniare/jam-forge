package io.forge.jam.node

import io.forge.jam.core.Hashing
import io.forge.jam.core.primitives.{BandersnatchPublicKey, Ed25519PublicKey}
import io.forge.jam.crypto.{Bls, Ed25519ZebraWrapper}
import io.forge.jam.vrfs.BandersnatchWrapper

/** A validator's signing keys: Bandersnatch (sealing/VRF), Ed25519
  * (guarantees, assurances, judgments, network identity) and BLS (BEEFY).
  */
final case class ValidatorKeySet(
    bandersnatchSecret: Array[Byte],
    bandersnatchPublic: Array[Byte],
    ed25519Secret: Array[Byte],
    ed25519Public: Array[Byte],
    blsSecret: Array[Byte] = Array.emptyByteArray,
    blsPublic: Array[Byte] = Array.emptyByteArray,
    bandersnatchHeld: String = ""
):
  /** The public keys as content-compared values, for map keys and set membership. */
  lazy val ed25519Key: Ed25519PublicKey = Ed25519PublicKey(ed25519Public)
  lazy val bandersnatchKey: BandersnatchPublicKey = BandersnatchPublicKey(bandersnatchPublic)

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
    * blake2b("jam_val_key_<algo>" ++ seed). JIP-5 names no BLS seed; we
    * assume "jam_val_key_bls" by the same pattern (BEEFY prototype).
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

    val blsSeed =
      Hashing.blake2b256("jam_val_key_bls".getBytes("UTF-8") ++ seed).bytes

    ValidatorKeySet(bsSecret, bsPublic, ed25519Seed, edPublic, blsSeed, Bls.publicFromSeed(blsSeed))

  final case class Secret(bytes: Array[Byte], from: String)

  final case class Resolved(keys: ValidatorKeySet, matched: Boolean, missing: Boolean, notes: List[String])

  private final case class Reading(secret: Array[Byte], public: BandersnatchPublicKey, from: String, as: String):
    def source: String = if as.isEmpty then from else s"$from read as $as"

  private def reading(from: String, as: String)(secret: => Array[Byte]): Option[Reading] =
    scala.util.Try(Reading(secret, BandersnatchPublicKey(BandersnatchWrapper.publicFromSecret(secret)), from, as)).toOption

  private def ofSeedFile(s: Secret): List[Reading] =
    List(
      reading(s.from, "")(BandersnatchWrapper.secretFromSeed(bandersnatchSeedOf(s.bytes))),
      reading(s.from, "a Bandersnatch secret seed")(BandersnatchWrapper.secretFromSeed(s.bytes)),
      reading(s.from, "a Bandersnatch secret scalar")(s.bytes.clone())
    ).flatten

  private def ofOption(s: Secret): List[Reading] =
    List(
      reading(s.from, "")(BandersnatchWrapper.secretFromSeed(s.bytes)),
      reading(s.from, "a seed file's contents")(BandersnatchWrapper.secretFromSeed(bandersnatchSeedOf(s.bytes))),
      reading(s.from, "a Bandersnatch secret scalar")(s.bytes.clone())
    ).flatten

  private def bandersnatchSeedOf(seed: Array[Byte]): Array[Byte] =
    Hashing.blake2b256("jam_val_key_bandersnatch".getBytes("UTF-8") ++ seed).bytes

  private def hex(key: BandersnatchPublicKey): String = key.bytes.toArray.map(b => f"$b%02x").mkString

  def resolveBandersnatch(
      keys: ValidatorKeySet,
      onChain: Seq[(Ed25519PublicKey, BandersnatchPublicKey)],
      seeds: Seq[Secret],
      option: Option[Secret]
  ): Resolved =
    BandersnatchWrapper.ensureLibraryLoaded()
    val wanted = onChain.collect { case (ed, bs) if ed == keys.ed25519Key => bs }.distinct
    val fromSeeds = seeds.toList.map(ofSeedFile)
    val fromOption = option.toList.flatMap(ofOption)
    val ownFrom = fromSeeds.flatMap(_.headOption).find(_.public == keys.bandersnatchKey).fold("the validator's seed")(_.from)
    val own = Reading(keys.bandersnatchSecret, keys.bandersnatchKey, ownFrom, "")
    val asked = fromOption.headOption.getOrElse(own)
    val has = (own :: fromOption.take(1) ::: fromSeeds.flatMap(_.headOption)).distinctBy(_.public)
    val held = has.map(r => s"${hex(r.public).take(8)} ${r.from}").mkString(", ")
    def use(r: Reading) = keys.copy(bandersnatchSecret = r.secret, bandersnatchPublic = r.public.bytes.toArray, bandersnatchHeld = held)

    (own :: fromOption ::: fromSeeds.flatten).find(r => wanted.contains(r.public)) match
      case Some(found) =>
        val where =
          if found eq own then Nil
          else List(s"the validator's on-chain Bandersnatch key ${hex(found.public)} comes from ${found.source}")
        val ignored = fromOption.headOption.filter(_.public != found.public).map(o =>
          s"${o.from} gives Bandersnatch key ${hex(o.public)}, not the validator's on-chain one: ignoring it"
        )
        Resolved(use(found), matched = true, missing = false, where ++ ignored)
      case None if wanted.nonEmpty =>
        Resolved(
          use(asked),
          matched = false,
          missing = true,
          List(
            s"no secret for the validator's on-chain Bandersnatch key ${wanted.map(hex).mkString(" / ")}: the node has " +
              has.map(r => s"${hex(r.public)} (${r.from})").mkString(", ") +
              ". It will make no tickets and author no blocks. Put the seed file that gives that key in the keys " +
              "directory (list-keys prints each file's key)"
          )
        )
      case None =>
        Resolved(
          use(asked),
          matched = false,
          missing = false,
          List(s"the chain's validator sets do not hold this validator: using Bandersnatch key ${hex(asked.public)} of ${asked.from}")
        )

  /** `keys` with its Bandersnatch key replaced by the one from `secretSeed`: the 32-byte Bandersnatch secret seed
    * (JIP-5 `bandersnatch_secret_seed`, the input of ark-vrf `Secret::from_seed`).
    */
  def withBandersnatchSeed(keys: ValidatorKeySet, secretSeed: Array[Byte]): ValidatorKeySet =
    require(secretSeed.length == 32, "Bandersnatch secret seed must be 32 bytes")
    BandersnatchWrapper.ensureLibraryLoaded()
    val secret = BandersnatchWrapper.secretFromSeed(secretSeed)
    keys.copy(bandersnatchSecret = secret, bandersnatchPublic = BandersnatchWrapper.publicFromSecret(secret))

  /** `keys` with its Ed25519 key replaced by the one from `secretSeed`: the 32-byte Ed25519 secret key (JIP-5
    * `ed25519_secret_seed`, the RFC 8032 private key).
    */
  def withEd25519Seed(keys: ValidatorKeySet, secretSeed: Array[Byte]): ValidatorKeySet =
    require(secretSeed.length == 32, "Ed25519 secret key must be 32 bytes")
    Ed25519ZebraWrapper.ensureLibraryLoaded()
    keys.copy(ed25519Secret = secretSeed.clone(), ed25519Public = Ed25519ZebraWrapper.publicFromSecret(secretSeed))

  /** Dev validator `index` (JIP-5 trivial seed). */
  def dev(index: Int): ValidatorKeySet = fromSeed(trivialSeed(index))
