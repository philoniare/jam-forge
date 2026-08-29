package io.forge.jam.crypto

import io.forge.jam.core.Hashing
import io.forge.jam.core.primitives.{BandersnatchPublicKey, Hash}
import io.forge.jam.vrfs.BandersnatchWrapper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UByte

/** Ring-VRF prover roundtrip: tickets we generate must verify against the
  * ring commitment (gamma_z) exactly as the Safrole STF verifies incoming
  * ticket envelopes.
  */
class RingProverTest extends AnyFunSuite with Matchers:

  private def available: Boolean =
    try
      BandersnatchWrapper.ensureLibraryLoaded()
      true
    catch case _: Throwable => false

  private def devSecret(index: Int): Array[Byte] =
    val le = Array(
      (index & 0xff).toByte,
      ((index >> 8) & 0xff).toByte,
      ((index >> 16) & 0xff).toByte,
      ((index >> 24) & 0xff).toByte
    )
    val seed = Array.fill(8)(le).flatten
    val bsSeed = Hashing.blake2b256("jam_val_key_bandersnatch".getBytes("UTF-8") ++ seed).bytes
    BandersnatchWrapper.secretFromSeed(bsSeed)

  test("ring proof roundtrip: sign as ring member, verify against the commitment") {
    assume(available)
    val ringSize = 6
    val secrets = (0 until ringSize).map(devSecret)
    val publics = secrets.map(s => BandersnatchWrapper.publicFromSecret(s))
    val ringKeys = publics.map(p => BandersnatchPublicKey(p)).toList

    val commitment = BandersnatchVrf
      .generateRingRoot(ringKeys, ringSize)
      .getOrElse(fail("ring root generation failed"))

    val entropy = Hash(Array.tabulate[Byte](32)(i => (i * 7).toByte))
    val attempt = UByte(1)

    for prover <- List(0, 3, 5) do
      val proof = BandersnatchVrf
        .createRingProof(secrets(prover), ringKeys, prover, entropy, attempt)
        .getOrElse(fail(s"ring proof failed for member $prover"))

      val verified = BandersnatchVrf.verifyRingProof(
        proof,
        commitment,
        entropy,
        attempt,
        ringSize
      )
      withClue(s"member $prover:") {
        verified.isDefined shouldBe true
        verified.get.attempt shouldBe attempt
        verified.get.ticketId.length shouldBe 32
      }

    // Different provers with the same (entropy, attempt) yield distinct ids.
    val id0 = BandersnatchVrf
      .createRingProof(secrets(0), ringKeys, 0, entropy, attempt)
      .flatMap(p => BandersnatchVrf.verifyRingProof(p, commitment, entropy, attempt, ringSize))
      .get
      .ticketId
    val id5 = BandersnatchVrf
      .createRingProof(secrets(5), ringKeys, 5, entropy, attempt)
      .flatMap(p => BandersnatchVrf.verifyRingProof(p, commitment, entropy, attempt, ringSize))
      .get
      .ticketId
    id0 should not be id5
  }

  test("a proof does not verify under different entropy or attempt") {
    assume(available)
    val ringSize = 6
    val secrets = (0 until ringSize).map(devSecret)
    val ringKeys = secrets.map(s => BandersnatchPublicKey(BandersnatchWrapper.publicFromSecret(s))).toList
    val commitment = BandersnatchVrf.generateRingRoot(ringKeys, ringSize).get

    val entropy = Hash(Array.fill[Byte](32)(9))
    val proof = BandersnatchVrf
      .createRingProof(secrets(2), ringKeys, 2, entropy, UByte(0))
      .get

    val otherEntropy = Hash(Array.fill[Byte](32)(10))
    BandersnatchVrf.verifyRingProof(proof, commitment, otherEntropy, UByte(0), ringSize) shouldBe None
    BandersnatchVrf.verifyRingProof(proof, commitment, entropy, UByte(1), ringSize) shouldBe None
  }
