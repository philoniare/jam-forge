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

  test("a ring call for a ring size never initialised fails on its own and leaves ring checks working") {
    assume(available)
    val ringSize = 6
    val secrets = (0 until ringSize).map(devSecret)
    val ringKeys = secrets.map(s => BandersnatchPublicKey(BandersnatchWrapper.publicFromSecret(s))).toList
    val commitment = BandersnatchVrf.generateRingRoot(ringKeys, ringSize).get
    val entropy = Hash(Array.fill[Byte](32)(3))
    val proof = BandersnatchVrf.createRingProof(secrets(1), ringKeys, 1, entropy, UByte(200)).get

    // Size 11 was never set up: each call fails with an exception instead of panicking under the context lock.
    val uninitialised = 11
    an[RuntimeException] should be thrownBy
      BandersnatchWrapper.getVerifierCommitment(uninitialised, new Array[Byte](uninitialised * 32))
    an[RuntimeException] should be thrownBy BandersnatchWrapper.verifierRingVrfVerify(
      entropy.bytes,
      200.toByte,
      proof.toArray,
      commitment.toArray,
      uninitialised
    )
    an[RuntimeException] should be thrownBy BandersnatchWrapper.ringVrfSign(
      secrets(1),
      new Array[Byte](uninitialised * 32),
      1,
      uninitialised,
      entropy.bytes,
      0.toByte
    )

    // A poisoned lock would make every one of these fail from here on.
    // (The reversed ring misses the Scala-side ring-root cache, so this reaches the native builder.)
    BandersnatchVrf.generateRingRoot(ringKeys.reverse, ringSize).isDefined shouldBe true
    BandersnatchVrf.verifyRingProof(proof, commitment, entropy, UByte(200), ringSize).isDefined shouldBe true
    BandersnatchVrf.createRingProof(secrets(4), ringKeys, 4, entropy, UByte(7)).isDefined shouldBe true
  }

  test("verifierRingVrfVerify takes the attempt as a byte, as the native side reads it (jbyte)") {
    val m = classOf[BandersnatchWrapper].getMethods.find(_.getName == "verifierRingVrfVerify").get
    m.getParameterTypes()(1) shouldBe java.lang.Byte.TYPE
  }
