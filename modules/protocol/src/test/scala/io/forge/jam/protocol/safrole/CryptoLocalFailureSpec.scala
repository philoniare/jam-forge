package io.forge.jam.protocol.safrole

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{BandersnatchPublicKey, Hash}
import io.forge.jam.crypto.BandersnatchVrf
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UByte

class CryptoLocalFailureSpec extends AnyFunSuite with Matchers:

  private val unsupportedRingSize = 100000

  test("a ring proof check whose ring context cannot be set up throws, and does not read as a failed proof") {
    assume(BandersnatchVrf.isAvailable, "native Bandersnatch library not available")
    val signature = JamBytes(new Array[Byte](BandersnatchVrf.RingVrfSignatureSize))
    val commitment = JamBytes(new Array[Byte](BandersnatchVrf.RingCommitmentSize))
    val entropy = Hash(new Array[Byte](32))
    a[RuntimeException] should be thrownBy
      BandersnatchVrf.verifyRingProof(signature, commitment, entropy, UByte(0), unsupportedRingSize)
    a[RuntimeException] should be thrownBy
      BandersnatchVrf.verifyRingProofs(Seq((signature, UByte(0)), (signature, UByte(1))), commitment, entropy, unsupportedRingSize)
    a[RuntimeException] should be thrownBy
      BandersnatchVrf.generateRingRoot(List.fill(3)(BandersnatchPublicKey(new Array[Byte](32))), unsupportedRingSize)
  }

  test("a ring proof that the native check refuses still reads as not verifying") {
    assume(BandersnatchVrf.isAvailable, "native Bandersnatch library not available")
    val ringSize = 6
    val signature = JamBytes(new Array[Byte](BandersnatchVrf.RingVrfSignatureSize))
    val commitment = BandersnatchVrf
      .generateRingRoot(List.fill(ringSize)(BandersnatchPublicKey(new Array[Byte](32))), ringSize)
      .getOrElse(fail("ring root"))
    BandersnatchVrf.verifyRingProof(signature, commitment, Hash(new Array[Byte](32)), UByte(0), ringSize) shouldBe None
    BandersnatchVrf.ietfVrfVerify(new Array[Byte](32), Array[Byte](1), Array.emptyByteArray, new Array[Byte](96)) shouldBe None
  }
