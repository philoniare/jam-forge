package io.forge.jam.crypto

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{BandersnatchPublicKey, Hash}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UByte

/**
 * Tests for the Bandersnatch VRF JNI wrapper.
 */
class BandersnatchVrfTest extends AnyFunSuite with Matchers:

  private def hexToBytes(hex: String): Array[Byte] =
    val cleanHex = hex.stripPrefix("0x")
    cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  private val tinyValidatorKeys: List[BandersnatchPublicKey] = List(
    "0x4fc1cc2146c5463a023a66d1fc72829025ef654f1e1dd5c917c40346c2b3aa06",
    "0x4d5c855aebc996a88e818acd27d995f8e04c5dc49652c349f3f3204b39f1984a",
    "0x60a294af38653b7f0073767a0b9d10d8bf0efa6804782a9ecd17a9f5fd323b5b",
    "0x1e50be41fa601015847ae50ef0946ba5217027653d0ffd7a879c45ca21bab357",
    "0xb9b83ef77c1fda8f4b2909e531e6842bf26d4e6f69ed1405fc8d12d1350e6265",
    "0x7ef0acbb01d1ad5da7a1b576aef6ea9a91b979612ff339970c69fe0004d68e05"
  ).map(hex => BandersnatchPublicKey(hexToBytes(hex)))

  private val expectedGammaZ: String =
    "b07448cf678ab70693c2d98a5859067d0f8cac5cb55095e40443cba0bebf090a13fae3bce396b32b484bde8efa788381853598d7531938d998ced21a4d8b406226261cfe5f0e666265c5935149301b618060b078a08249bca249abd14f94fe5492e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"

  private val testKeys: List[BandersnatchPublicKey] = List(
    "0x60a294af38653b7f0073767a0b9d10d8bf0efa6804782a9ecd17a9f5fd323b5b",
    "0x0000000000000000000000000000000000000000000000000000000000000000",
    "0x4fc1cc2146c5463a023a66d1fc72829025ef654f1e1dd5c917c40346c2b3aa06",
    "0x1ecc3686b60ee3b84b6c7d321d70d5c06e9dac63a4d0a79d731b17c0d04d030d",
    "0x4d5c855aebc996a88e818acd27d995f8e04c5dc49652c349f3f3204b39f1984a",
    "0x1e50be41fa601015847ae50ef0946ba5217027653d0ffd7a879c45ca21bab357"
  ).map(hex => BandersnatchPublicKey(hexToBytes(hex)))

  private val expectedTestGammaZ: String =
    "8d075cb903103f1d854a6216bc7dfff2cda2fbf2c60019e77d522be4428a9460ad150ee3269fdd835b2a24e2a73c010e910ab9a40dd0dae4d3f8beb38ff1766e859156d27c7ea14e253baa2f8cdf18373f6cc3acfb942b1776d9716bc8f2de1892e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"

  test("Native library should be available") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")
    BandersnatchVrf.isAvailable shouldBe true
  }

  test("Generate ring root with tiny validator keys should produce expected commitment") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    val ringSize = 6
    val result = BandersnatchVrf.generateRingRoot(tinyValidatorKeys, ringSize)

    result.isDefined shouldBe true
    result.get.length shouldBe BandersnatchVrf.RingCommitmentSize

    val resultHex = result.get.toArray.map(b => f"${b & 0xff}%02x").mkString
    resultHex shouldBe expectedGammaZ
  }

  test("Generate ring root with a null key in the set should produce expected commitment (enact-epoch-change-with-padding-1)") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    val ringSize = 6
    val result = BandersnatchVrf.generateRingRoot(testKeys, ringSize)

    result.isDefined shouldBe true
    result.get.length shouldBe BandersnatchVrf.RingCommitmentSize

    val resultHex = result.get.toArray.map(b => f"${b & 0xff}%02x").mkString
    resultHex shouldBe expectedTestGammaZ
  }

  test("GP: a resized ring padded to V = 255 commits like the bare ring (fuzz spec, GP 0.8.0 appendix bandersnatch)") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    // Under the fuzz spec gamma_Z is built with ring size V = 255 even when a designate shrank the set to 6.
    // Missing keys become the padding point, and ark-vrf uses the same 512-point PIOP domain for every ring of
    // 6..255 keys, so the commitment must equal the one over the 6 keys alone.
    val padded = tinyValidatorKeys.padTo(255, BandersnatchPublicKey(new Array[Byte](32)))
    val result = BandersnatchVrf.generateRingRoot(padded, 255)

    result.map(_.toArray.map(b => f"${b & 0xff}%02x").mkString) shouldBe Some(expectedGammaZ)
  }

  test("Ring root generation should handle padding for invalid keys") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    // Create a list with some invalid (all zeros) keys
    val mixedKeys = List(
      BandersnatchPublicKey(Array.fill(32)(0.toByte)), // Invalid - all zeros
      BandersnatchPublicKey(hexToBytes("0xf16e5352840afb47e206b5c89f560f2611835855cf2e6ebad1acc9520a72591d")),
      BandersnatchPublicKey(Array.fill(32)(0.toByte)), // Invalid - all zeros
      BandersnatchPublicKey(hexToBytes("0x48e5fcdce10e0b64ec4eebd0d9211c7bac2f27ce54bca6f7776ff6fee86ab3e3")),
      BandersnatchPublicKey(hexToBytes("0x3d5e5a51aab2b048f8686ecd79712a80e3265a114cc73f14bdb2a59233fb66d0")),
      BandersnatchPublicKey(hexToBytes("0x7f6190116d118d643a98878e294ccf62b509e214299931aad8ff9764181a4e33"))
    )

    val ringSize = 6
    val result = BandersnatchVrf.generateRingRoot(mixedKeys, ringSize)

    result.isDefined shouldBe true
    result.get.length shouldBe BandersnatchVrf.RingCommitmentSize
  }

  test("Verify ring proof with valid signature from test vector should succeed") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    val entropyHex = "bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"
    val entropy = Hash(hexToBytes("0x" + entropyHex))

    val gammaZHex = expectedGammaZ
    val gammaZ = JamBytes(hexToBytes("0x" + gammaZHex))

    // First ticket of the publish-tickets-no-mark-1.json extrinsic (ark-vrf 0.6.0)
    val signatureHex =
      "32cf17fc1a644ec4beddef894bb3eaa0de48aaeae7a46f6640c068e8afa65ba4fd4838c3604255e69af4667aa693b89798541a876ddae9d663c23694dafaca6546839ec1262170ffe7a8ec36bddf2c95acc9b06e37df5f7c1b1c0f7b2a3e053fb3c51977b040f8a7b43a9efa3b34338964ac96da94857b55a36ec3a94b73c58cf57404acf36e5ff9aa5807f3e301daf55f8bde2552a0f2c9f48f1d270f513412b48d5e7cef8c930e91c8c612f292560df3f4717622883cf58d40a1ffcda116198e9697c9c98759836886b306ecef13c13db8c1780766d3033019e38a0cc1f6ed48b80034dfc16051afabeb532dc9ad538d6db123d2815dbfa6cbe74866ed9855eecbbab8e2011f84f71a2e360caeac3bcd64c4b46b11ca167e238cf5f0ccfe7fa4672999c5e600dab3ce872923c156ed02ca479ce7e23fbe3aecfbeb064124b33efde1047ea4a3bb26dccad623a576a3811c151635d9d0cc5fa6c6718c70bad9c366b8590e0ee0ac16d2033b2512cf44ebfbf62bf140a4906b1e0d79b8481a168d4777d92820d978e956d0019a75d0afa19a27d08f105e6447e3abca3697273199c42970096952350793e5d700b46e35a28d9ce1b433795d7a20ac8d1258ea68e75eda361f79454040cca38351faa74ae722aff185ab1aabab93f23cd1a7cb1305aa0da4ecbc5d0b6e4e258a6e2162e1019e8268c64d0627d511c4910c427f3439caa1a05d235057cc94110e40f66d2c5b45a944395c5a5ec27789bb2d391502bca3fa7583610fb484eba21071b0bf8dff18c3d389576b647bb5b9bb49b5a4088db0e948737d74a5173d736ea8ed43609b968cc897dca45ed854e93fc182c12aa70ae8980653eea7c0cdfc97d576058b038726efe24f0e733e719d7fceec737673195ed0a1352723be03b44354b3df979cb4655a8f950485f836149b1d83ff5e435f32371a3d0497c2a37cf8fa497c41b6c23716fabfb98105686c90dd2e10b1f625bc197412f09de553f2ee6a4a2b46aacca69abfd73e1eb8d851d34b053d4c8b5570c2aa5bc844775f8148bb07c614e6014b73b65273227fe4647ed8a53d9b87daad403779251bb878508bdda1c343"
    val signature = JamBytes(hexToBytes("0x" + signatureHex))

    val attempt = UByte(2)
    val ringSize = 6

    val result = BandersnatchVrf.verifyRingProof(signature, gammaZ, entropy, attempt, ringSize)

    result.isDefined shouldBe true
    result.get.attempt shouldBe attempt
    result.get.ticketId.length shouldBe 32
  }

  test("Verify ring proof with invalid signature should fail") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    val entropy = Hash(hexToBytes("0xbb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"))
    val gammaZ = JamBytes(hexToBytes("0x" + expectedGammaZ))

    val invalidSignature = JamBytes(Array.fill(784)(0.toByte))

    val attempt = UByte(0)
    val ringSize = 6

    val result = BandersnatchVrf.verifyRingProof(invalidSignature, gammaZ, entropy, attempt, ringSize)

    result.isDefined shouldBe false
  }

  test("Verify ring proof with wrong attempt should fail") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    val entropy = Hash(hexToBytes("0xbb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"))
    val gammaZ = JamBytes(hexToBytes("0x" + expectedGammaZ))

    // Valid signature for attempt 2
    val signatureHex =
      "32cf17fc1a644ec4beddef894bb3eaa0de48aaeae7a46f6640c068e8afa65ba4fd4838c3604255e69af4667aa693b89798541a876ddae9d663c23694dafaca6546839ec1262170ffe7a8ec36bddf2c95acc9b06e37df5f7c1b1c0f7b2a3e053fb3c51977b040f8a7b43a9efa3b34338964ac96da94857b55a36ec3a94b73c58cf57404acf36e5ff9aa5807f3e301daf55f8bde2552a0f2c9f48f1d270f513412b48d5e7cef8c930e91c8c612f292560df3f4717622883cf58d40a1ffcda116198e9697c9c98759836886b306ecef13c13db8c1780766d3033019e38a0cc1f6ed48b80034dfc16051afabeb532dc9ad538d6db123d2815dbfa6cbe74866ed9855eecbbab8e2011f84f71a2e360caeac3bcd64c4b46b11ca167e238cf5f0ccfe7fa4672999c5e600dab3ce872923c156ed02ca479ce7e23fbe3aecfbeb064124b33efde1047ea4a3bb26dccad623a576a3811c151635d9d0cc5fa6c6718c70bad9c366b8590e0ee0ac16d2033b2512cf44ebfbf62bf140a4906b1e0d79b8481a168d4777d92820d978e956d0019a75d0afa19a27d08f105e6447e3abca3697273199c42970096952350793e5d700b46e35a28d9ce1b433795d7a20ac8d1258ea68e75eda361f79454040cca38351faa74ae722aff185ab1aabab93f23cd1a7cb1305aa0da4ecbc5d0b6e4e258a6e2162e1019e8268c64d0627d511c4910c427f3439caa1a05d235057cc94110e40f66d2c5b45a944395c5a5ec27789bb2d391502bca3fa7583610fb484eba21071b0bf8dff18c3d389576b647bb5b9bb49b5a4088db0e948737d74a5173d736ea8ed43609b968cc897dca45ed854e93fc182c12aa70ae8980653eea7c0cdfc97d576058b038726efe24f0e733e719d7fceec737673195ed0a1352723be03b44354b3df979cb4655a8f950485f836149b1d83ff5e435f32371a3d0497c2a37cf8fa497c41b6c23716fabfb98105686c90dd2e10b1f625bc197412f09de553f2ee6a4a2b46aacca69abfd73e1eb8d851d34b053d4c8b5570c2aa5bc844775f8148bb07c614e6014b73b65273227fe4647ed8a53d9b87daad403779251bb878508bdda1c343"
    val signature = JamBytes(hexToBytes("0x" + signatureHex))

    // Try to verify with wrong attempt value (signature was created for attempt 2)
    val wrongAttempt = UByte(0)
    val ringSize = 6

    val result = BandersnatchVrf.verifyRingProof(signature, gammaZ, entropy, wrongAttempt, ringSize)

    // Verification should fail because attempt doesn't match
    result.isDefined shouldBe false
  }

  test("Malformed input to guarded exports returns None and does not abort the JVM") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    // Garbage of an unexpected length into the Thin VRF verify export.
    val garbagePublic = Array.fill(32)(0xff.toByte)
    val garbageInput = Array.fill(48)(0x7a.toByte)
    val garbageAux = Array.emptyByteArray
    val garbageSig = Array.fill(96)(0xab.toByte)
    BandersnatchVrf.thinVrfVerify(garbagePublic, garbageInput, garbageAux, garbageSig) shouldBe None

    // Too-short signature into the getVrfOutput export.
    BandersnatchVrf.getVrfOutput(Array.fill(10)(0x01.toByte)) shouldBe None

    // Malformed ring signature (all zeros, wrong content) into the ring verify export.
    val entropy = Hash(hexToBytes("0xbb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"))
    val gammaZ = JamBytes(hexToBytes("0x" + expectedGammaZ))
    val malformedRingSig = JamBytes(Array.fill(784)(0xcd.toByte))
    BandersnatchVrf.verifyRingProof(malformedRingSig, gammaZ, entropy, UByte(0), 6) shouldBe None

    // The JVM is still alive: a subsequent valid operation succeeds.
    val stillAlive = BandersnatchVrf.generateRingRoot(tinyValidatorKeys, 6)
    stillAlive.isDefined shouldBe true
  }
