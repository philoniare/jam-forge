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
    "0xff71c6c03ff88adb5ed52c9681de1629a54e702fc14729f6b50d2f0a76f185b3",
    "0xdee6d555b82024f1ccf8a1e37e60fa60fd40b1958c4bb3006af78647950e1b91",
    "0x9326edb21e5541717fde24ec085000b28709847b8aab1ac51f84e94b37ca1b66",
    "0x0746846d17469fb2f95ef365efcab9f4e22fa1feb53111c995376be8019981cc",
    "0x151e5c8fe2b9d8a606966a79edd2f9e5db47e83947ce368ccba53bf6ba20a40b",
    "0x2105650944fcd101621fd5bb3124c9fd191d114b7ad936c1d79d734f9f21392e"
  ).map(hex => BandersnatchPublicKey(hexToBytes(hex)))

  private val expectedGammaZ: String =
    "af39b7de5fcfb9fb8a46b1645310529ce7d08af7301d9758249da4724ec698eb127f489b58e49ae9ab85027509116962a135fc4d97b66fbbed1d3df88cd7bf5cc6e5d7391d261a4b552246648defcb64ad440d61d69ec61b5473506a48d58e1992e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"

  private val testKeys: List[BandersnatchPublicKey] = List(
    "0xaa2b95f7572875b0d0f186552ae745ba8222fc0b5bd456554bfe51c68938f8bc",
    "0xf16e5352840afb47e206b5c89f560f2611835855cf2e6ebad1acc9520a72591d",
    "0x5e465beb01dbafe160ce8216047f2155dd0569f058afd52dcea601025a8d161d",
    "0x48e5fcdce10e0b64ec4eebd0d9211c7bac2f27ce54bca6f7776ff6fee86ab3e3",
    "0x3d5e5a51aab2b048f8686ecd79712a80e3265a114cc73f14bdb2a59233fb66d0",
    "0x7f6190116d118d643a98878e294ccf62b509e214299931aad8ff9764181a4e33"
  ).map(hex => BandersnatchPublicKey(hexToBytes(hex)))

  private val expectedTestGammaZ: String =
    "8387a131593447e4e1c3d4e220c322e42d33207fa77cd0fedb39fc3491479ca47a2d82295252e278fa3eec78185982ed82ae0c8fd691335e703d663fb5be02b3def15380789320636b2479beab5a03ccb3f0909ffea59d859fcdc7e187e45a8c92e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"

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

  test("Generate ring root with test keys should produce expected commitment") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    val ringSize = 6
    val result = BandersnatchVrf.generateRingRoot(testKeys, ringSize)

    result.isDefined shouldBe true
    result.get.length shouldBe BandersnatchVrf.RingCommitmentSize

    val resultHex = result.get.toArray.map(b => f"${b & 0xff}%02x").mkString
    resultHex shouldBe expectedTestGammaZ
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

    // Valid signature from test vector (first ticket in publish-tickets-no-mark-1.json extrinsic)
    val signatureHex =
      "e5055ade89aba8daa558078c28b6f86468772f63bebe920d88ce3e189405c09660e430f3ea78a6d70aa849570e35de25a35414c1bf5df87f0ebcf495a9481ef289bfe8b2135788aa9687549f8a4a1d492323e817eb7ddd396d9d4d1f95da6b807822b436856a8a5c06f65f0aecb4a077944d0b5b308293c403a28a91c8d4474fe99615eebe49cd57973e101a5bc3982144d236031327aa95f58d16d92364df062a89bb767c63aef3cd2938e7af15cf9e3e60b19afd0d3d78c43ed0af8e9fd0108407aef70f2bba8bf2bd9316eb6c1cc923e133e00096f6ebbb58b124491d7093f271db465926809975343eaa02d30c108d6db123d2815dbfa6cbe74866ed9855eecbbab8e2011f84f71a2e360caeac3bcd64c4b46b11ca167e238cf5f0ccfe7f93ff1cab74815f48db1759417f39f81d8887490789027344561d32285bf92ca503bca1dcf8b013d5938e865ea933221f98910d47cc02f4a9a826c16b5893a6c792c4aee4b20c2df607796db538b5c6538623b88151a00aad17689fa94ee32123f1ec933cb4fdbc00e5452ba7f151b86359c8fed9878216e1fdac29c26a5a700c2994d49afce3daec4839a2ec63f1c36b47bef47014f9433005103d9d1bd5541d409ec0cfbeaf2dd92627a1fb702e9a0b509403f965c3be6a31aeb8ad97cbc725e359d8b8960a3a512af9af95c2712f26a5d4483b69001c956abfdc781148d73db38ee8ce579f5030bb28d98fa84e4c942327b6e58ee6ce1f0f1e71b4e4b7263b347140d7979bd4d1cda8532dc8ba12dbc31f7b154f0bda72fc486adba2737e6621633f98350f73040c9abf9530a09989476e6243cdcf397ac05a0ccb71eb6265af7258d8dc1ac2f66210686e7deeca4099bb7b2240d2dae18bcd739cf9fdcb0536745cdf6b30c3c29312511b8f71a035c2114ea47439261d30a556b66b84d1a4ba2cc64bdf2b19f862a363b9d91ee022ab7ffa61dedd23efb3c4effdbe955638956d6ee3998cf874449763818d23fbdb7297d47f82db65db6ebac8a6d06c0dbeb3163095c13fd27af4e31416c5561c7adc552bf02ad47d1c99757bc66c302eb2a851c706b097869faed86517a22fae96"
    val signature = JamBytes(hexToBytes("0x" + signatureHex))

    val attempt = UByte(0)
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

    // Valid signature for attempt 0
    val signatureHex =
      "e5055ade89aba8daa558078c28b6f86468772f63bebe920d88ce3e189405c09660e430f3ea78a6d70aa849570e35de25a35414c1bf5df87f0ebcf495a9481ef289bfe8b2135788aa9687549f8a4a1d492323e817eb7ddd396d9d4d1f95da6b807822b436856a8a5c06f65f0aecb4a077944d0b5b308293c403a28a91c8d4474fe99615eebe49cd57973e101a5bc3982144d236031327aa95f58d16d92364df062a89bb767c63aef3cd2938e7af15cf9e3e60b19afd0d3d78c43ed0af8e9fd0108407aef70f2bba8bf2bd9316eb6c1cc923e133e00096f6ebbb58b124491d7093f271db465926809975343eaa02d30c108d6db123d2815dbfa6cbe74866ed9855eecbbab8e2011f84f71a2e360caeac3bcd64c4b46b11ca167e238cf5f0ccfe7f93ff1cab74815f48db1759417f39f81d8887490789027344561d32285bf92ca503bca1dcf8b013d5938e865ea933221f98910d47cc02f4a9a826c16b5893a6c792c4aee4b20c2df607796db538b5c6538623b88151a00aad17689fa94ee32123f1ec933cb4fdbc00e5452ba7f151b86359c8fed9878216e1fdac29c26a5a700c2994d49afce3daec4839a2ec63f1c36b47bef47014f9433005103d9d1bd5541d409ec0cfbeaf2dd92627a1fb702e9a0b509403f965c3be6a31aeb8ad97cbc725e359d8b8960a3a512af9af95c2712f26a5d4483b69001c956abfdc781148d73db38ee8ce579f5030bb28d98fa84e4c942327b6e58ee6ce1f0f1e71b4e4b7263b347140d7979bd4d1cda8532dc8ba12dbc31f7b154f0bda72fc486adba2737e6621633f98350f73040c9abf9530a09989476e6243cdcf397ac05a0ccb71eb6265af7258d8dc1ac2f66210686e7deeca4099bb7b2240d2dae18bcd739cf9fdcb0536745cdf6b30c3c29312511b8f71a035c2114ea47439261d30a556b66b84d1a4ba2cc64bdf2b19f862a363b9d91ee022ab7ffa61dedd23efb3c4effdbe955638956d6ee3998cf874449763818d23fbdb7297d47f82db65db6ebac8a6d06c0dbeb3163095c13fd27af4e31416c5561c7adc552bf02ad47d1c99757bc66c302eb2a851c706b097869faed86517a22fae96"
    val signature = JamBytes(hexToBytes("0x" + signatureHex))

    // Try to verify with wrong attempt value (signature was created for attempt 0)
    val wrongAttempt = UByte(1)
    val ringSize = 6

    val result = BandersnatchVrf.verifyRingProof(signature, gammaZ, entropy, wrongAttempt, ringSize)

    // Verification should fail because attempt doesn't match
    result.isDefined shouldBe false
  }

  test("Malformed input to guarded exports returns None and does not abort the JVM") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    // Garbage of an unexpected length into the IETF verify export.
    val garbagePublic = Array.fill(32)(0xff.toByte)
    val garbageInput = Array.fill(48)(0x7a.toByte)
    val garbageAux = Array.emptyByteArray
    val garbageSig = Array.fill(96)(0xab.toByte)
    BandersnatchVrf.ietfVrfVerify(garbagePublic, garbageInput, garbageAux, garbageSig) shouldBe None

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
