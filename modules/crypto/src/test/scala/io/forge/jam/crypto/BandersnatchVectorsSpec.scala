package io.forge.jam.crypto

import io.circe.Decoder
import io.forge.jam.vrfs.BandersnatchWrapper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
 * jam-crypto bandersnatch vectors (suite Bandersnatch-SHA512-ELL2-v1, made by ark-vrf 0.6.1, spec draft 35).
 *
 * We pin ark-vrf 0.2.1 (draft 29), whose hash-to-curve, outputs and ring padding point differ, so only key derivation
 * is checked here. The h, gamma, beta, proof and ring commitment checks come with the draft 35 port.
 */
class BandersnatchVectorsSpec extends AnyFunSuite with Matchers:

  private def vectors(scheme: String): List[Map[String, String]] =
    JamCryptoVectors.load[List[Map[String, String]]](s"bandersnatch/vectors/bandersnatch_sha-512_ell2_$scheme.json")(
      using Decoder.decodeList(using Decoder.decodeMap[String, String])
    )

  for scheme <- Seq("thin", "ring") do
    test(s"$scheme vectors derive pk = sk . G") {
      assume(BandersnatchVrf.isAvailable, "Native library not available")
      val vs = vectors(scheme)
      vs should have size 7
      for v <- vs do
        withClue(v("comment") + ": ") {
          BandersnatchWrapper.publicFromSecret(JamCryptoVectors.hex(v("sk"))).toSeq shouldBe
            JamCryptoVectors.hex(v("pk")).toSeq
        }
    }

  test("ring vectors put the signer at index 3 of an 8-key ring") {
    for v <- vectors("ring") do
      val ring = JamCryptoVectors.hex(v("ring_pks")).grouped(32).map(_.toSeq).toList
      ring should have size 8
      ring(3) shouldBe JamCryptoVectors.hex(v("pk")).toSeq
  }

  test("the ring SRS is the one we ship") {
    val theirs = JamCryptoVectors.file("bandersnatch/srs/zcash-srs-2-11-uncompressed.bin").toPath
    val ours = getClass.getResourceAsStream("/zcash-srs-2-11-uncompressed.bin")
    try ours.readAllBytes().toSeq shouldBe java.nio.file.Files.readAllBytes(theirs).toSeq
    finally ours.close()
  }
