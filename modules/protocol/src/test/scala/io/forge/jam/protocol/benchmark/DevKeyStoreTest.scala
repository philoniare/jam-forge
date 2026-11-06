package io.forge.jam.protocol.benchmark

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DevKeyStoreTest extends AnyFunSpec with Matchers:

  describe("DevKeyStore"):
    it("should derive keys matching genesis validators"):
      val validators = DevKeyStore.getAllTinyValidators()

      // Expected public keys of the trivial validators (first 8 bytes hex), as in the jam-test-vectors STF
      // vectors. The Bandersnatch keys are the ark-vrf 0.6.0 ones (VRF spec draft 35).
      val expectedBs = List(
        "4fc1cc2146c5463a", "4d5c855aebc996a8", "60a294af38653b7f",
        "1e50be41fa601015", "b9b83ef77c1fda8f", "7ef0acbb01d1ad5d"
      )
      val expectedEd = List(
        "4418fb8c85bb3985", "ad93247bd0130755", "cab2b9ff25c2410f",
        "f30aa5444688b3ca", "8b8c5d436f92ecf6", "ab0084d01534b31c"
      )

      for i <- 0 until 6 do
        val bsHex = validators(i).bandersnatchPublic.take(8).map("%02x".format(_)).mkString
        val edHex = validators(i).ed25519Public.take(8).map("%02x".format(_)).mkString
        withClue(s"Validator $i Bandersnatch:") {
          bsHex shouldBe expectedBs(i)
        }
        withClue(s"Validator $i Ed25519:") {
          edHex shouldBe expectedEd(i)
        }

    it("should produce valid signing keys"):
      val v0 = DevKeyStore.getValidator(0)

      // Test that we can sign and verify with Ed25519
      val message = "test message".getBytes
      val signature = io.forge.jam.crypto.Ed25519ZebraWrapper.sign(v0.ed25519Secret, message)
      signature should not be null
      signature.length shouldBe 64

      val verified = io.forge.jam.crypto.Ed25519ZebraWrapper.verify(v0.ed25519Public, message, signature)
      verified shouldBe true

    it("should produce valid Bandersnatch VRF signatures"):
      val v0 = DevKeyStore.getValidator(0)

      val vrfInput = "test vrf input".getBytes
      val auxData = "test aux".getBytes
      val sig = io.forge.jam.vrfs.BandersnatchWrapper.thinVrfSign(v0.bandersnatchSecret, vrfInput, auxData)
      sig should not be null
      sig.length shouldBe 96

      // Verify the signature
      val output = io.forge.jam.vrfs.BandersnatchWrapper.thinVrfVerify(
        v0.bandersnatchPublic, vrfInput, auxData, sig
      )
      output should not be null
      output.length shouldBe 32
