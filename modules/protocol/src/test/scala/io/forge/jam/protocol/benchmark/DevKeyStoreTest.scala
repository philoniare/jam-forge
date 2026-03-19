package io.forge.jam.protocol.benchmark

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DevKeyStoreTest extends AnyFunSpec with Matchers:

  describe("DevKeyStore"):
    it("should derive keys matching genesis validators"):
      val validators = DevKeyStore.getAllTinyValidators()

      // Expected Bandersnatch public keys from genesis (first 8 bytes hex)
      val expectedBs = List(
        "ff71c6c03ff88adb", "dee6d555b82024f1", "9326edb21e554171",
        "0746846d17469fb2", "151e5c8fe2b9d8a6", "2105650944fcd101"
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
      verified(0) shouldBe 1.toByte

    it("should produce valid Bandersnatch VRF signatures"):
      val v0 = DevKeyStore.getValidator(0)

      val vrfInput = "test vrf input".getBytes
      val auxData = "test aux".getBytes
      val sig = io.forge.jam.vrfs.BandersnatchWrapper.ietfVrfSign(v0.bandersnatchSecret, vrfInput, auxData)
      sig should not be null
      sig.length shouldBe 96

      // Verify the signature
      val output = io.forge.jam.vrfs.BandersnatchWrapper.ietfVrfVerify(
        v0.bandersnatchPublic, vrfInput, auxData, sig
      )
      output should not be null
      output.length shouldBe 32
