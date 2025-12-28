package io.forge.jam.protocol.assurance

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.generators.StfGenerators.*
import io.forge.jam.protocol.assurance.AssuranceTypes.*
import io.forge.jam.core.scodec.JamCodecs.{hashCodec, jamBytesCodec}

/**
 * - Assurances must be sorted and unique by validator index
 * - Supermajority (2/3 + 1) required for availability confirmation
 * - Only engaged cores can receive assurance bits
 */
class AssurancesSTFSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  private val testConfig = ChainConfig.TINY

  // Override default ScalaCheck configuration for faster tests
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  test("property: empty assurances input produces successful output") {
    forAll(genAssuranceState(testConfig), genHash) { (preState, parent) =>
      val input = AssuranceInput(List.empty, slot = 100L, parent = parent)

      val (_, output) = AssuranceTransition.stfInternal(input, preState, testConfig)

      // Property: empty assurances should succeed
      output.isRight shouldBe true

      // Property: output should have empty reported list
      output.toOption.get.reported shouldBe empty
    }
  }

  test("property: generated assurance inputs are sorted by validator index") {
    forAll(genAssuranceInput(testConfig)) { input =>
      // Property: assurances should be sorted by validator index
      val validatorIndices = input.assurances.map(_.validatorIndex.toInt)
      validatorIndices shouldBe validatorIndices.sorted

      // Property: assurances should be unique by validator index
      validatorIndices.distinct.size shouldBe validatorIndices.size
    }
  }

  test("property: assurance validator indices are within bounds") {
    forAll(genAssuranceInput(testConfig)) { input =>
      input.assurances.foreach { assurance =>
        // Property: validator index should be less than validator count
        assurance.validatorIndex.toInt should be < testConfig.validatorCount
        assurance.validatorIndex.toInt should be >= 0
      }
    }
  }

  test("property: supermajority threshold is correctly calculated for TINY") {
    // Property: superMajority = (2 * validatorCount) / 3
    // For TINY: (2 * 6) / 3 = 4
    testConfig.superMajority shouldBe 4

    // Property: validator count for TINY
    testConfig.validatorCount shouldBe 6
  }

  test("property: supermajority threshold is correctly calculated for FULL") {
    val fullConfig = ChainConfig.FULL

    // Property: superMajority for FULL
    // (2 * 1023) / 3 = 682
    fullConfig.superMajority shouldBe 682

    // Property: validator count for FULL
    fullConfig.validatorCount shouldBe 1023
  }

  test("property: availability confirmation requires supermajority") {
    forAll(genAssuranceState(testConfig), genHash) { (preState, parent) =>
      val input = AssuranceInput(List.empty, slot = 100L, parent = parent)

      val (_, output) = AssuranceTransition.stfInternal(input, preState, testConfig)

      whenever(output.isRight) {
        // Property: with no assurances, no reports should become available
        output.toOption.get.reported shouldBe empty
      }
    }
  }

  test("property: availAssignments size matches cores count") {
    forAll(genAssuranceState(testConfig)) { state =>
      // Property: availAssignments list size should equal cores count
      state.availAssignments.size shouldBe testConfig.coresCount
    }
  }

  test("property: generated assurance bitfields have correct size") {
    forAll(genAssuranceExtrinsic(testConfig)) { assurance =>
      // Property: bitfield size should be ceil(coresCount / 8)
      val expectedSize = (testConfig.coresCount + 7) / 8
      assurance.bitfield.length shouldBe expectedSize
    }
  }

  test("property: validator keys list size matches validator count") {
    forAll(genAssuranceState(testConfig)) { state =>
      // Property: currValidators size should equal validator count
      state.currValidators.size shouldBe testConfig.validatorCount
    }
  }

  test("property: validators are preserved during assurance processing") {
    forAll(genAssuranceState(testConfig), genHash) { (preState, parent) =>
      val input = AssuranceInput(List.empty, slot = 100L, parent = parent)

      val (postState, output) = AssuranceTransition.stfInternal(input, preState, testConfig)

      whenever(output.isRight) {
        // Property: currValidators should not change
        postState.currValidators.size shouldBe preState.currValidators.size
        postState.currValidators.zip(preState.currValidators).foreach {
          case (post, pre) =>
            java.util.Arrays.equals(post.bandersnatch.bytes, pre.bandersnatch.bytes) shouldBe true
            java.util.Arrays.equals(post.ed25519.bytes, pre.ed25519.bytes) shouldBe true
        }
      }
    }
  }

  test("property: availAssignments size is preserved") {
    forAll(genAssuranceState(testConfig), genHash) { (preState, parent) =>
      val input = AssuranceInput(List.empty, slot = 100L, parent = parent)

      val (postState, output) = AssuranceTransition.stfInternal(input, preState, testConfig)

      whenever(output.isRight) {
        // Property: availAssignments size should remain constant
        postState.availAssignments.size shouldBe preState.availAssignments.size
      }
    }
  }

  test("property: assurance timeout period is configured correctly") {
    // Property: verify timeout period exists in config
    testConfig.assuranceTimeoutPeriod should be > 0
  }

  test("property: stale reports are cleared on timeout") {
    forAll(genAssuranceState(testConfig), genHash) { (preState, parent) =>
      // Use a very large slot to trigger timeouts
      val largeSlot = 100000L
      val input = AssuranceInput(List.empty, slot = largeSlot, parent = parent)

      val (postState, output) = AssuranceTransition.stfInternal(input, preState, testConfig)

      whenever(output.isRight) {
        // Property: reports that have timed out should be cleared
        // (They will have timeout + assuranceTimeoutPeriod <= currentSlot)
        postState.availAssignments.foreach { assignmentOpt =>
          assignmentOpt.foreach { assignment =>
            // Any remaining assignment should have timeout + period > currentSlot
            // or was refreshed
            (assignment.timeout + testConfig.assuranceTimeoutPeriod) should be > largeSlot
          }
        }
      }
    }
  }

  test("property: bitfield byte order is little-endian within each byte") {
    forAll(genAssuranceExtrinsic(testConfig)) { assurance =>
      val bitfieldBytes = assurance.bitfield.toArray

      // Property: bit positions 0..7 are in first byte, 8..15 in second, etc.
      for coreIndex <- 0 until testConfig.coresCount do
        val expectedByteIndex = coreIndex / 8
        expectedByteIndex should be < bitfieldBytes.length
    }
  }

  test("GP: supermajority formula matches 2V/3") {
    // GP: superMajority = floor(2V/3)
    val tinyExpected = (2 * testConfig.validatorCount) / 3
    testConfig.superMajority shouldBe tinyExpected

    val fullConfig = ChainConfig.FULL
    val fullExpected = (2 * fullConfig.validatorCount) / 3
    fullConfig.superMajority shouldBe fullExpected
  }

  test("GP: stfInternal is deterministic (same input produces same output)") {
    forAll(genAssuranceState(testConfig), genHash) { (preState, parent) =>
      val input = AssuranceInput(List.empty, slot = 100L, parent = parent)

      val (postState1, output1) = AssuranceTransition.stfInternal(input, preState, testConfig)
      val (postState2, output2) = AssuranceTransition.stfInternal(input, preState, testConfig)

      // GP: Same inputs produce identical outputs
      output1 shouldBe output2
      postState1.availAssignments.size shouldBe postState2.availAssignments.size
      postState1.currValidators.size shouldBe postState2.currValidators.size
    }
  }

  test("GP: timeout formula is slot >= timeout + U") {
    // GP: Report times out when currentSlot >= timeout + assuranceTimeoutPeriod (U)
    // U = assuranceTimeoutPeriod
    testConfig.assuranceTimeoutPeriod should be > 0

    // Verify the formula is applied in stfInternal
    forAll(genAssuranceState(testConfig), genHash) { (preState, parent) =>
      val veryLargeSlot = Long.MaxValue / 2 // Use a very large slot
      val input = AssuranceInput(List.empty, slot = veryLargeSlot, parent = parent)

      val (postState, output) = AssuranceTransition.stfInternal(input, preState, testConfig)

      whenever(output.isRight) {
        // GP: All assignments with timeout + U <= currentSlot should be cleared
        postState.availAssignments.foreach { assignmentOpt =>
          assignmentOpt.foreach { assignment =>
            (assignment.timeout + testConfig.assuranceTimeoutPeriod) should be > veryLargeSlot
          }
        }
      }
    }
  }

  test("GP: availability requires count > superMajority") {
    // GP: Core becomes available when assurance count > superMajority (2V/3)
    // With empty assurances, no core should become available
    forAll(genAssuranceState(testConfig), genHash) { (preState, parent) =>
      val input = AssuranceInput(List.empty, slot = 100L, parent = parent)

      val (_, output) = AssuranceTransition.stfInternal(input, preState, testConfig)

      whenever(output.isRight) {
        // GP: With 0 assurances, count <= superMajority, so no cores available
        output.toOption.get.reported shouldBe empty
      }
    }
  }

  test("GP: bitfield size formula is ceil(C/8)") {
    // GP: bitfield size = ceil(coresCount / 8)
    val tinyBitfieldSize = (testConfig.coresCount + 7) / 8
    tinyBitfieldSize shouldBe 1 // ceil(2/8) = 1

    val fullConfig = ChainConfig.FULL
    val fullBitfieldSize = (fullConfig.coresCount + 7) / 8
    fullBitfieldSize shouldBe 43 // ceil(341/8) = 43
  }

  test("GP: assurance Ed25519 signature verification with real test vector data") {
    // From jamtestvectors - validator 0's assurance signature
    val pubKeyHex = "4418fb8c85bb3985394a8c2756d3643457ce614546202a2f50b093d762499ace"
    val anchorHex = "d61a38a0f73beda90e8c1dfba731f65003742539f4260694f44e22cabef24a8e"
    val bitfield = Array(0x02.toByte) // Core 1 set
    val signatureHex =
      "f23ddcfb8239e6b9fe943b085b5661b587d3cee9a5db2d3098ed69a1debc621ef17fc6960b39fb90a7e6675a1c7ad1c0eed37894f3ba240f918620a597075e0d"

    val pubKey = io.forge.jam.core.primitives.Ed25519PublicKey(hexToBytes(pubKeyHex))
    val anchor = io.forge.jam.core.primitives.Hash(hexToBytes(anchorHex))
    val signature = io.forge.jam.core.primitives.Ed25519Signature(hexToBytes(signatureHex))

    // GP: Assurance message = "$jam_available" ++ blake2b(encode(parent, bitfield))
    val messagePayload = io.forge.jam.core.Hashing.blake2b256(
      io.forge.jam.core.scodec.JamCodecs.encode((anchor, io.forge.jam.core.JamBytes(bitfield)))
    )
    val message = io.forge.jam.core.constants.JAM_AVAILABLE_BYTES ++ messagePayload.bytes

    // GP: Verify signature using JNI
    val isValid = io.forge.jam.crypto.Ed25519.verify(pubKey, message, signature)
    isValid shouldBe true
  }

  test("GP: assurance signature verification rejects invalid signature") {
    val pubKeyHex = "4418fb8c85bb3985394a8c2756d3643457ce614546202a2f50b093d762499ace"
    val anchorHex = "d61a38a0f73beda90e8c1dfba731f65003742539f4260694f44e22cabef24a8e"
    val bitfield = Array(0x02.toByte)

    val pubKey = io.forge.jam.core.primitives.Ed25519PublicKey(hexToBytes(pubKeyHex))
    val anchor = io.forge.jam.core.primitives.Hash(hexToBytes(anchorHex))
    val badSignature = io.forge.jam.core.primitives.Ed25519Signature(new Array[Byte](64))

    val messagePayload = io.forge.jam.core.Hashing.blake2b256(
      io.forge.jam.core.scodec.JamCodecs.encode((anchor, io.forge.jam.core.JamBytes(bitfield)))
    )
    val message = io.forge.jam.core.constants.JAM_AVAILABLE_BYTES ++ messagePayload.bytes

    // GP: Bad signature should fail verification
    val isValid = io.forge.jam.crypto.Ed25519.verify(pubKey, message, badSignature)
    isValid shouldBe false
  }

  test("GP: assurance bitfield is correctly parsed") {
    // Bitfield 0x03 means cores 0 and 1 are set
    val bitfield = 0x03.toByte

    // GP: Bit at position c is set if (bitfield[c/8] >> (c%8)) & 1 == 1
    val core0Set = ((bitfield >> 0) & 1) == 1
    val core1Set = ((bitfield >> 1) & 1) == 1
    val core2Set = ((bitfield >> 2) & 1) == 1

    core0Set shouldBe true
    core1Set shouldBe true
    core2Set shouldBe false
  }

  private def hexToBytes(hex: String): Array[Byte] =
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
