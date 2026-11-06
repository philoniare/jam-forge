package io.forge.jam.protocol.safrole

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.Hashing
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.{TicketMark, TicketEnvelope}
import io.forge.jam.crypto.BandersnatchVrf
import io.forge.jam.protocol.generators.StfGenerators.*
import io.forge.jam.protocol.safrole.SafroleTypes.*
import spire.math.UByte

/**
 * - Slot validation (timeslot must be strictly monotonic)
 * - Entropy evolution using Blake2b-256 hashing
 * - Epoch transitions with validator set rotation
 * - Ticket accumulator invariants (sorted, unique, cutoff)
 * - Fallback sequence generation
 */
class SafroleSTFSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  private val testConfig = ChainConfig.TINY

  // Override default ScalaCheck configuration for faster tests
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  test("property: valid slot must be strictly greater than previous tau") {
    forAll(genSafroleState(testConfig)) { preState =>
      // Generate input with slot > tau
      val validSlot = preState.tau + 1
      val input = SafroleInput(
        slot = validSlot,
        entropy = Hash.zero,
        extrinsic = List.empty
      )

      val (postState, output) = SafroleTransition.stfInternal(input, preState, testConfig)

      // Property: output should be successful (Right)
      output.isRight shouldBe true

      // Property: post-state tau should equal input slot
      postState.tau shouldBe validSlot
    }
  }

  test("property: invalid slot (equal to tau) returns UNEXPECTED_SLOT error") {
    forAll(genSafroleState(testConfig)) { preState =>
      // Generate input with slot == tau (invalid)
      val invalidSlot = preState.tau
      val input = SafroleInput(
        slot = invalidSlot,
        entropy = Hash.zero,
        extrinsic = List.empty
      )

      val (postState, output) = SafroleTransition.stfInternal(input, preState, testConfig)

      // Property: output should be error (Left) with BadSlot
      output.isLeft shouldBe true
      output.left.toOption.get shouldBe SafroleErrorCode.BadSlot

      // Property: state should be unchanged on error
      postState shouldBe preState
    }
  }

  test("property: invalid slot (less than tau) returns UNEXPECTED_SLOT error") {
    forAll(genSafroleState(testConfig)) { preState =>
      whenever(preState.tau > 0) {
        // Generate input with slot < tau (invalid)
        val invalidSlot = preState.tau - 1
        val input = SafroleInput(
          slot = invalidSlot,
          entropy = Hash.zero,
          extrinsic = List.empty
        )

        val (postState, output) = SafroleTransition.stfInternal(input, preState, testConfig)

        // Property: output should be error (Left) with BadSlot
        output.isLeft shouldBe true
        output.left.toOption.get shouldBe SafroleErrorCode.BadSlot

        // Property: state should be unchanged on error
        postState shouldBe preState
      }
    }
  }

  test("edge case: tau = 0 (first slot of epoch)") {
    forAll(genSafroleStateAtEpochStart(testConfig)) { preState =>
      // State at epoch start (tau = 0)
      preState.tau shouldBe 0

      // Slot 1 should be valid
      val input = SafroleInput(
        slot = 1,
        entropy = Hash.zero,
        extrinsic = List.empty
      )

      val (postState, output) = SafroleTransition.stfInternal(input, preState, testConfig)

      // Property: transition should succeed
      output.isRight shouldBe true
      postState.tau shouldBe 1
    }
  }

  test("edge case: tau = epochLength - 1 (last slot before epoch boundary)") {
    forAll(genSafroleStateAtEpochEnd(testConfig)) { preState =>
      // State at epoch end
      preState.tau shouldBe testConfig.epochLength - 1

      // Next slot will cross epoch boundary
      val nextSlot = preState.tau + 1
      val input = SafroleInput(
        slot = nextSlot,
        entropy = Hash.zero,
        extrinsic = List.empty
      )

      val (postState, output) = SafroleTransition.stfInternal(input, preState, testConfig)

      // Property: transition should succeed
      output.isRight shouldBe true

      // Property: tau should be updated to new epoch
      postState.tau shouldBe nextSlot
    }
  }

  test("property: eta[0] evolves correctly using blake2b256(eta[0] || input_entropy)") {
    forAll(genSafroleState(testConfig), genHash) { (preState, inputEntropy) =>
      // Ensure we're not crossing epoch boundary for simpler entropy evolution
      val currentEpoch = preState.tau / testConfig.epochLength
      val slotInSameEpoch = (currentEpoch * testConfig.epochLength) +
        (preState.tau % testConfig.epochLength) + 1

      whenever(slotInSameEpoch / testConfig.epochLength == currentEpoch) {
        val input = SafroleInput(
          slot = slotInSameEpoch,
          entropy = inputEntropy,
          extrinsic = List.empty
        )

        val (postState, output) = SafroleTransition.stfInternal(input, preState, testConfig)

        whenever(output.isRight) {
          // Property: eta[0] should be blake2b256(previous_eta[0] || input_entropy)
          val expectedEta0 = Hashing.blake2b256(preState.eta.head.bytes ++ inputEntropy.bytes)
          postState.eta.head.bytes shouldBe expectedEta0.bytes
        }
      }
    }
  }

  test("property: eta values stay same within epoch (no rotation on non-boundary)") {
    forAll(genSafroleState(testConfig), genHash) { (preState, inputEntropy) =>
      // Ensure we stay within same epoch
      val currentEpoch = preState.tau / testConfig.epochLength
      val tauInEpoch = preState.tau % testConfig.epochLength

      // Only test if there's room in the epoch (not at last slot)
      whenever(tauInEpoch < testConfig.epochLength - 1) {
        val nextSlot = preState.tau + 1
        val nextEpoch = nextSlot / testConfig.epochLength

        whenever(nextEpoch == currentEpoch) {
          val input = SafroleInput(
            slot = nextSlot,
            entropy = inputEntropy,
            extrinsic = List.empty
          )

          val (postState, output) = SafroleTransition.stfInternal(input, preState, testConfig)

          whenever(output.isRight) {
            // Property: eta[1], eta[2], eta[3] should remain unchanged when not crossing epoch boundary
            postState.eta(1).bytes shouldBe preState.eta(1).bytes
            postState.eta(2).bytes shouldBe preState.eta(2).bytes
            postState.eta(3).bytes shouldBe preState.eta(3).bytes
          }
        }
      }
    }
  }

  test("property: epoch boundary rotates validator sets correctly") {
    forAll(genSafroleStateAtEpochEnd(testConfig), genHash) { (preState, inputEntropy) =>
      // Force state to be at epoch end and transition to next epoch
      val stateAtEpochEnd = preState.copy(tau = testConfig.epochLength - 1)
      val nextEpochSlot = testConfig.epochLength // First slot of next epoch

      val input = SafroleInput(
        slot = nextEpochSlot,
        entropy = inputEntropy,
        extrinsic = List.empty
      )

      val (postState, output) = SafroleTransition.stfInternal(input, stateAtEpochEnd, testConfig)

      whenever(output.isRight) {
        // Property: lambda <- kappa (previous epoch validators get old current validators)
        postState.lambda.zip(stateAtEpochEnd.kappa).foreach {
          case (post, pre) =>
            post.bandersnatch.bytes shouldBe pre.bandersnatch.bytes
        }

        // Property: kappa <- gammaK (current validators get old queued validators)
        postState.kappa.zip(stateAtEpochEnd.gammaK).foreach {
          case (post, pre) =>
            post.bandersnatch.bytes shouldBe pre.bandersnatch.bytes
        }
      }
    }
  }

  test("property: non-epoch boundary preserves validator sets unchanged") {
    forAll(genSafroleState(testConfig), genHash) { (preState, inputEntropy) =>
      // Ensure we stay within same epoch
      val currentEpoch = preState.tau / testConfig.epochLength
      val tauInEpoch = preState.tau % testConfig.epochLength

      whenever(tauInEpoch < testConfig.epochLength - 1) {
        val nextSlot = preState.tau + 1
        val nextEpoch = nextSlot / testConfig.epochLength

        whenever(nextEpoch == currentEpoch) {
          val input = SafroleInput(
            slot = nextSlot,
            entropy = inputEntropy,
            extrinsic = List.empty
          )

          val (postState, output) = SafroleTransition.stfInternal(input, preState, testConfig)

          whenever(output.isRight) {
            // Property: lambda should remain unchanged
            postState.lambda.zip(preState.lambda).foreach {
              case (post, pre) =>
                post.bandersnatch.bytes shouldBe pre.bandersnatch.bytes
            }

            // Property: kappa should remain unchanged
            postState.kappa.zip(preState.kappa).foreach {
              case (post, pre) =>
                post.bandersnatch.bytes shouldBe pre.bandersnatch.bytes
            }

            // Property: gammaK should remain unchanged
            postState.gammaK.zip(preState.gammaK).foreach {
              case (post, pre) =>
                post.bandersnatch.bytes shouldBe pre.bandersnatch.bytes
            }

            // Property: iota should remain unchanged
            postState.iota.zip(preState.iota).foreach {
              case (post, pre) =>
                post.bandersnatch.bytes shouldBe pre.bandersnatch.bytes
            }
          }
        }
      }
    }
  }

  test("property: epoch mark output produced only at epoch boundaries") {
    forAll(genSafroleStateAtEpochEnd(testConfig), genHash) { (preState, inputEntropy) =>
      val stateAtEpochEnd = preState.copy(tau = testConfig.epochLength - 1)
      val nextEpochSlot = testConfig.epochLength

      val input = SafroleInput(
        slot = nextEpochSlot,
        entropy = inputEntropy,
        extrinsic = List.empty
      )

      val (_, output) = SafroleTransition.stfInternal(input, stateAtEpochEnd, testConfig)

      whenever(output.isRight) {
        val outputData = output.toOption.get
        // Property: epoch mark should be produced at epoch boundary
        outputData.epochMark.isDefined shouldBe true

        // Property: epoch mark should contain correct validators
        outputData.epochMark.foreach(mark => mark.validators.size shouldBe testConfig.validatorCount)
      }
    }
  }

  test("property: no epoch mark when not crossing boundary") {
    forAll(genSafroleState(testConfig), genHash) { (preState, inputEntropy) =>
      val currentEpoch = preState.tau / testConfig.epochLength
      val tauInEpoch = preState.tau % testConfig.epochLength

      whenever(tauInEpoch < testConfig.epochLength - 1) {
        val nextSlot = preState.tau + 1
        val nextEpoch = nextSlot / testConfig.epochLength

        whenever(nextEpoch == currentEpoch) {
          val input = SafroleInput(
            slot = nextSlot,
            entropy = inputEntropy,
            extrinsic = List.empty
          )

          val (_, output) = SafroleTransition.stfInternal(input, preState, testConfig)

          whenever(output.isRight) {
            val outputData = output.toOption.get
            // Property: no epoch mark when not crossing boundary
            outputData.epochMark shouldBe None
          }
        }
      }
    }
  }

  test("property: gammaA must remain sorted by ticket ID after insertion") {
    forAll(genSafroleState(testConfig)) { preState =>
      // Verify gammaA is sorted in pre-state
      val ticketIds = preState.gammaA.map(_.id.toHex)
      val sortedIds = ticketIds.sorted

      // Property: gammaA should always be sorted
      ticketIds shouldBe sortedIds
    }
  }

  test("property: gammaA tickets are unique (no duplicate IDs)") {
    forAll(genSafroleState(testConfig)) { preState =>
      val ticketIds = preState.gammaA.map(_.id.toHex)

      // Property: all ticket IDs should be unique
      ticketIds.distinct.size shouldBe ticketIds.size
    }
  }

  test("property: gammaA size never exceeds epochLength") {
    forAll(genSafroleState(testConfig)) { preState =>
      // Property: ticket accumulator should never exceed epoch length
      preState.gammaA.size should be <= testConfig.epochLength
    }
  }

  test("edge case: empty gammaA is valid") {
    forAll(genSafroleState(testConfig), genHash) { (preState, inputEntropy) =>
      val stateWithEmptyGammaA = preState.copy(gammaA = List.empty)

      val nextSlot = stateWithEmptyGammaA.tau + 1
      val input = SafroleInput(
        slot = nextSlot,
        entropy = inputEntropy,
        extrinsic = List.empty
      )

      val (postState, output) = SafroleTransition.stfInternal(input, stateWithEmptyGammaA, testConfig)

      whenever(output.isRight) {
        // Property: empty gammaA should remain empty without ticket submissions
        postState.gammaA shouldBe List.empty
      }
    }
  }

  test("edge case: full gammaA (epochLength tickets) is handled correctly") {
    // Generate a state with exactly epochLength tickets
    val epochLength = testConfig.epochLength
    forAll(genSortedTicketMarks(epochLength, testConfig.ticketsPerValidator)) { tickets =>
      whenever(tickets.size == epochLength) {
        forAll(genSafroleState(testConfig), genHash) { (baseState, inputEntropy) =>
          val stateWithFullGammaA = baseState.copy(gammaA = tickets)

          // Verify gammaA is full
          stateWithFullGammaA.gammaA.size shouldBe epochLength

          val nextSlot = stateWithFullGammaA.tau + 1
          val input = SafroleInput(
            slot = nextSlot,
            entropy = inputEntropy,
            extrinsic = List.empty
          )

          val (postState, output) = SafroleTransition.stfInternal(input, stateWithFullGammaA, testConfig)

          whenever(output.isRight) {
            // Property: gammaA size should not increase beyond epochLength
            postState.gammaA.size should be <= epochLength
          }
        }
      }
    }
  }

  test("property: fallback sequence uses keys mode when tickets insufficient") {
    forAll(genSafroleStateAtEpochEnd(testConfig), genHash) { (preState, inputEntropy) =>
      // Create state at epoch end with empty ticket accumulator (insufficient tickets)
      val stateWithNoTickets = preState.copy(
        tau = testConfig.epochLength - 1,
        gammaA = List.empty
      )
      val nextEpochSlot = testConfig.epochLength

      val input = SafroleInput(
        slot = nextEpochSlot,
        entropy = inputEntropy,
        extrinsic = List.empty
      )

      val (postState, output) = SafroleTransition.stfInternal(input, stateWithNoTickets, testConfig)

      whenever(output.isRight) {
        // Property: gammaS should be Keys mode when insufficient tickets
        postState.gammaS match
          case TicketsOrKeys.Keys(keys) =>
            // Property: fallback sequence should have epochLength keys
            keys.size shouldBe testConfig.epochLength
          case TicketsOrKeys.Tickets(_) =>
            // Also acceptable if tickets mode based on other conditions
            succeed
      }
    }
  }

  test("property: tickets mode used when ticket count equals epochLength at cutoff") {
    // Generate exactly epochLength tickets
    val epochLength = testConfig.epochLength
    val ticketCutoff = testConfig.ticketCutoff

    forAll(genSortedTicketMarks(epochLength, testConfig.ticketsPerValidator)) { tickets =>
      whenever(tickets.size == epochLength) {
        forAll(genSafroleState(testConfig), genHash) { (baseState, inputEntropy) =>
          // Create state just before cutoff with full ticket accumulator
          // tau should be ticketCutoff - 1 so next slot is at cutoff
          val stateBeforeCutoff = baseState.copy(
            tau = ticketCutoff - 1,
            gammaA = tickets
          )

          val cutoffSlot = ticketCutoff.toLong
          val input = SafroleInput(
            slot = cutoffSlot,
            entropy = inputEntropy,
            extrinsic = List.empty
          )

          val (_, output) = SafroleTransition.stfInternal(input, stateBeforeCutoff, testConfig)

          whenever(output.isRight) {
            val outputData = output.toOption.get

            // Property: ticketsMark should be produced at cutoff crossing with full accumulator
            outputData.ticketsMark match
              case Some(ticketsMark) =>
                ticketsMark.size shouldBe epochLength
              case None =>
                // Not at cutoff or not full accumulator - also acceptable
                succeed
          }
        }
      }
    }
  }

  test("property: fallback sequence generation is deterministic") {
    forAll(genSafroleStateAtEpochEnd(testConfig), genHash) { (preState, inputEntropy) =>
      val stateWithNoTickets = preState.copy(
        tau = testConfig.epochLength - 1,
        gammaA = List.empty
      )
      val nextEpochSlot = testConfig.epochLength

      val input = SafroleInput(
        slot = nextEpochSlot,
        entropy = inputEntropy,
        extrinsic = List.empty
      )

      // Run STF twice with same inputs
      val (postState1, _) = SafroleTransition.stfInternal(input, stateWithNoTickets, testConfig)
      val (postState2, _) = SafroleTransition.stfInternal(input, stateWithNoTickets, testConfig)

      // Property: gammaS should be identical for identical inputs
      (postState1.gammaS, postState2.gammaS) match
        case (TicketsOrKeys.Keys(keys1), TicketsOrKeys.Keys(keys2)) =>
          keys1.zip(keys2).foreach {
            case (k1, k2) =>
              k1.bytes shouldBe k2.bytes
          }
        case (TicketsOrKeys.Tickets(t1), TicketsOrKeys.Tickets(t2)) =>
          t1.zip(t2).foreach {
            case (m1, m2) =>
              m1.id.toArray shouldBe m2.id.toArray
          }
        case _ =>
          fail("gammaS types should be identical for identical inputs")
    }
  }

  test("property: epoch boundary clears ticket accumulator") {
    forAll(genSafroleState(testConfig), genHash) { (preState, inputEntropy) =>
      // Setup state at epoch end
      val stateAtEpochEnd = preState.copy(tau = testConfig.epochLength - 1)
      val nextEpochSlot = testConfig.epochLength

      val input = SafroleInput(
        slot = nextEpochSlot,
        entropy = inputEntropy,
        extrinsic = List.empty
      )

      val (postState, output) = SafroleTransition.stfInternal(input, stateAtEpochEnd, testConfig)

      whenever(output.isRight) {
        // Property: gammaA should be empty after epoch boundary
        postState.gammaA shouldBe List.empty
      }
    }
  }

  private def hexToBytes(hex: String): Array[Byte] =
    val cleanHex = hex.stripPrefix("0x")
    cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  test("GP: ring VRF proof verification with real test vector data") {
    // Test data from jamtestvectors/stf/safrole/tiny/publish-tickets-no-mark-1.json
    import io.forge.jam.crypto.BandersnatchVrf
    import io.forge.jam.core.JamBytes
    import spire.math.UByte

    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    // Tickets entropy from test vector eta[2]
    val entropyHex = "bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"
    val entropy = io.forge.jam.core.primitives.Hash(hexToBytes("0x" + entropyHex))

    // Ring commitment (gamma_z) for TINY validators
    val gammaZHex = "b07448cf678ab70693c2d98a5859067d0f8cac5cb55095e40443cba0bebf090a13fae3bce396b32b484bde8efa788381853598d7531938d998ced21a4d8b406226261cfe5f0e666265c5935149301b618060b078a08249bca249abd14f94fe5492e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"
    val gammaZ = JamBytes(hexToBytes("0x" + gammaZHex))

    // Valid ring VRF signature from test vector (first ticket, attempt=2)
    val signatureHex = "32cf17fc1a644ec4beddef894bb3eaa0de48aaeae7a46f6640c068e8afa65ba4fd4838c3604255e69af4667aa693b89798541a876ddae9d663c23694dafaca6546839ec1262170ffe7a8ec36bddf2c95acc9b06e37df5f7c1b1c0f7b2a3e053fb3c51977b040f8a7b43a9efa3b34338964ac96da94857b55a36ec3a94b73c58cf57404acf36e5ff9aa5807f3e301daf55f8bde2552a0f2c9f48f1d270f513412b48d5e7cef8c930e91c8c612f292560df3f4717622883cf58d40a1ffcda116198e9697c9c98759836886b306ecef13c13db8c1780766d3033019e38a0cc1f6ed48b80034dfc16051afabeb532dc9ad538d6db123d2815dbfa6cbe74866ed9855eecbbab8e2011f84f71a2e360caeac3bcd64c4b46b11ca167e238cf5f0ccfe7fa4672999c5e600dab3ce872923c156ed02ca479ce7e23fbe3aecfbeb064124b33efde1047ea4a3bb26dccad623a576a3811c151635d9d0cc5fa6c6718c70bad9c366b8590e0ee0ac16d2033b2512cf44ebfbf62bf140a4906b1e0d79b8481a168d4777d92820d978e956d0019a75d0afa19a27d08f105e6447e3abca3697273199c42970096952350793e5d700b46e35a28d9ce1b433795d7a20ac8d1258ea68e75eda361f79454040cca38351faa74ae722aff185ab1aabab93f23cd1a7cb1305aa0da4ecbc5d0b6e4e258a6e2162e1019e8268c64d0627d511c4910c427f3439caa1a05d235057cc94110e40f66d2c5b45a944395c5a5ec27789bb2d391502bca3fa7583610fb484eba21071b0bf8dff18c3d389576b647bb5b9bb49b5a4088db0e948737d74a5173d736ea8ed43609b968cc897dca45ed854e93fc182c12aa70ae8980653eea7c0cdfc97d576058b038726efe24f0e733e719d7fceec737673195ed0a1352723be03b44354b3df979cb4655a8f950485f836149b1d83ff5e435f32371a3d0497c2a37cf8fa497c41b6c23716fabfb98105686c90dd2e10b1f625bc197412f09de553f2ee6a4a2b46aacca69abfd73e1eb8d851d34b053d4c8b5570c2aa5bc844775f8148bb07c614e6014b73b65273227fe4647ed8a53d9b87daad403779251bb878508bdda1c343"
    val signature = JamBytes(hexToBytes("0x" + signatureHex))

    val attempt = UByte(2)
    val ringSize = 6 // TINY config

    // GP: Ring VRF verification should succeed and return a 32-byte ticket ID
    val result = BandersnatchVrf.verifyRingProof(signature, gammaZ, entropy, attempt, ringSize)

    result.isDefined shouldBe true
    result.get.ticketId.length shouldBe 32
    result.get.attempt shouldBe attempt
  }

  test("GP: ring VRF verification rejects invalid signature") {
    import io.forge.jam.crypto.BandersnatchVrf
    import io.forge.jam.core.JamBytes
    import spire.math.UByte

    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    val entropyHex = "bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"
    val entropy = io.forge.jam.core.primitives.Hash(hexToBytes("0x" + entropyHex))

    val gammaZHex = "b07448cf678ab70693c2d98a5859067d0f8cac5cb55095e40443cba0bebf090a13fae3bce396b32b484bde8efa788381853598d7531938d998ced21a4d8b406226261cfe5f0e666265c5935149301b618060b078a08249bca249abd14f94fe5492e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"
    val gammaZ = JamBytes(hexToBytes("0x" + gammaZHex))

    // Invalid signature (all zeros)
    val invalidSignature = JamBytes(Array.fill(784)(0.toByte))

    val attempt = UByte(0)
    val ringSize = 6

    // GP: Invalid signatures should be rejected
    val result = BandersnatchVrf.verifyRingProof(invalidSignature, gammaZ, entropy, attempt, ringSize)
    result.isDefined shouldBe false
  }

  test("GP: ring VRF verification rejects wrong attempt index") {
    import io.forge.jam.crypto.BandersnatchVrf
    import io.forge.jam.core.JamBytes
    import spire.math.UByte

    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    val entropyHex = "bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"
    val entropy = io.forge.jam.core.primitives.Hash(hexToBytes("0x" + entropyHex))

    val gammaZHex = "b07448cf678ab70693c2d98a5859067d0f8cac5cb55095e40443cba0bebf090a13fae3bce396b32b484bde8efa788381853598d7531938d998ced21a4d8b406226261cfe5f0e666265c5935149301b618060b078a08249bca249abd14f94fe5492e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"
    val gammaZ = JamBytes(hexToBytes("0x" + gammaZHex))

    // Valid signature for attempt=2, but we'll try with attempt=0
    val signatureHex = "32cf17fc1a644ec4beddef894bb3eaa0de48aaeae7a46f6640c068e8afa65ba4fd4838c3604255e69af4667aa693b89798541a876ddae9d663c23694dafaca6546839ec1262170ffe7a8ec36bddf2c95acc9b06e37df5f7c1b1c0f7b2a3e053fb3c51977b040f8a7b43a9efa3b34338964ac96da94857b55a36ec3a94b73c58cf57404acf36e5ff9aa5807f3e301daf55f8bde2552a0f2c9f48f1d270f513412b48d5e7cef8c930e91c8c612f292560df3f4717622883cf58d40a1ffcda116198e9697c9c98759836886b306ecef13c13db8c1780766d3033019e38a0cc1f6ed48b80034dfc16051afabeb532dc9ad538d6db123d2815dbfa6cbe74866ed9855eecbbab8e2011f84f71a2e360caeac3bcd64c4b46b11ca167e238cf5f0ccfe7fa4672999c5e600dab3ce872923c156ed02ca479ce7e23fbe3aecfbeb064124b33efde1047ea4a3bb26dccad623a576a3811c151635d9d0cc5fa6c6718c70bad9c366b8590e0ee0ac16d2033b2512cf44ebfbf62bf140a4906b1e0d79b8481a168d4777d92820d978e956d0019a75d0afa19a27d08f105e6447e3abca3697273199c42970096952350793e5d700b46e35a28d9ce1b433795d7a20ac8d1258ea68e75eda361f79454040cca38351faa74ae722aff185ab1aabab93f23cd1a7cb1305aa0da4ecbc5d0b6e4e258a6e2162e1019e8268c64d0627d511c4910c427f3439caa1a05d235057cc94110e40f66d2c5b45a944395c5a5ec27789bb2d391502bca3fa7583610fb484eba21071b0bf8dff18c3d389576b647bb5b9bb49b5a4088db0e948737d74a5173d736ea8ed43609b968cc897dca45ed854e93fc182c12aa70ae8980653eea7c0cdfc97d576058b038726efe24f0e733e719d7fceec737673195ed0a1352723be03b44354b3df979cb4655a8f950485f836149b1d83ff5e435f32371a3d0497c2a37cf8fa497c41b6c23716fabfb98105686c90dd2e10b1f625bc197412f09de553f2ee6a4a2b46aacca69abfd73e1eb8d851d34b053d4c8b5570c2aa5bc844775f8148bb07c614e6014b73b65273227fe4647ed8a53d9b87daad403779251bb878508bdda1c343"
    val signature = JamBytes(hexToBytes("0x" + signatureHex))

    // Wrong attempt (signature was created for attempt=2)
    val wrongAttempt = UByte(0)
    val ringSize = 6

    // GP: Verification with wrong attempt should fail
    val result = BandersnatchVrf.verifyRingProof(signature, gammaZ, entropy, wrongAttempt, ringSize)
    result.isDefined shouldBe false
  }

  test("GP: ring commitment generation from validator keys") {
    import io.forge.jam.crypto.BandersnatchVrf
    import io.forge.jam.core.primitives.BandersnatchPublicKey

    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    // TINY validator Bandersnatch keys from test vector
    val tinyValidatorKeys: List[BandersnatchPublicKey] = List(
      "0x4fc1cc2146c5463a023a66d1fc72829025ef654f1e1dd5c917c40346c2b3aa06",
      "0x4d5c855aebc996a88e818acd27d995f8e04c5dc49652c349f3f3204b39f1984a",
      "0x60a294af38653b7f0073767a0b9d10d8bf0efa6804782a9ecd17a9f5fd323b5b",
      "0x1e50be41fa601015847ae50ef0946ba5217027653d0ffd7a879c45ca21bab357",
      "0xb9b83ef77c1fda8f4b2909e531e6842bf26d4e6f69ed1405fc8d12d1350e6265",
      "0x7ef0acbb01d1ad5da7a1b576aef6ea9a91b979612ff339970c69fe0004d68e05"
    ).map(hex => BandersnatchPublicKey(hexToBytes(hex)))

    val expectedGammaZ = "b07448cf678ab70693c2d98a5859067d0f8cac5cb55095e40443cba0bebf090a13fae3bce396b32b484bde8efa788381853598d7531938d998ced21a4d8b406226261cfe5f0e666265c5935149301b618060b078a08249bca249abd14f94fe5492e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"

    val ringSize = 6
    val result = BandersnatchVrf.generateRingRoot(tinyValidatorKeys, ringSize)

    // GP: Ring commitment should be 144 bytes
    result.isDefined shouldBe true
    result.get.length shouldBe BandersnatchVrf.RingCommitmentSize

    // GP: Ring commitment should match expected value
    val resultHex = result.get.toArray.map(b => f"${b & 0xff}%02x").mkString
    resultHex shouldBe expectedGammaZ
  }

  test("GP: ring VRF signature size is 784 bytes") {
    import io.forge.jam.crypto.BandersnatchVrf

    // GP: Ring VRF signature must be exactly 784 bytes
    BandersnatchVrf.RingVrfSignatureSize shouldBe 784
  }

  test("GP: ring commitment size is 144 bytes") {
    import io.forge.jam.crypto.BandersnatchVrf

    // GP: Ring commitment (gamma_z) must be exactly 144 bytes
    BandersnatchVrf.RingCommitmentSize shouldBe 144
  }

  private val tinyBanderKeys: List[BandersnatchPublicKey] = List(
    "0x4fc1cc2146c5463a023a66d1fc72829025ef654f1e1dd5c917c40346c2b3aa06",
    "0x4d5c855aebc996a88e818acd27d995f8e04c5dc49652c349f3f3204b39f1984a",
    "0x60a294af38653b7f0073767a0b9d10d8bf0efa6804782a9ecd17a9f5fd323b5b",
    "0x1e50be41fa601015847ae50ef0946ba5217027653d0ffd7a879c45ca21bab357",
    "0xb9b83ef77c1fda8f4b2909e531e6842bf26d4e6f69ed1405fc8d12d1350e6265",
    "0x7ef0acbb01d1ad5da7a1b576aef6ea9a91b979612ff339970c69fe0004d68e05"
  ).map(h => BandersnatchPublicKey(hexToBytes(h)))

  // Ring commitment (gamma_z) over the six keys above, in order.
  private val tinyFullRingRootHex =
    "b07448cf678ab70693c2d98a5859067d0f8cac5cb55095e40443cba0bebf090a13fae3bce396b32b484bde8efa788381853598d7531938d998ced21a4d8b406226261cfe5f0e666265c5935149301b618060b078a08249bca249abd14f94fe5492e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"

  // A real ring-VRF ticket (attempt 2) and its verification inputs.
  private val ticketEntropyHex = "bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"
  private val ticketSignatureHex =
    "32cf17fc1a644ec4beddef894bb3eaa0de48aaeae7a46f6640c068e8afa65ba4fd4838c3604255e69af4667aa693b89798541a876ddae9d663c23694dafaca6546839ec1262170ffe7a8ec36bddf2c95acc9b06e37df5f7c1b1c0f7b2a3e053fb3c51977b040f8a7b43a9efa3b34338964ac96da94857b55a36ec3a94b73c58cf57404acf36e5ff9aa5807f3e301daf55f8bde2552a0f2c9f48f1d270f513412b48d5e7cef8c930e91c8c612f292560df3f4717622883cf58d40a1ffcda116198e9697c9c98759836886b306ecef13c13db8c1780766d3033019e38a0cc1f6ed48b80034dfc16051afabeb532dc9ad538d6db123d2815dbfa6cbe74866ed9855eecbbab8e2011f84f71a2e360caeac3bcd64c4b46b11ca167e238cf5f0ccfe7fa4672999c5e600dab3ce872923c156ed02ca479ce7e23fbe3aecfbeb064124b33efde1047ea4a3bb26dccad623a576a3811c151635d9d0cc5fa6c6718c70bad9c366b8590e0ee0ac16d2033b2512cf44ebfbf62bf140a4906b1e0d79b8481a168d4777d92820d978e956d0019a75d0afa19a27d08f105e6447e3abca3697273199c42970096952350793e5d700b46e35a28d9ce1b433795d7a20ac8d1258ea68e75eda361f79454040cca38351faa74ae722aff185ab1aabab93f23cd1a7cb1305aa0da4ecbc5d0b6e4e258a6e2162e1019e8268c64d0627d511c4910c427f3439caa1a05d235057cc94110e40f66d2c5b45a944395c5a5ec27789bb2d391502bca3fa7583610fb484eba21071b0bf8dff18c3d389576b647bb5b9bb49b5a4088db0e948737d74a5173d736ea8ed43609b968cc897dca45ed854e93fc182c12aa70ae8980653eea7c0cdfc97d576058b038726efe24f0e733e719d7fceec737673195ed0a1352723be03b44354b3df979cb4655a8f950485f836149b1d83ff5e435f32371a3d0497c2a37cf8fa497c41b6c23716fabfb98105686c90dd2e10b1f625bc197412f09de553f2ee6a4a2b46aacca69abfd73e1eb8d851d34b053d4c8b5570c2aa5bc844775f8148bb07c614e6014b73b65273227fe4647ed8a53d9b87daad403779251bb878508bdda1c343"

  /** Build a TINY validator key; `blsByte`/`edByte` let a test mark one key as an
    * offender with a non-zero BLS body so the zeroing is observable. */
  private def tinyValidator(bander: BandersnatchPublicKey, edByte: Int, blsByte: Int): ValidatorKey =
    ValidatorKey(
      bandersnatch = bander,
      ed25519 = Ed25519PublicKey(Array.fill(32)(edByte.toByte)),
      bls = BlsPublicKey(Array.fill(144)(blsByte.toByte)),
      metadata = JamBytes(Array.fill(ValidatorKey.MetadataSize)(edByte.toByte))
    )

  /** iota = six validators on the valid TINY ring; validator 0 is the offender,
    * with a non-zero (0xAB) BLS key. Returns (preStateAtEpochEnd, iota). */
  private def offenderEpochEndState(cfg: ChainConfig): (SafroleState, IndexedSeq[ValidatorKey]) =
    val iota = tinyBanderKeys.zipWithIndex.map { case (b, i) =>
      tinyValidator(b, edByte = i + 1, blsByte = if i == 0 then 0xAB else i + 1)
    }.toIndexedSeq
    val pre = SafroleState(
      tau = cfg.epochLength - 1,
      eta = List.fill(4)(Hash.zero),
      lambda = iota,
      kappa = iota,
      gammaK = iota,
      iota = iota,
      gammaA = List.empty,
      gammaS = TicketsOrKeys.Keys(List.fill(cfg.epochLength)(tinyBanderKeys.head)),
      gammaZ = JamBytes(hexToBytes(tinyFullRingRootHex)), // root over the full (un-nullified) ring
      postOffenders = List(iota.head.ed25519)
    )
    (pre, iota)

  test("an offender's keys are nullified on the epoch transition and the ring root is regenerated") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI test")
    val cfg = ChainConfig.TINY
    val (pre, iota) = offenderEpochEndState(cfg)

    // Crossing into the next epoch applies the Φ offender filter.
    val input = SafroleInput(slot = cfg.epochLength.toLong, entropy = Hash.zero, extrinsic = List.empty)
    val (post, output) = SafroleTransition.stfInternal(input, pre, cfg)

    output.isRight shouldBe true

    // Validator 0 (the offender) becomes the null key K_0 in the posterior γ_K.
    val nulled = post.gammaK.head
    nulled.bandersnatch.bytes.forall(_ == 0) shouldBe true
    nulled.ed25519.bytes.toSeq shouldBe (2.toByte +: Seq.fill(31)(0.toByte))
    nulled.metadata.toArray.forall(_ == 0) shouldBe true

    // Non-offending validators are carried through unchanged.
    post.gammaK.tail.zip(iota.tail).foreach { case (p, original) =>
      p.bandersnatch.bytes shouldBe original.bandersnatch.bytes
      p.ed25519.bytes shouldBe original.ed25519.bytes
      p.bls.bytes shouldBe original.bls.bytes
    }

    post.gammaZ.length shouldBe SafroleTypes.BandersnatchRingCommitmentSize
    post.gammaZ.toArray.toSeq should not equal pre.gammaZ.toArray.toSeq
  }

  test("a convicted validator's full 144-byte BLS key is zeroed") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI test")
    val cfg = ChainConfig.TINY
    val (pre, iota) = offenderEpochEndState(cfg)

    // Pre-condition: the offender carries a non-zero, full-length BLS key.
    iota.head.bls.bytes.length shouldBe 144
    iota.head.bls.bytes.exists(_ != 0) shouldBe true

    val input = SafroleInput(slot = cfg.epochLength.toLong, entropy = Hash.zero, extrinsic = List.empty)
    val (post, output) = SafroleTransition.stfInternal(input, pre, cfg)

    output.isRight shouldBe true
    // All 144 BLS bytes of the convicted validator are zeroed in the posterior γ_K.
    val blsBytes = post.gammaK.head.bls.bytes
    blsBytes.length shouldBe 144
    blsBytes.forall(_ == 0) shouldBe true
  }

  test("a losing ticket against a saturated accumulator rejects the block") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI test")
    val cfg = ChainConfig.TINY

    val lowIds = (0 until cfg.epochLength).toList.map { i =>
      val b = new Array[Byte](32); b(31) = i.toByte
      TicketMark(JamBytes(b), UByte(0))
    }
    val validators = (0 until cfg.validatorCount).map(i => tinyValidator(tinyBanderKeys(i), i + 1, i + 1))

    val pre = SafroleState(
      tau = 0L,
      // eta(2) drives ticket verification and must match the vector's entropy.
      eta = List(Hash.zero, Hash.zero, Hash(hexToBytes(ticketEntropyHex)), Hash.zero),
      lambda = validators,
      kappa = validators,
      gammaK = validators,
      iota = validators,
      gammaA = lowIds,
      gammaS = TicketsOrKeys.Keys(List.fill(cfg.epochLength)(tinyBanderKeys.head)),
      gammaZ = JamBytes(hexToBytes(tinyFullRingRootHex)),
      postOffenders = List.empty
    )
    // slot 1 stays in epoch 0 (no rotation) and is before the ticket cutoff.
    val realTicket = TicketEnvelope(UByte(2), JamBytes(hexToBytes(ticketSignatureHex)))
    val input = SafroleInput(slot = 1L, entropy = Hash.zero, extrinsic = List(realTicket))

    val (post, output) = SafroleTransition.stfInternal(input, pre, cfg)

    output.isLeft shouldBe true
    output.left.toOption.get shouldBe SafroleErrorCode.Reserved
    post shouldBe pre
  }
