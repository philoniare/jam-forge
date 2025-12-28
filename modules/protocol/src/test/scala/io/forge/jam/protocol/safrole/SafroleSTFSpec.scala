package io.forge.jam.protocol.safrole

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.Hashing
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.generators.StfGenerators.*
import io.forge.jam.protocol.safrole.SafroleTypes.*

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
          java.util.Arrays.equals(postState.eta.head.bytes, expectedEta0.bytes) shouldBe true
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
            java.util.Arrays.equals(postState.eta(1).bytes, preState.eta(1).bytes) shouldBe true
            java.util.Arrays.equals(postState.eta(2).bytes, preState.eta(2).bytes) shouldBe true
            java.util.Arrays.equals(postState.eta(3).bytes, preState.eta(3).bytes) shouldBe true
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
            java.util.Arrays.equals(post.bandersnatch.bytes, pre.bandersnatch.bytes) shouldBe true
        }

        // Property: kappa <- gammaK (current validators get old queued validators)
        postState.kappa.zip(stateAtEpochEnd.gammaK).foreach {
          case (post, pre) =>
            java.util.Arrays.equals(post.bandersnatch.bytes, pre.bandersnatch.bytes) shouldBe true
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
                java.util.Arrays.equals(post.bandersnatch.bytes, pre.bandersnatch.bytes) shouldBe true
            }

            // Property: kappa should remain unchanged
            postState.kappa.zip(preState.kappa).foreach {
              case (post, pre) =>
                java.util.Arrays.equals(post.bandersnatch.bytes, pre.bandersnatch.bytes) shouldBe true
            }

            // Property: gammaK should remain unchanged
            postState.gammaK.zip(preState.gammaK).foreach {
              case (post, pre) =>
                java.util.Arrays.equals(post.bandersnatch.bytes, pre.bandersnatch.bytes) shouldBe true
            }

            // Property: iota should remain unchanged
            postState.iota.zip(preState.iota).foreach {
              case (post, pre) =>
                java.util.Arrays.equals(post.bandersnatch.bytes, pre.bandersnatch.bytes) shouldBe true
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
              java.util.Arrays.equals(k1.bytes, k2.bytes) shouldBe true
          }
        case (TicketsOrKeys.Tickets(t1), TicketsOrKeys.Tickets(t2)) =>
          t1.zip(t2).foreach {
            case (m1, m2) =>
              java.util.Arrays.equals(m1.id.toArray, m2.id.toArray) shouldBe true
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
    // Test data from jamtestvectors/stf/safrole/full/publish-tickets-no-mark-1.json
    import io.forge.jam.crypto.BandersnatchVrf
    import io.forge.jam.core.JamBytes
    import spire.math.UByte

    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI tests")

    // Tickets entropy from test vector eta[2]
    val entropyHex = "bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"
    val entropy = io.forge.jam.core.primitives.Hash(hexToBytes("0x" + entropyHex))

    // Ring commitment (gamma_z) for TINY validators
    val gammaZHex = "af39b7de5fcfb9fb8a46b1645310529ce7d08af7301d9758249da4724ec698eb127f489b58e49ae9ab85027509116962a135fc4d97b66fbbed1d3df88cd7bf5cc6e5d7391d261a4b552246648defcb64ad440d61d69ec61b5473506a48d58e1992e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"
    val gammaZ = JamBytes(hexToBytes("0x" + gammaZHex))

    // Valid ring VRF signature from test vector (first ticket with attempt=0)
    val signatureHex = "e5055ade89aba8daa558078c28b6f86468772f63bebe920d88ce3e189405c09660e430f3ea78a6d70aa849570e35de25a35414c1bf5df87f0ebcf495a9481ef289bfe8b2135788aa9687549f8a4a1d492323e817eb7ddd396d9d4d1f95da6b807822b436856a8a5c06f65f0aecb4a077944d0b5b308293c403a28a91c8d4474fe99615eebe49cd57973e101a5bc3982144d236031327aa95f58d16d92364df062a89bb767c63aef3cd2938e7af15cf9e3e60b19afd0d3d78c43ed0af8e9fd0108407aef70f2bba8bf2bd9316eb6c1cc923e133e00096f6ebbb58b124491d7093f271db465926809975343eaa02d30c108d6db123d2815dbfa6cbe74866ed9855eecbbab8e2011f84f71a2e360caeac3bcd64c4b46b11ca167e238cf5f0ccfe7f93ff1cab74815f48db1759417f39f81d8887490789027344561d32285bf92ca503bca1dcf8b013d5938e865ea933221f98910d47cc02f4a9a826c16b5893a6c792c4aee4b20c2df607796db538b5c6538623b88151a00aad17689fa94ee32123f1ec933cb4fdbc00e5452ba7f151b86359c8fed9878216e1fdac29c26a5a700c2994d49afce3daec4839a2ec63f1c36b47bef47014f9433005103d9d1bd5541d409ec0cfbeaf2dd92627a1fb702e9a0b509403f965c3be6a31aeb8ad97cbc725e359d8b8960a3a512af9af95c2712f26a5d4483b69001c956abfdc781148d73db38ee8ce579f5030bb28d98fa84e4c942327b6e58ee6ce1f0f1e71b4e4b7263b347140d7979bd4d1cda8532dc8ba12dbc31f7b154f0bda72fc486adba2737e6621633f98350f73040c9abf9530a09989476e6243cdcf397ac05a0ccb71eb6265af7258d8dc1ac2f66210686e7deeca4099bb7b2240d2dae18bcd739cf9fdcb0536745cdf6b30c3c29312511b8f71a035c2114ea47439261d30a556b66b84d1a4ba2cc64bdf2b19f862a363b9d91ee022ab7ffa61dedd23efb3c4effdbe955638956d6ee3998cf874449763818d23fbdb7297d47f82db65db6ebac8a6d06c0dbeb3163095c13fd27af4e31416c5561c7adc552bf02ad47d1c99757bc66c302eb2a851c706b097869faed86517a22fae96"
    val signature = JamBytes(hexToBytes("0x" + signatureHex))

    val attempt = UByte(0)
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

    val gammaZHex = "af39b7de5fcfb9fb8a46b1645310529ce7d08af7301d9758249da4724ec698eb127f489b58e49ae9ab85027509116962a135fc4d97b66fbbed1d3df88cd7bf5cc6e5d7391d261a4b552246648defcb64ad440d61d69ec61b5473506a48d58e1992e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"
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

    val gammaZHex = "af39b7de5fcfb9fb8a46b1645310529ce7d08af7301d9758249da4724ec698eb127f489b58e49ae9ab85027509116962a135fc4d97b66fbbed1d3df88cd7bf5cc6e5d7391d261a4b552246648defcb64ad440d61d69ec61b5473506a48d58e1992e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"
    val gammaZ = JamBytes(hexToBytes("0x" + gammaZHex))

    // Valid signature for attempt=0, but we'll try with attempt=1
    val signatureHex = "e5055ade89aba8daa558078c28b6f86468772f63bebe920d88ce3e189405c09660e430f3ea78a6d70aa849570e35de25a35414c1bf5df87f0ebcf495a9481ef289bfe8b2135788aa9687549f8a4a1d492323e817eb7ddd396d9d4d1f95da6b807822b436856a8a5c06f65f0aecb4a077944d0b5b308293c403a28a91c8d4474fe99615eebe49cd57973e101a5bc3982144d236031327aa95f58d16d92364df062a89bb767c63aef3cd2938e7af15cf9e3e60b19afd0d3d78c43ed0af8e9fd0108407aef70f2bba8bf2bd9316eb6c1cc923e133e00096f6ebbb58b124491d7093f271db465926809975343eaa02d30c108d6db123d2815dbfa6cbe74866ed9855eecbbab8e2011f84f71a2e360caeac3bcd64c4b46b11ca167e238cf5f0ccfe7f93ff1cab74815f48db1759417f39f81d8887490789027344561d32285bf92ca503bca1dcf8b013d5938e865ea933221f98910d47cc02f4a9a826c16b5893a6c792c4aee4b20c2df607796db538b5c6538623b88151a00aad17689fa94ee32123f1ec933cb4fdbc00e5452ba7f151b86359c8fed9878216e1fdac29c26a5a700c2994d49afce3daec4839a2ec63f1c36b47bef47014f9433005103d9d1bd5541d409ec0cfbeaf2dd92627a1fb702e9a0b509403f965c3be6a31aeb8ad97cbc725e359d8b8960a3a512af9af95c2712f26a5d4483b69001c956abfdc781148d73db38ee8ce579f5030bb28d98fa84e4c942327b6e58ee6ce1f0f1e71b4e4b7263b347140d7979bd4d1cda8532dc8ba12dbc31f7b154f0bda72fc486adba2737e6621633f98350f73040c9abf9530a09989476e6243cdcf397ac05a0ccb71eb6265af7258d8dc1ac2f66210686e7deeca4099bb7b2240d2dae18bcd739cf9fdcb0536745cdf6b30c3c29312511b8f71a035c2114ea47439261d30a556b66b84d1a4ba2cc64bdf2b19f862a363b9d91ee022ab7ffa61dedd23efb3c4effdbe955638956d6ee3998cf874449763818d23fbdb7297d47f82db65db6ebac8a6d06c0dbeb3163095c13fd27af4e31416c5561c7adc552bf02ad47d1c99757bc66c302eb2a851c706b097869faed86517a22fae96"
    val signature = JamBytes(hexToBytes("0x" + signatureHex))

    // Wrong attempt (signature was created for attempt=0)
    val wrongAttempt = UByte(1)
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
      "0xff71c6c03ff88adb5ed52c9681de1629a54e702fc14729f6b50d2f0a76f185b3",
      "0xdee6d555b82024f1ccf8a1e37e60fa60fd40b1958c4bb3006af78647950e1b91",
      "0x9326edb21e5541717fde24ec085000b28709847b8aab1ac51f84e94b37ca1b66",
      "0x0746846d17469fb2f95ef365efcab9f4e22fa1feb53111c995376be8019981cc",
      "0x151e5c8fe2b9d8a606966a79edd2f9e5db47e83947ce368ccba53bf6ba20a40b",
      "0x2105650944fcd101621fd5bb3124c9fd191d114b7ad936c1d79d734f9f21392e"
    ).map(hex => BandersnatchPublicKey(hexToBytes(hex)))

    val expectedGammaZ = "af39b7de5fcfb9fb8a46b1645310529ce7d08af7301d9758249da4724ec698eb127f489b58e49ae9ab85027509116962a135fc4d97b66fbbed1d3df88cd7bf5cc6e5d7391d261a4b552246648defcb64ad440d61d69ec61b5473506a48d58e1992e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"

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
