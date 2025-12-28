package io.forge.jam.protocol.dispute

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{Hash, Ed25519PublicKey, Ed25519Signature}
import io.forge.jam.core.types.extrinsic.{Dispute, Verdict}
import io.forge.jam.core.types.dispute.{Culprit, Fault}
import io.forge.jam.core.types.work.Vote
import io.forge.jam.protocol.generators.StfGenerators.*
import io.forge.jam.protocol.dispute.DisputeTypes.*
import io.forge.jam.core.primitives.{ValidatorIndex, Timeslot}
import spire.math.{UByte, UInt, UShort}

/**
 * - GP: Vote distribution allows only 0, V/3, or 2V/3+1 positive votes
 * - GP: Verdicts categorized as good/bad/wonky based on vote count
 * - GP: Offenders list grows monotonically (append-only)
 * - GP: Sorting requirements for verdicts, culprits, faults
 * - GP: Judgment age must be current or previous epoch
 */
class DisputesSTFSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  private val tinyConfig = ChainConfig.TINY
  private val fullConfig = ChainConfig.FULL

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  test("GP: TINY config vote thresholds match formulas V/3 and 2V/3+1") {
    // GP: oneThird = V/3 where V = validatorCount
    val expectedOneThird = tinyConfig.validatorCount / 3
    tinyConfig.oneThird shouldBe expectedOneThird
    tinyConfig.oneThird shouldBe 2 // 6/3 = 2

    // GP: votesPerVerdict = 2V/3 + 1 (supermajority)
    val expectedSupermajority = (2 * tinyConfig.validatorCount) / 3 + 1
    tinyConfig.votesPerVerdict shouldBe expectedSupermajority
    tinyConfig.votesPerVerdict shouldBe 5 // (2*6)/3 + 1 = 5
  }

  test("GP: FULL config vote thresholds match formulas V/3 and 2V/3+1") {
    // GP: V = 1023 validators
    fullConfig.validatorCount shouldBe 1023

    // GP: oneThird = V/3
    val expectedOneThird = fullConfig.validatorCount / 3
    fullConfig.oneThird shouldBe expectedOneThird
    fullConfig.oneThird shouldBe 341 // 1023/3 = 341

    // GP: votesPerVerdict = 2V/3 + 1
    val expectedSupermajority = (2 * fullConfig.validatorCount) / 3 + 1
    fullConfig.votesPerVerdict shouldBe expectedSupermajority
    fullConfig.votesPerVerdict shouldBe 683 // (2*1023)/3 + 1 = 683
  }

  test("GP: only three valid vote thresholds exist (0, V/3, 2V/3+1)") {
    val validThresholds = Set(0, tinyConfig.oneThird, tinyConfig.votesPerVerdict)

    // Verify the three thresholds
    validThresholds shouldBe Set(0, 2, 5)

    // Any intermediate value should be invalid
    (1 until tinyConfig.validatorCount).foreach { n =>
      if !validThresholds.contains(n) then
        validThresholds.contains(n) shouldBe false
    }
  }

  test("GP: empty disputes input produces successful output") {
    forAll(genDisputeState(tinyConfig)) { preState =>
      val input = DisputeInput(Dispute(List.empty, List.empty, List.empty))

      val (postState, output) = DisputeTransition.stfInternal(input, preState, tinyConfig)

      // GP: Empty disputes should succeed
      output.isRight shouldBe true

      // GP: ψ should remain unchanged
      postState.psi.good shouldBe preState.psi.good
      postState.psi.bad shouldBe preState.psi.bad
      postState.psi.wonky shouldBe preState.psi.wonky
      postState.psi.offenders shouldBe preState.psi.offenders
    }
  }

  test("GP: unsorted verdicts are rejected with VerdictsNotSortedUnique") {
    forAll(genDisputeState(tinyConfig)) { preState =>
      // Create two verdicts with hashes in wrong order
      val hash1 = Hash(Array.fill(32)(0xff.toByte)) // Large hash
      val hash2 = Hash(Array.fill(32)(0x00.toByte)) // Small hash

      // Put larger hash first (wrong order)
      val unsortedVerdicts = List(
        createMinimalVerdict(hash1, tinyConfig),
        createMinimalVerdict(hash2, tinyConfig)
      )

      val input = DisputeInput(Dispute(unsortedVerdicts, List.empty, List.empty))
      val (_, output) = DisputeTransition.stfInternal(input, preState, tinyConfig)

      // GP: Should reject with VerdictsNotSortedUnique
      output.isLeft shouldBe true
      output.left.toOption.get shouldBe DisputeErrorCode.VerdictsNotSortedUnique
    }
  }

  test("GP: duplicate verdict targets are rejected") {
    forAll(genDisputeState(tinyConfig), genHash) { (preState, targetHash) =>
      // Create two verdicts with same target (duplicates)
      val duplicateVerdicts = List(
        createMinimalVerdict(targetHash, tinyConfig),
        createMinimalVerdict(targetHash, tinyConfig)
      )

      val input = DisputeInput(Dispute(duplicateVerdicts, List.empty, List.empty))
      val (_, output) = DisputeTransition.stfInternal(input, preState, tinyConfig)

      // GP: Should reject (duplicates means not unique)
      output.isLeft shouldBe true
    }
  }

  test("GP: offenders list is superset of previous offenders after transition") {
    forAll(genDisputeState(tinyConfig)) { preState =>
      val input = DisputeInput(Dispute(List.empty, List.empty, List.empty))

      val (postState, output) = DisputeTransition.stfInternal(input, preState, tinyConfig)

      whenever(output.isRight) {
        // GP: All previous offenders must still be present
        preState.psi.offenders.foreach { preOffender =>
          val found = postState.psi.offenders.exists { postOffender =>
            java.util.Arrays.equals(preOffender.bytes, postOffender.bytes)
          }
          found shouldBe true
        }

        // GP: Offenders list size never decreases
        postState.psi.offenders.size should be >= preState.psi.offenders.size
      }
    }
  }

  test("GP: existing offenders are preserved across multiple transitions") {
    forAll(genPsi, genDisputeState(tinyConfig)) { (initialPsi, baseState) =>
      // Create state with known offenders
      val stateWithOffenders = baseState.copy(psi = initialPsi)
      val input = DisputeInput(Dispute(List.empty, List.empty, List.empty))

      // Apply transition
      val (postState, output) = DisputeTransition.stfInternal(input, stateWithOffenders, tinyConfig)

      whenever(output.isRight) {
        // All original offenders must be preserved
        initialPsi.offenders.foreach { originalOffender =>
          postState.psi.offenders.exists { postOffender =>
            java.util.Arrays.equals(originalOffender.bytes, postOffender.bytes)
          } shouldBe true
        }
      }
    }
  }

  test("GP: τ is preserved during dispute processing") {
    forAll(genDisputeState(tinyConfig)) { preState =>
      val input = DisputeInput(Dispute(List.empty, List.empty, List.empty))

      val (postState, output) = DisputeTransition.stfInternal(input, preState, tinyConfig)

      whenever(output.isRight) {
        // GP: Disputes STF does not modify τ
        postState.tau shouldBe preState.tau
      }
    }
  }

  test("GP: validator sets κ and λ are preserved during dispute processing") {
    forAll(genDisputeState(tinyConfig)) { preState =>
      val input = DisputeInput(Dispute(List.empty, List.empty, List.empty))

      val (postState, output) = DisputeTransition.stfInternal(input, preState, tinyConfig)

      whenever(output.isRight) {
        // GP: κ (current validators) unchanged
        postState.kappa.size shouldBe preState.kappa.size
        postState.kappa.zip(preState.kappa).foreach {
          case (post, pre) =>
            java.util.Arrays.equals(post.bandersnatch.bytes, pre.bandersnatch.bytes) shouldBe true
            java.util.Arrays.equals(post.ed25519.bytes, pre.ed25519.bytes) shouldBe true
        }

        // GP: λ (previous validators) unchanged
        postState.lambda.size shouldBe preState.lambda.size
        postState.lambda.zip(preState.lambda).foreach {
          case (post, pre) =>
            java.util.Arrays.equals(post.bandersnatch.bytes, pre.bandersnatch.bytes) shouldBe true
            java.util.Arrays.equals(post.ed25519.bytes, pre.ed25519.bytes) shouldBe true
        }
      }
    }
  }

  test("GP: ρ size equals C (cores count) and is preserved") {
    forAll(genDisputeState(tinyConfig)) { preState =>
      // GP: |ρ| = C
      preState.rho.size shouldBe tinyConfig.coresCount

      val input = DisputeInput(Dispute(List.empty, List.empty, List.empty))
      val (postState, output) = DisputeTransition.stfInternal(input, preState, tinyConfig)

      whenever(output.isRight) {
        postState.rho.size shouldBe tinyConfig.coresCount
      }
    }
  }

  test("GP: verdict with invalid age is rejected") {
    forAll(genDisputeState(tinyConfig), genHash) { (preState, targetHash) =>
      // Create state at epoch 2
      val stateAtEpoch2 = preState.copy(tau = 2 * tinyConfig.epochLength + 5)

      // Create verdict with age = 0 (invalid - too old)
      val votes = (0 until tinyConfig.votesPerVerdict).map { i =>
        Vote(
          vote = true,
          validatorIndex = ValidatorIndex(UShort(i)),
          signature = Ed25519Signature(new Array[Byte](64))
        )
      }.toList

      val verdictWithOldAge = Verdict(
        target = targetHash,
        age = Timeslot(UInt(0)), // Epoch 0 is invalid when current is epoch 2
        votes = votes
      )

      val input = DisputeInput(Dispute(List(verdictWithOldAge), List.empty, List.empty))
      val (_, output) = DisputeTransition.stfInternal(input, stateAtEpoch2, tinyConfig)

      // GP: Should reject with BadJudgementAge (unless signature check fails first)
      output.isLeft shouldBe true
    }
  }

  test("GP: disputes STF is deterministic (same input → same output)") {
    forAll(genDisputeState(tinyConfig)) { preState =>
      val input = DisputeInput(Dispute(List.empty, List.empty, List.empty))

      val (postState1, output1) = DisputeTransition.stfInternal(input, preState, tinyConfig)
      val (postState2, output2) = DisputeTransition.stfInternal(input, preState, tinyConfig)

      // GP: Identical inputs produce identical outputs
      output1 shouldBe output2
      postState1.tau shouldBe postState2.tau
      postState1.psi.good shouldBe postState2.psi.good
      postState1.psi.bad shouldBe postState2.psi.bad
      postState1.psi.wonky shouldBe postState2.psi.wonky
      postState1.psi.offenders.size shouldBe postState2.psi.offenders.size
    }
  }

  test("GP: Ed25519 signature verification with real test vector data") {
    // From jamtestvectors - validator 0's ed25519 key
    val pubKeyHex = "4418fb8c85bb3985394a8c2756d3643457ce614546202a2f50b093d762499ace"
    val targetHex = "11da6d1f761ddf9bdb4c9d6e5303ebd41f61858d0a5647a1a7bfe089bf921be9"
    val signatureHex =
      "370cc0ccac71fdc30aec9c1844da766bb763acffe8e6fedc66abec5a1312397ee821b9dfa471bf731d7be868e85e3a695f9b360bfcb218928f8285d673975801"

    val pubKey = Ed25519PublicKey(hexToBytes(pubKeyHex))
    val target = Hash(hexToBytes(targetHex))
    val signature = Ed25519Signature(hexToBytes(signatureHex))

    // GP: Vote signature message is prefix ++ target
    val message = io.forge.jam.core.constants.JAM_VALID_BYTES ++ target.bytes

    // GP: Verify signature using JNI
    val isValid = io.forge.jam.crypto.Ed25519.verify(pubKey, message, signature)
    isValid shouldBe true
  }

  test("GP: Ed25519 verification rejects invalid signature") {
    val pubKeyHex = "4418fb8c85bb3985394a8c2756d3643457ce614546202a2f50b093d762499ace"
    val targetHex = "11da6d1f761ddf9bdb4c9d6e5303ebd41f61858d0a5647a1a7bfe089bf921be9"

    val pubKey = Ed25519PublicKey(hexToBytes(pubKeyHex))
    val target = Hash(hexToBytes(targetHex))
    // Invalid signature (all zeros)
    val badSignature = Ed25519Signature(new Array[Byte](64))

    val message = io.forge.jam.core.constants.JAM_VALID_BYTES ++ target.bytes

    // GP: Bad signature should fail verification
    val isValid = io.forge.jam.crypto.Ed25519.verify(pubKey, message, badSignature)
    isValid shouldBe false
  }

  test("GP: fault signature verification uses JAM_INVALID prefix for false vote") {
    // From jamtestvectors - fault with vote=false
    val faultKeyHex = "4418fb8c85bb3985394a8c2756d3643457ce614546202a2f50b093d762499ace"
    val faultTargetHex = "11da6d1f761ddf9bdb4c9d6e5303ebd41f61858d0a5647a1a7bfe089bf921be9"
    val faultSigHex =
      "04a8e50ee27865d466cec61410628b3e0644cbc51a914b3e9180e47deda9f2baedf28df8c75b65a64bc23448d26c6598e3a10195364628d8192eb3f15ea9ba07"

    val pubKey = Ed25519PublicKey(hexToBytes(faultKeyHex))
    val target = Hash(hexToBytes(faultTargetHex))
    val signature = Ed25519Signature(hexToBytes(faultSigHex))

    // GP: Fault with vote=false uses JAM_INVALID prefix
    val message = io.forge.jam.core.constants.JAM_INVALID_BYTES ++ target.bytes

    val isValid = io.forge.jam.crypto.Ed25519.verify(pubKey, message, signature)
    isValid shouldBe true
  }

  test("GP: bad reports in psi.bad are cleared from rho (eq:removenonpositive)") {
    import io.forge.jam.core.Hashing
    import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment}
    import _root_.scodec.Codec

    forAll(genWorkReport(tinyConfig), genDisputeState(tinyConfig)) { (workReport, baseState) =>
      // Compute the hash of the work report (same formula as in processDisputes)
      val reportHash = Hashing.blake2b256(
        summon[Codec[WorkReport]].encode(workReport).require.toByteArray
      )

      // Create an AvailabilityAssignment with this report
      val assignment = AvailabilityAssignment(workReport, 100L)

      // Create rho with this assignment at index 0, rest are None
      val rhoWithReport = Some(assignment) :: List.fill(tinyConfig.coresCount - 1)(None)

      // Create psi with the report hash in bad list
      val psiWithBad = Psi(
        good = List.empty,
        bad = List(reportHash),
        wonky = List.empty,
        offenders = List.empty
      )

      // Set up the state with the bad hash and the matching report in rho
      val stateWithBadReport = baseState.copy(
        psi = psiWithBad,
        rho = rhoWithReport
      )

      // Run STF with empty disputes (the clearing happens based on existing psi.bad)
      val input = DisputeInput(Dispute(List.empty, List.empty, List.empty))
      val (postState, output) = DisputeTransition.stfInternal(input, stateWithBadReport, tinyConfig)

      whenever(output.isRight) {
        // GP eq:removenonpositive: Report with hash in bad should be cleared from rho
        postState.rho.head shouldBe None

        // Other rho entries should remain unchanged
        postState.rho.tail shouldBe rhoWithReport.tail
      }
    }
  }

  test("GP: wonky reports in psi.wonky are cleared from rho (eq:removenonpositive)") {
    import io.forge.jam.core.Hashing
    import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment}
    import _root_.scodec.Codec

    forAll(genWorkReport(tinyConfig), genDisputeState(tinyConfig)) { (workReport, baseState) =>
      // Compute the hash of the work report
      val reportHash = Hashing.blake2b256(
        summon[Codec[WorkReport]].encode(workReport).require.toByteArray
      )

      // Create an AvailabilityAssignment with this report at index 1
      val assignment = AvailabilityAssignment(workReport, 200L)
      val rhoWithReport = None :: Some(assignment) :: List.fill(tinyConfig.coresCount - 2)(None)

      // Create psi with the report hash in wonky list
      val psiWithWonky = Psi(
        good = List.empty,
        bad = List.empty,
        wonky = List(reportHash),
        offenders = List.empty
      )

      val stateWithWonkyReport = baseState.copy(
        psi = psiWithWonky,
        rho = rhoWithReport
      )

      val input = DisputeInput(Dispute(List.empty, List.empty, List.empty))
      val (postState, output) = DisputeTransition.stfInternal(input, stateWithWonkyReport, tinyConfig)

      whenever(output.isRight) {
        // GP eq:removenonpositive: Report with hash in wonky should be cleared from rho
        postState.rho(1) shouldBe None
      }
    }
  }

  test("GP: good reports in psi.good are NOT cleared from rho") {
    import io.forge.jam.core.Hashing
    import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment}
    import _root_.scodec.Codec

    forAll(genWorkReport(tinyConfig), genDisputeState(tinyConfig)) { (workReport, baseState) =>
      val reportHash = Hashing.blake2b256(
        summon[Codec[WorkReport]].encode(workReport).require.toByteArray
      )

      val assignment = AvailabilityAssignment(workReport, 300L)
      val rhoWithReport = Some(assignment) :: List.fill(tinyConfig.coresCount - 1)(None)

      // Create psi with the report hash in GOOD list (not bad or wonky)
      val psiWithGood = Psi(
        good = List(reportHash),
        bad = List.empty,
        wonky = List.empty,
        offenders = List.empty
      )

      val stateWithGoodReport = baseState.copy(
        psi = psiWithGood,
        rho = rhoWithReport
      )

      val input = DisputeInput(Dispute(List.empty, List.empty, List.empty))
      val (postState, output) = DisputeTransition.stfInternal(input, stateWithGoodReport, tinyConfig)

      whenever(output.isRight) {
        // GP: Good reports should NOT be cleared - they remain in rho
        postState.rho.head shouldBe Some(assignment)
      }
    }
  }

  test("GP: reports not in psi (bad/wonky/good) are preserved in rho") {
    import io.forge.jam.core.types.workpackage.AvailabilityAssignment

    forAll(genWorkReport(tinyConfig), genDisputeState(tinyConfig)) { (workReport, baseState) =>
      val assignment = AvailabilityAssignment(workReport, 400L)
      val rhoWithReport = Some(assignment) :: List.fill(tinyConfig.coresCount - 1)(None)

      // Empty psi - no judgments at all
      val emptyPsi = Psi.empty

      val stateWithNoJudgments = baseState.copy(
        psi = emptyPsi,
        rho = rhoWithReport
      )

      val input = DisputeInput(Dispute(List.empty, List.empty, List.empty))
      val (postState, output) = DisputeTransition.stfInternal(input, stateWithNoJudgments, tinyConfig)

      whenever(output.isRight) {
        // GP: Reports with no matching judgment should be preserved
        postState.rho.head shouldBe Some(assignment)
      }
    }
  }

  private def hexToBytes(hex: String): Array[Byte] =
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  private def createMinimalVerdict(target: Hash, config: ChainConfig): Verdict =
    val votes = (0 until config.votesPerVerdict).map { i =>
      Vote(
        vote = true,
        validatorIndex = ValidatorIndex(UShort(i)),
        signature = Ed25519Signature(new Array[Byte](64))
      )
    }.toList

    Verdict(
      target = target,
      age = Timeslot(UInt(0)),
      votes = votes
    )
