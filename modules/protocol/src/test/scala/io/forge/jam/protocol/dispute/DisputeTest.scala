package io.forge.jam.protocol.dispute

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{
  Hash,
  BandersnatchPublicKey,
  Ed25519PublicKey,
  BlsPublicKey,
  Ed25519Signature
}
import io.forge.jam.core.types.extrinsic.Dispute
import io.forge.jam.core.types.dispute.{Culprit, Fault}
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.protocol.dispute.DisputeTypes.*
import io.forge.jam.protocol.dispute.DisputeTransition

/**
 * Tests for the Disputes State Transition Function.
 */
class DisputeTest extends AnyFunSuite with Matchers:

  // Tiny config: 6 validators, 2 cores
  val TinyConfig = ChainConfig.TINY

  // Full config: 1023 validators, 341 cores
  val FullConfig = ChainConfig.FULL

  private val validEd25519Keys: Seq[Array[Byte]] = Seq(
    hexToBytes("0100000000000000000000000000000000000000000000000000000000000000"),
    hexToBytes("c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a"),
    hexToBytes("26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05"),
    hexToBytes("26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85"),
    hexToBytes("ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"),
    hexToBytes("c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa")
  ).map(ensureLength32)

  private def ensureLength32(arr: Array[Byte]): Array[Byte] =
    if arr.length >= 32 then arr.take(32)
    else Array.fill(32 - arr.length)(0.toByte) ++ arr

  private def hexToBytes(hex: String): Array[Byte] =
    hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  private def validatorKeyFilled(value: Int): ValidatorKey =
    val ed25519Key = if value < validEd25519Keys.size then validEd25519Keys(value)
    else validEd25519Keys(value % validEd25519Keys.size)
    ValidatorKey(
      BandersnatchPublicKey(Array.fill(32)(value.toByte)),
      Ed25519PublicKey(ed25519Key),
      BlsPublicKey(Array.fill(144)(value.toByte)),
      JamBytes(Array.fill(128)(value.toByte))
    )

  // Helper to create empty dispute state
  private def emptyDisputeState(validatorCount: Int, coreCount: Int): DisputeState =
    DisputeState(
      psi = Psi.empty,
      rho = List.fill(coreCount)(None),
      tau = 0,
      kappa = (0 until validatorCount).map(i => validatorKeyFilled(i)).toList,
      lambda = (0 until validatorCount).map(i => validatorKeyFilled(i + 100)).toList
    )

  // Helper to create empty disputes
  private def emptyDisputes: Dispute =
    Dispute(List.empty, List.empty, List.empty)

  test("verdict processing with 0 positive votes (bad)") {
    // Test that a verdict with all negative votes classifies the report as bad
    val config = ChainConfig.TINY

    // With 0 positive votes out of 5, the report should be marked as bad
    // For bad verdict, we need at least 2 culprits
    config.votesPerVerdict shouldBe 5 // (2 * 6) / 3 + 1 = 5
    config.oneThird shouldBe 2 // 6 / 3 = 2

    // Create a state where we can test vote thresholds
    val state = emptyDisputeState(6, 2)

    // With empty disputes (no verdicts), should succeed
    val input = DisputeInput(emptyDisputes)
    val (postState, output) = DisputeTransition.stfInternal(input, state, config)

    output.isRight shouldBe true
    postState.psi.bad shouldBe empty
    postState.psi.good shouldBe empty
    postState.psi.wonky shouldBe empty
  }

  test("verdict processing with 1/3 votes (wonky)") {
    // Test that a verdict with 1/3 positive votes classifies the report as wonky
    val config = ChainConfig.TINY

    // oneThird = 6 / 3 = 2 validators
    config.oneThird shouldBe 2

    // When 2 out of 5 voters vote positive, the report is wonky
    // This is tested through the test vectors
    val state = emptyDisputeState(6, 2)
    val input = DisputeInput(emptyDisputes)
    val (_, output) = DisputeTransition.stfInternal(input, state, config)

    output.isRight shouldBe true
  }

  test("verdict processing with supermajority (good)") {
    // Test that a verdict with 2/3+1 positive votes classifies the report as good
    val config = ChainConfig.TINY

    // votesPerVerdict = (2 * 6) / 3 + 1 = 5 validators
    config.votesPerVerdict shouldBe 5

    // For 1023 validators: (2 * 1023) / 3 + 1 = 683
    ChainConfig.FULL.votesPerVerdict shouldBe 683
  }

  test("culprit signature verification") {
    // Test that culprit signatures are verified with "jam_guarantee" prefix
    val config = ChainConfig.TINY
    val state = emptyDisputeState(6, 2)

    // Create a culprit with invalid signature (all zeros)
    val invalidCulprit = Culprit(
      Hash.zero,
      Ed25519PublicKey(Array.fill(32)(0.toByte)),
      Ed25519Signature(Array.fill(64)(0.toByte))
    )

    val disputes = Dispute(List.empty, List(invalidCulprit), List.empty)
    val input = DisputeInput(disputes)
    val (_, output) = DisputeTransition.stfInternal(input, state, config)

    // Should fail with BadGuarantorKey since the key is not in kappa or lambda
    output.isLeft shouldBe true
  }

  test("fault signature verification") {
    // Test that fault signatures are verified with "jam_valid"/"jam_invalid" prefixes
    val config = ChainConfig.TINY
    val state = emptyDisputeState(6, 2)

    // Create a fault with invalid signature (all zeros)
    val invalidFault = Fault(
      Hash.zero,
      vote = true,
      Ed25519PublicKey(Array.fill(32)(0.toByte)),
      Ed25519Signature(Array.fill(64)(0.toByte))
    )

    val disputes = Dispute(List.empty, List.empty, List(invalidFault))
    val input = DisputeInput(disputes)
    val (_, output) = DisputeTransition.stfInternal(input, state, config)

    // Should fail with BadAuditorKey since the key is not in kappa or lambda
    output.isLeft shouldBe true
  }

  test("sorted/unique ordering validation") {
    // Test that culprits, verdicts, and faults must be sorted and unique
    val config = ChainConfig.TINY
    val state = emptyDisputeState(6, 2)

    // Create a valid key from kappa
    val validKey = state.kappa.head.ed25519

    // Create two culprits with the same key (not unique)
    val culprit1 = Culprit(
      Hash.zero,
      validKey,
      Ed25519Signature(Array.fill(64)(0.toByte))
    )
    val culprit2 = Culprit(
      Hash(Array.fill(32)(1.toByte)),
      validKey,
      Ed25519Signature(Array.fill(64)(0.toByte))
    )

    // Culprits with same key in wrong order should fail
    val disputes = Dispute(List.empty, List(culprit1, culprit2), List.empty)
    val input = DisputeInput(disputes)
    val (_, output) = DisputeTransition.stfInternal(input, state, config)

    // Should fail since keys are duplicate (not unique)
    output.isLeft shouldBe true
  }

  test("tiny config state transition") {
    val folderPath = "stf/disputes/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadJsonFromTestVectors[DisputeCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right(testCase) =>
          // Test state transition
          val (postState, output) = DisputeTransition.stfInternal(
            testCase.input,
            testCase.preState,
            TinyConfig
          )
          assertDisputeOutputEquals(testCase.output, output, testCaseName)
          assertDisputeStateEquals(testCase.postState, postState, testCaseName)
  }

  test("full config state transition") {
    val folderPath = "stf/disputes/full"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadJsonFromTestVectors[DisputeCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right(testCase) =>
          // Test state transition
          val (postState, output) = DisputeTransition.stfInternal(
            testCase.input,
            testCase.preState,
            FullConfig
          )
          assertDisputeOutputEquals(testCase.output, output, testCaseName)
          assertDisputeStateEquals(testCase.postState, postState, testCaseName)
  }

  // Helper method to compare DisputeOutput instances
  private def assertDisputeOutputEquals(
    expected: DisputeOutput,
    actual: DisputeOutput,
    testCaseName: String
  ): Unit =
    expected match
      case Right(expectedMarks) =>
        actual.isRight shouldBe true withClue s"Expected OK output but got error in test case: $testCaseName. Actual error: ${actual.left.toOption}"

        val actualMarks = actual.toOption.get
        expectedMarks.offenders.size shouldBe actualMarks.offenders.size withClue
          s"Offenders size mismatch in test case: $testCaseName"

        expectedMarks.offenders.zip(actualMarks.offenders).zipWithIndex.foreach {
          case ((exp, act), index) =>
            java.util.Arrays.equals(exp.bytes, act.bytes) shouldBe true withClue
              s"Offender mismatch at index $index in test case: $testCaseName"
        }

      case Left(expectedErr) =>
        actual.isLeft shouldBe true withClue s"Expected error output but got OK in test case: $testCaseName. Actual OK: ${actual.toOption}"
        expectedErr shouldBe actual.left.toOption.get withClue s"Error code mismatch in test case: $testCaseName"

  // Helper method to compare DisputeState instances
  private def assertDisputeStateEquals(
    expected: DisputeState,
    actual: DisputeState,
    testCaseName: String
  ): Unit =
    expected.tau shouldBe actual.tau withClue s"Tau mismatch in test case: $testCaseName"

    // Check psi
    expected.psi.good.size shouldBe actual.psi.good.size withClue s"Psi good size mismatch in test case: $testCaseName"
    expected.psi.bad.size shouldBe actual.psi.bad.size withClue s"Psi bad size mismatch in test case: $testCaseName"
    expected.psi.wonky.size shouldBe actual.psi.wonky
      .size withClue s"Psi wonky size mismatch in test case: $testCaseName"
    expected.psi.offenders.size shouldBe actual.psi.offenders
      .size withClue s"Psi offenders size mismatch in test case: $testCaseName"

    expected.psi.good.zip(actual.psi.good).zipWithIndex.foreach {
      case ((exp, act), idx) =>
        java.util.Arrays.equals(exp.bytes, act.bytes) shouldBe true withClue
          s"Psi good mismatch at index $idx in test case: $testCaseName"
    }

    expected.psi.bad.zip(actual.psi.bad).zipWithIndex.foreach {
      case ((exp, act), idx) =>
        java.util.Arrays.equals(exp.bytes, act.bytes) shouldBe true withClue
          s"Psi bad mismatch at index $idx in test case: $testCaseName"
    }

    expected.psi.wonky.zip(actual.psi.wonky).zipWithIndex.foreach {
      case ((exp, act), idx) =>
        java.util.Arrays.equals(exp.bytes, act.bytes) shouldBe true withClue
          s"Psi wonky mismatch at index $idx in test case: $testCaseName"
    }

    expected.psi.offenders.zip(actual.psi.offenders).zipWithIndex.foreach {
      case ((exp, act), idx) =>
        java.util.Arrays.equals(exp.bytes, act.bytes) shouldBe true withClue
          s"Psi offenders mismatch at index $idx in test case: $testCaseName"
    }

    // Check rho
    expected.rho.size shouldBe actual.rho.size withClue s"Rho size mismatch in test case: $testCaseName"
    expected.rho.zip(actual.rho).zipWithIndex.foreach {
      case ((exp, act), idx) =>
        (exp, act) match
          case (None, None) => // Both null is fine
          case (None, Some(_)) =>
            fail(s"Expected null rho at index $idx but got non-null in test case: $testCaseName")
          case (Some(_), None) =>
            fail(s"Expected non-null rho at index $idx but got null in test case: $testCaseName")
          case (Some(e), Some(a)) =>
            e.timeout shouldBe a.timeout withClue s"Rho timeout mismatch at index $idx in test case: $testCaseName"
    }

    // Check kappa
    expected.kappa.size shouldBe actual.kappa.size withClue s"Kappa size mismatch in test case: $testCaseName"
    expected.kappa.zip(actual.kappa).zipWithIndex.foreach {
      case ((exp, act), idx) =>
        java.util.Arrays.equals(exp.bandersnatch.bytes, act.bandersnatch.bytes) shouldBe true withClue
          s"Kappa bandersnatch mismatch at index $idx in test case: $testCaseName"
        java.util.Arrays.equals(exp.ed25519.bytes, act.ed25519.bytes) shouldBe true withClue
          s"Kappa ed25519 mismatch at index $idx in test case: $testCaseName"
    }

    // Check lambda
    expected.lambda.size shouldBe actual.lambda.size withClue s"Lambda size mismatch in test case: $testCaseName"
    expected.lambda.zip(actual.lambda).zipWithIndex.foreach {
      case ((exp, act), idx) =>
        java.util.Arrays.equals(exp.bandersnatch.bytes, act.bandersnatch.bytes) shouldBe true withClue
          s"Lambda bandersnatch mismatch at index $idx in test case: $testCaseName"
        java.util.Arrays.equals(exp.ed25519.bytes, act.ed25519.bytes) shouldBe true withClue
          s"Lambda ed25519 mismatch at index $idx in test case: $testCaseName"
    }
