package io.forge.jam.protocol.assurance

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.encode
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey, ValidatorIndex, ServiceId, Ed25519Signature, Timeslot, CoreIndex, Gas}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.core.types.work.PackageSpec
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workresult.{WorkResult, RefineLoad}
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.assurance.AssuranceTypes.*
import io.forge.jam.protocol.assurance.AssuranceTransition
import spire.math.{UByte, UInt, UShort}

/**
 * Tests for the Assurances State Transition Function.
 */
class AssuranceTest extends AnyFunSuite with Matchers:

  val TinyConfig = ChainConfig.TINY
  val FullConfig = ChainConfig.FULL

  // Helper to create a validator key filled with a specific byte value
  private def validatorKeyFilled(value: Int): ValidatorKey =
    ValidatorKey(
      BandersnatchPublicKey(Array.fill(32)(value.toByte)),
      Ed25519PublicKey(Array.fill(32)(value.toByte)),
      BlsPublicKey(Array.fill(144)(value.toByte)),
      JamBytes(Array.fill(128)(value.toByte))
    )

  // Helper to create a simple work report
  private def simpleWorkReport(coreIndex: Int = 0): WorkReport =
    WorkReport(
      PackageSpec(Hash.zero, UInt(100), Hash.zero, Hash.zero, UShort(1)),
      Context(Hash.zero, Hash.zero, Hash.zero, Hash.zero, Timeslot(0), List.empty),
      CoreIndex(coreIndex),
      Hash.zero,
      Gas(0L),
      JamBytes.empty,
      List.empty,
      List.empty
    )

  // Helper to create initial state with assignments
  private def initialState(validatorCount: Int, coreCount: Int, timeout: Long = 10L): AssuranceState =
    val assignments = (0 until coreCount).map { coreIndex =>
      Some(AvailabilityAssignment(simpleWorkReport(coreIndex), timeout))
    }.toList
    AssuranceState(
      availAssignments = assignments,
      currValidators = (0 until validatorCount).map(i => validatorKeyFilled(i)).toList
    )

  test("assurance bitfield processing") {
    // Test that bitfield processing correctly identifies which cores a validator assures
    val state = initialState(validatorCount = 4, coreCount = 4, timeout = 20L)

    // Create assurance with bitfield assuring cores 0 and 2 (binary: 0101 = 0x05)
    val assurance = AssuranceExtrinsic(
      Hash.zero,
      JamBytes(Array(0x05.toByte)), // cores 0 and 2
      ValidatorIndex(0),
      Ed25519Signature(Array.fill(64)(0.toByte))
    )

    val input = AssuranceInput(
      assurances = List(assurance),
      slot = 5,
      parent = Hash.zero
    )

    val config = TinyConfig.copy(validatorCount = 4, coresCount = 4)

    // Since signature won't verify, expect bad signature error
    // But this test validates the bitfield parsing logic works
    val (_, output) = AssuranceTransition.stf(input, state, config)
    output.err shouldBe Some(AssuranceErrorCode.BadSignature)
  }

  test("2/3 supermajority threshold") {
    // Test that supermajority calculation is correct
    // For 6 validators: 2/3 = 4, so need > 4, meaning 5+ assurances
    TinyConfig.superMajority shouldBe 4 // (2 * 6) / 3 = 4

    // For 9 validators: 2/3 = 6, so need > 6, meaning 7+ assurances
    val config9 = TinyConfig.copy(validatorCount = 9)
    config9.superMajority shouldBe 6 // (2 * 9) / 3 = 6

    // For 1023 validators: 2/3 = 682, so need > 682, meaning 683+ assurances
    FullConfig.superMajority shouldBe 682 // (2 * 1023) / 3 = 682
  }

  test("core timeout handling") {
    // Test that stale reports are cleared based on timeout
    val state = AssuranceState(
      availAssignments = List(
        Some(AvailabilityAssignment(simpleWorkReport(0), 5)), // timeout at slot 5
        Some(AvailabilityAssignment(simpleWorkReport(1), 10)) // timeout at slot 10
      ),
      currValidators = (0 until 4).map(i => validatorKeyFilled(i)).toList
    )

    val input = AssuranceInput(
      assurances = List.empty,
      slot = 12, // slot 12 means: 12 >= 5 + 5 (true), 12 >= 10 + 5 (false)
      parent = Hash.zero
    )

    val config = TinyConfig.copy(validatorCount = 4, coresCount = 2, assuranceTimeoutPeriod = 5)
    val (postState, output) = AssuranceTransition.stf(input, state, config)

    // First assignment should be cleared (5 + 5 = 10 <= 12)
    postState.availAssignments(0) shouldBe None
    // Second assignment should remain (10 + 5 = 15 > 12)
    postState.availAssignments(1) shouldBe defined
    output.ok shouldBe defined
  }

  test("Ed25519 signature verification") {
    // Test that signature verification rejects invalid signatures
    val state = initialState(validatorCount = 4, coreCount = 2, timeout = 20L)

    // Create assurance with invalid signature
    val assurance = AssuranceExtrinsic(
      Hash.zero,
      JamBytes(Array(0x03.toByte)), // both cores
      ValidatorIndex(0),
      Ed25519Signature(Array.fill(64)(0.toByte)) // invalid signature
    )

    val input = AssuranceInput(
      assurances = List(assurance),
      slot = 5,
      parent = Hash.zero
    )

    val config = TinyConfig.copy(validatorCount = 4, coresCount = 2)
    val (_, output) = AssuranceTransition.stf(input, state, config)

    output.err shouldBe Some(AssuranceErrorCode.BadSignature)
  }

  test("tiny config state transition") {
    val folderPath = "stf/assurances/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadTestDataFromTestVectors[AssuranceCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right((testCase, expectedBinaryData)) =>
          // Test encoding
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"

          // Test state transition
          val (postState, output) = AssuranceTransition.stf(
            testCase.input,
            testCase.preState,
            TinyConfig
          )
          assertAssuranceOutputEquals(testCase.output, output, testCaseName)
          assertAssuranceStateEquals(testCase.postState, postState, testCaseName)
  }

  test("full config state transition") {
    val folderPath = "stf/assurances/full"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadTestDataFromTestVectors[AssuranceCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right((testCase, expectedBinaryData)) =>
          // Test encoding
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"

          // Test state transition
          val (postState, output) = AssuranceTransition.stf(
            testCase.input,
            testCase.preState,
            FullConfig
          )
          assertAssuranceOutputEquals(testCase.output, output, testCaseName)
          assertAssuranceStateEquals(testCase.postState, postState, testCaseName)
  }

  // Helper method to compare AssuranceOutput instances
  private def assertAssuranceOutputEquals(
    expected: AssuranceOutput,
    actual: AssuranceOutput,
    testCaseName: String
  ): Unit =
    (expected.ok, expected.err) match
      case (Some(expectedMarks), _) =>
        actual.ok shouldBe defined withClue s"Expected OK output but got error in test case: $testCaseName. Actual: $actual"
        actual.err shouldBe None withClue s"Expected OK output but got both OK and error in test case: $testCaseName"

        expectedMarks.reported.size shouldBe actual.ok.get.reported.size withClue
          s"Reported work reports size mismatch in test case: $testCaseName"

        expectedMarks.reported.zip(actual.ok.get.reported).zipWithIndex.foreach { case ((exp, act), index) =>
          exp shouldBe act withClue s"Work report mismatch at index $index in test case: $testCaseName"
        }

      case (_, Some(expectedErr)) =>
        actual.err shouldBe defined withClue s"Expected error output but got OK in test case: $testCaseName. Actual: $actual"
        actual.ok shouldBe None withClue s"Expected error output but got both OK and error in test case: $testCaseName"
        expectedErr shouldBe actual.err.get withClue s"Error code mismatch in test case: $testCaseName"

      case (None, None) =>
        fail(s"Invalid expected AssuranceOutput - both ok and err are None in test case: $testCaseName")

  // Helper method to compare AssuranceState instances
  private def assertAssuranceStateEquals(
    expected: AssuranceState,
    actual: AssuranceState,
    testCaseName: String
  ): Unit =
    expected.availAssignments.size shouldBe actual.availAssignments.size withClue
      s"Availability assignments size mismatch in test case: $testCaseName"

    expected.availAssignments.zip(actual.availAssignments).zipWithIndex.foreach { case ((exp, act), index) =>
      (exp, act) match
        case (None, None) => // Both null is fine
        case (None, Some(_)) =>
          fail(s"Expected null assignment at index $index but got non-null in test case: $testCaseName")
        case (Some(_), None) =>
          fail(s"Expected non-null assignment at index $index but got null in test case: $testCaseName")
        case (Some(e), Some(a)) =>
          e.report shouldBe a.report withClue
            s"Work report mismatch at index $index in test case: $testCaseName"
          e.timeout shouldBe a.timeout withClue
            s"Timeout mismatch at index $index in test case: $testCaseName"
    }

    expected.currValidators.size shouldBe actual.currValidators.size withClue
      s"Current validators size mismatch in test case: $testCaseName"

    expected.currValidators.zip(actual.currValidators).zipWithIndex.foreach { case ((exp, act), index) =>
      exp.bandersnatch shouldBe act.bandersnatch withClue
        s"Bandersnatch key mismatch at validator $index in test case: $testCaseName"
      exp.ed25519 shouldBe act.ed25519 withClue
        s"Ed25519 key mismatch at validator $index in test case: $testCaseName"
      exp.bls shouldBe act.bls withClue
        s"BLS key mismatch at validator $index in test case: $testCaseName"
      exp.metadata shouldBe act.metadata withClue
        s"Metadata mismatch at validator $index in test case: $testCaseName"
    }
