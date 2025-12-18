package io.forge.jam.protocol.assurance

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.scodec.JamCodecs.encode
import _root_.scodec.Codec
import io.forge.jam.core.primitives.{
  Hash,
  BandersnatchPublicKey,
  Ed25519PublicKey,
  BlsPublicKey,
  ValidatorIndex,
  Ed25519Signature,
  Timeslot,
  CoreIndex,
  Gas
}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.core.types.work.PackageSpec
import io.forge.jam.core.types.workpackage.{WorkReport, AvailabilityAssignment}
import io.forge.jam.core.types.context.Context
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.assurance.AssuranceTypes.*
import io.forge.jam.protocol.assurance.AssuranceTransition
import spire.math.{UInt, UShort}

/**
 * Tests for the Assurances State Transition Function.
 */
class AssuranceTest extends AnyFunSuite with Matchers:

  val TinyConfig = ChainConfig.TINY
  val FullConfig = ChainConfig.FULL

  private val validEd25519Keys: Seq[Array[Byte]] = Seq(
    hexToBytes("c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a"),
    hexToBytes("26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05"),
    hexToBytes("26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85"),
    hexToBytes("ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"),
    hexToBytes("c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa"),
    hexToBytes("0100000000000000000000000000000000000000000000000000000000000000")
  ).map(ensureLength32)

  private def ensureLength32(arr: Array[Byte]): Array[Byte] =
    if arr.length >= 32 then arr.take(32)
    else Array.fill(32 - arr.length)(0.toByte) ++ arr

  private def hexToBytes(hex: String): Array[Byte] =
    hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  private def invalidButCanonicalSignature: Ed25519Signature =
    val r = hexToBytes("26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05")
    val s = Array.fill(32)(0x01.toByte)
    Ed25519Signature(r ++ s)

  private def validatorKeyFilled(value: Int): ValidatorKey =
    val ed25519Key = if value < validEd25519Keys.size then validEd25519Keys(value)
    else validEd25519Keys(value % validEd25519Keys.size)
    ValidatorKey(
      BandersnatchPublicKey(Array.fill(32)(value.toByte)),
      Ed25519PublicKey(ed25519Key),
      BlsPublicKey(Array.fill(144)(value.toByte)),
      JamBytes(Array.fill(128)(value.toByte))
    )

  private def simpleWorkReport(coreIndex: Int): WorkReport =
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

  private def initialState(validatorCount: Int, coreCount: Int, timeout: Long): AssuranceState =
    val assignments = (0 until coreCount).map { coreIndex =>
      Some(AvailabilityAssignment(simpleWorkReport(coreIndex), timeout))
    }.toList
    AssuranceState(
      availAssignments = assignments,
      currValidators = (0 until validatorCount).map(i => validatorKeyFilled(i)).toList
    )

  test("assurance bitfield processing") {
    val state = initialState(validatorCount = 4, coreCount = 4, timeout = 20L)
    val assurance = AssuranceExtrinsic(
      Hash.zero,
      JamBytes(Array(0x05.toByte)), // cores 0 and 2
      ValidatorIndex(0),
      invalidButCanonicalSignature
    )

    val input = AssuranceInput(
      assurances = List(assurance),
      slot = 5,
      parent = Hash.zero
    )

    val config = TinyConfig.copy(validatorCount = 4, coresCount = 4)
    val (_, output) = AssuranceTransition.stfInternal(input, state, config)
    output.left.toOption shouldBe Some(AssuranceErrorCode.BadSignature)
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
    val (postState, output) = AssuranceTransition.stfInternal(input, state, config)

    // First assignment should be cleared (5 + 5 = 10 <= 12)
    postState.availAssignments(0) shouldBe None
    // Second assignment should remain (10 + 5 = 15 > 12)
    postState.availAssignments(1) shouldBe defined
    output.isRight shouldBe true
  }

  test("Ed25519 signature verification") {
    // Test that signature verification rejects invalid signatures
    val state = initialState(validatorCount = 4, coreCount = 2, timeout = 20L)

    // Create assurance with invalid signature
    val assurance = AssuranceExtrinsic(
      Hash.zero,
      JamBytes(Array(0x03.toByte)), // both cores
      ValidatorIndex(0),
      invalidButCanonicalSignature
    )

    val input = AssuranceInput(
      assurances = List(assurance),
      slot = 5,
      parent = Hash.zero
    )

    val config = TinyConfig.copy(validatorCount = 4, coresCount = 2)
    val (_, output) = AssuranceTransition.stfInternal(input, state, config)

    output.left.toOption shouldBe Some(AssuranceErrorCode.BadSignature)
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
          given Codec[AssuranceCase] = AssuranceCase.codec(TinyConfig.coresCount, TinyConfig.validatorCount)
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"

          // Test state transition
          val (postState, output) = AssuranceTransition.stfInternal(
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
          given Codec[AssuranceCase] = AssuranceCase.codec(FullConfig.coresCount, FullConfig.validatorCount)
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"

          // Test state transition
          val (postState, output) = AssuranceTransition.stfInternal(
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
    expected match
      case Right(expectedMarks) =>
        actual.isRight shouldBe true withClue s"Expected OK output but got error in test case: $testCaseName. Actual: $actual"

        val actualMarks = actual.toOption.get
        expectedMarks.reported.size shouldBe actualMarks.reported.size withClue
          s"Reported work reports size mismatch in test case: $testCaseName"

        expectedMarks.reported.zip(actualMarks.reported).zipWithIndex.foreach {
          case ((exp, act), index) =>
            exp shouldBe act withClue s"Work report mismatch at index $index in test case: $testCaseName"
        }

      case Left(expectedErr) =>
        actual.isLeft shouldBe true withClue s"Expected error output but got OK in test case: $testCaseName. Actual: $actual"
        expectedErr shouldBe actual.left.toOption.get withClue s"Error code mismatch in test case: $testCaseName"

  // Helper method to compare AssuranceState instances
  private def assertAssuranceStateEquals(
    expected: AssuranceState,
    actual: AssuranceState,
    testCaseName: String
  ): Unit =
    expected.availAssignments.size shouldBe actual.availAssignments.size withClue
      s"Availability assignments size mismatch in test case: $testCaseName"

    expected.availAssignments.zip(actual.availAssignments).zipWithIndex.foreach {
      case ((exp, act), index) =>
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

    expected.currValidators.zip(actual.currValidators).zipWithIndex.foreach {
      case ((exp, act), index) =>
        exp.bandersnatch shouldBe act.bandersnatch withClue
          s"Bandersnatch key mismatch at validator $index in test case: $testCaseName"
        exp.ed25519 shouldBe act.ed25519 withClue
          s"Ed25519 key mismatch at validator $index in test case: $testCaseName"
        exp.bls shouldBe act.bls withClue
          s"BLS key mismatch at validator $index in test case: $testCaseName"
        exp.metadata shouldBe act.metadata withClue
          s"Metadata mismatch at validator $index in test case: $testCaseName"
    }
