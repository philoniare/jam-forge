package io.forge.jam.protocol.safrole

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.encode
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey}
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.safrole.SafroleTypes.*

/**
 * Tests for the Safrole State Transition Function.
 */
class SafroleTest extends AnyFunSuite with Matchers:

  val TinyConfig: ChainConfig = ChainConfig.TINY
  val FullConfig: ChainConfig = ChainConfig.FULL

  test("JSON test vector parsing for tiny config") {
    val folderPath = "stf/safrole/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true withClue s"Failed to list tiny test vectors: ${testCaseNamesResult.left.getOrElse("")}"

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty withClue "Should have test cases in tiny folder"

    // Parse first test case to verify JSON parsing works
    val testCaseName = testCaseNames.head
    val testDataResult = TestFileLoader.loadJsonFromTestVectors[SafroleCase](folderPath, testCaseName)

    testDataResult match {
      case Left(error) =>
        fail(s"Failed to parse JSON test case $testCaseName: $error")
      case Right(testCase) =>
        // Verify structure matches TINY config
        testCase.preState.lambda.size shouldBe TinyConfig.validatorCount withClue "Lambda should have validatorCount validators"
        testCase.preState.kappa.size shouldBe TinyConfig.validatorCount withClue "Kappa should have validatorCount validators"
        testCase.preState.gammaK.size shouldBe TinyConfig.validatorCount withClue "GammaK should have validatorCount validators"
        testCase.preState.iota.size shouldBe TinyConfig.validatorCount withClue "Iota should have validatorCount validators"
        testCase.preState.eta.size shouldBe 4 withClue "Eta should have 4 entropy values"
        testCase.preState.gammaZ.length shouldBe 144 withClue "GammaZ should be 144 bytes"
    }
  }

  test("binary test vector parsing for tiny config") {
    val folderPath = "stf/safrole/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    // Load both JSON and binary for first test case
    val testCaseName = testCaseNames.head
    val testDataResult = TestFileLoader.loadTestDataFromTestVectors[SafroleCase](folderPath, testCaseName)

    testDataResult match {
      case Left(error) =>
        fail(s"Failed to load test data for $testCaseName: $error")
      case Right((testCase, binaryData)) =>
        // Verify binary data is not empty
        binaryData should not be empty withClue "Binary data should not be empty"
        // Verify binary data has reasonable size (at least state + input + output)
        binaryData.length should be > 1000 withClue "Binary data should have reasonable size for tiny config"
    }
  }

  test("JSON test vector parsing for full config") {
    val folderPath = "stf/safrole/full"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true withClue s"Failed to list full test vectors: ${testCaseNamesResult.left.getOrElse("")}"

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty withClue "Should have test cases in full folder"

    // Parse first test case to verify JSON parsing works
    val testCaseName = testCaseNames.head
    val testDataResult = TestFileLoader.loadJsonFromTestVectors[SafroleCase](folderPath, testCaseName)

    testDataResult match {
      case Left(error) =>
        fail(s"Failed to parse JSON test case $testCaseName: $error")
      case Right(testCase) =>
        // Verify structure matches FULL config
        testCase.preState.lambda.size shouldBe FullConfig.validatorCount withClue "Lambda should have validatorCount validators"
        testCase.preState.kappa.size shouldBe FullConfig.validatorCount withClue "Kappa should have validatorCount validators"
        testCase.preState.gammaK.size shouldBe FullConfig.validatorCount withClue "GammaK should have validatorCount validators"
        testCase.preState.iota.size shouldBe FullConfig.validatorCount withClue "Iota should have validatorCount validators"
        testCase.preState.eta.size shouldBe 4 withClue "Eta should have 4 entropy values"
        testCase.preState.gammaZ.length shouldBe 144 withClue "GammaZ should be 144 bytes"
    }
  }

  test("encoding roundtrip - encode decoded JSON matches binary") {
    val folderPath = "stf/safrole/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    // Test first few test cases for roundtrip
    for (testCaseName <- testCaseNames.take(3)) {
      val testDataResult = TestFileLoader.loadTestDataFromTestVectors[SafroleCase](folderPath, testCaseName)
      testDataResult match {
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right((testCase, expectedBinaryData)) =>
          // Encode the parsed JSON
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"
      }
    }
  }

  test("tiny config state transition - all test vectors") {
    val folderPath = "stf/safrole/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true withClue s"Failed to list tiny test vectors"

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    info(s"Running ${testCaseNames.size} tiny config test vectors")

    for (testCaseName <- testCaseNames) {
      val testDataResult = TestFileLoader.loadTestDataFromTestVectors[SafroleCase](folderPath, testCaseName)
      testDataResult match {
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right((testCase, expectedBinaryData)) =>
          // Test encoding matches expected binary
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"

          // Test state transition
          val (postState, output) = SafroleTransition.stfInternal(
            testCase.input,
            testCase.preState,
            TinyConfig
          )
          assertSafroleOutputEquals(testCase.output, output, testCaseName)
          assertSafroleStateEquals(testCase.postState, postState, testCaseName)
      }
    }
  }

  test("full config state transition - all test vectors") {
    val folderPath = "stf/safrole/full"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true withClue s"Failed to list full test vectors"

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    info(s"Running ${testCaseNames.size} full config test vectors")

    for (testCaseName <- testCaseNames) {
      val testDataResult = TestFileLoader.loadJsonFromTestVectors[SafroleCase](folderPath, testCaseName)
      testDataResult match {
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right(testCase) =>
          // Test state transition
          val (postState, output) = SafroleTransition.stfInternal(
            testCase.input,
            testCase.preState,
            FullConfig
          )
          assertSafroleOutputEquals(testCase.output, output, testCaseName)
          assertSafroleStateEquals(testCase.postState, postState, testCaseName)
      }
    }
  }

  /**
   * Compare SafroleOutput instances with detailed error messages.
   */
  private def assertSafroleOutputEquals(
    expected: SafroleOutput,
    actual: SafroleOutput,
    testCaseName: String
  ): Unit = {
    (expected.ok, expected.err) match {
      case (Some(expectedData), _) =>
        actual.ok shouldBe defined withClue s"Expected OK output but got error in test case: $testCaseName. Actual error: ${actual.err}"
        actual.err shouldBe None withClue s"Expected OK output but got both OK and error in test case: $testCaseName"

        val actualData = actual.ok.get

        // Compare epoch_mark
        (expectedData.epochMark, actualData.epochMark) match {
          case (Some(expMark), Some(actMark)) =>
            java.util.Arrays.equals(expMark.entropy.bytes, actMark.entropy.bytes) shouldBe true withClue
              s"Epoch mark entropy mismatch in test case: $testCaseName"
            java.util.Arrays.equals(expMark.ticketsEntropy.bytes, actMark.ticketsEntropy.bytes) shouldBe true withClue
              s"Epoch mark ticketsEntropy mismatch in test case: $testCaseName"
            expMark.validators.size shouldBe actMark.validators.size withClue
              s"Epoch mark validators size mismatch in test case: $testCaseName"
            expMark.validators.zip(actMark.validators).zipWithIndex.foreach { case ((exp, act), idx) =>
              java.util.Arrays.equals(exp.bandersnatch.bytes, act.bandersnatch.bytes) shouldBe true withClue
                s"Epoch mark validator bandersnatch mismatch at index $idx in test case: $testCaseName"
              java.util.Arrays.equals(exp.ed25519.bytes, act.ed25519.bytes) shouldBe true withClue
                s"Epoch mark validator ed25519 mismatch at index $idx in test case: $testCaseName"
            }
          case (None, None) => // Both None is fine
          case (Some(_), None) =>
            fail(s"Expected epoch_mark but got None in test case: $testCaseName")
          case (None, Some(_)) =>
            fail(s"Expected no epoch_mark but got Some in test case: $testCaseName")
        }

        // Compare tickets_mark
        (expectedData.ticketsMark, actualData.ticketsMark) match {
          case (Some(expTickets), Some(actTickets)) =>
            expTickets.size shouldBe actTickets.size withClue
              s"Tickets mark size mismatch in test case: $testCaseName"
            expTickets.zip(actTickets).zipWithIndex.foreach { case ((exp, act), idx) =>
              java.util.Arrays.equals(exp.id.toArray, act.id.toArray) shouldBe true withClue
                s"Tickets mark ID mismatch at index $idx in test case: $testCaseName"
              exp.attempt shouldBe act.attempt withClue
                s"Tickets mark attempt mismatch at index $idx in test case: $testCaseName"
            }
          case (None, None) => // Both None is fine
          case (Some(_), None) =>
            fail(s"Expected tickets_mark but got None in test case: $testCaseName")
          case (None, Some(_)) =>
            fail(s"Expected no tickets_mark but got Some in test case: $testCaseName")
        }

      case (_, Some(expectedErr)) =>
        actual.err shouldBe defined withClue s"Expected error output but got OK in test case: $testCaseName. Actual OK: ${actual.ok}"
        actual.ok shouldBe None withClue s"Expected error output but got both OK and error in test case: $testCaseName"
        expectedErr shouldBe actual.err.get withClue s"Error code mismatch in test case: $testCaseName. Expected: $expectedErr, Actual: ${actual.err.get}"

      case (None, None) =>
        fail(s"Invalid expected SafroleOutput - both ok and err are None in test case: $testCaseName")
    }
  }

  /**
   * Compare SafroleState instances with detailed error messages.
   */
  private def assertSafroleStateEquals(
    expected: SafroleState,
    actual: SafroleState,
    testCaseName: String
  ): Unit = {
    // Compare tau
    expected.tau shouldBe actual.tau withClue s"Tau mismatch in test case: $testCaseName"

    // Compare eta (4 entropy values)
    expected.eta.size shouldBe actual.eta.size withClue s"Eta size mismatch in test case: $testCaseName"
    expected.eta.zip(actual.eta).zipWithIndex.foreach { case ((exp, act), idx) =>
      java.util.Arrays.equals(exp.bytes, act.bytes) shouldBe true withClue
        s"Eta[$idx] mismatch in test case: $testCaseName. Expected: ${exp.bytes.take(8).map(b => f"$b%02x").mkString}, Actual: ${act.bytes.take(8).map(b => f"$b%02x").mkString}"
    }

    // Compare lambda (previous epoch validators)
    expected.lambda.size shouldBe actual.lambda.size withClue s"Lambda size mismatch in test case: $testCaseName"
    assertValidatorListEquals(expected.lambda, actual.lambda, "lambda", testCaseName)

    // Compare kappa (current epoch validators)
    expected.kappa.size shouldBe actual.kappa.size withClue s"Kappa size mismatch in test case: $testCaseName"
    assertValidatorListEquals(expected.kappa, actual.kappa, "kappa", testCaseName)

    // Compare gammaK (next epoch validators)
    expected.gammaK.size shouldBe actual.gammaK.size withClue s"GammaK size mismatch in test case: $testCaseName"
    assertValidatorListEquals(expected.gammaK, actual.gammaK, "gammaK", testCaseName)

    // Compare iota (scheduled validators)
    expected.iota.size shouldBe actual.iota.size withClue s"Iota size mismatch in test case: $testCaseName"
    assertValidatorListEquals(expected.iota, actual.iota, "iota", testCaseName)

    // Compare gammaA (ticket accumulator)
    expected.gammaA.size shouldBe actual.gammaA.size withClue
      s"GammaA (ticket accumulator) size mismatch in test case: $testCaseName. Expected: ${expected.gammaA.size}, Actual: ${actual.gammaA.size}"
    expected.gammaA.zip(actual.gammaA).zipWithIndex.foreach { case ((exp, act), idx) =>
      java.util.Arrays.equals(exp.id.toArray, act.id.toArray) shouldBe true withClue
        s"GammaA ticket ID mismatch at index $idx in test case: $testCaseName"
      exp.attempt shouldBe act.attempt withClue
        s"GammaA ticket attempt mismatch at index $idx in test case: $testCaseName"
    }

    // Compare gammaS (sealing sequence - TicketsOrKeys)
    (expected.gammaS, actual.gammaS) match {
      case (TicketsOrKeys.Tickets(expTickets), TicketsOrKeys.Tickets(actTickets)) =>
        expTickets.size shouldBe actTickets.size withClue
          s"GammaS tickets size mismatch in test case: $testCaseName"
        expTickets.zip(actTickets).zipWithIndex.foreach { case ((exp, act), idx) =>
          java.util.Arrays.equals(exp.id.toArray, act.id.toArray) shouldBe true withClue
            s"GammaS ticket ID mismatch at index $idx in test case: $testCaseName"
          exp.attempt shouldBe act.attempt withClue
            s"GammaS ticket attempt mismatch at index $idx in test case: $testCaseName"
        }
      case (TicketsOrKeys.Keys(expKeys), TicketsOrKeys.Keys(actKeys)) =>
        expKeys.size shouldBe actKeys.size withClue
          s"GammaS keys size mismatch in test case: $testCaseName"
        expKeys.zip(actKeys).zipWithIndex.foreach { case ((exp, act), idx) =>
          java.util.Arrays.equals(exp.bytes, act.bytes) shouldBe true withClue
            s"GammaS key mismatch at index $idx in test case: $testCaseName"
        }
      case (TicketsOrKeys.Tickets(_), TicketsOrKeys.Keys(_)) =>
        fail(s"GammaS type mismatch in test case: $testCaseName. Expected Tickets, got Keys")
      case (TicketsOrKeys.Keys(_), TicketsOrKeys.Tickets(_)) =>
        fail(s"GammaS type mismatch in test case: $testCaseName. Expected Keys, got Tickets")
    }

    // Compare gammaZ (ring commitment)
    java.util.Arrays.equals(expected.gammaZ.toArray, actual.gammaZ.toArray) shouldBe true withClue
      s"GammaZ (ring commitment) mismatch in test case: $testCaseName"

    // Compare postOffenders
    expected.postOffenders.size shouldBe actual.postOffenders.size withClue
      s"PostOffenders size mismatch in test case: $testCaseName"
    expected.postOffenders.zip(actual.postOffenders).zipWithIndex.foreach { case ((exp, act), idx) =>
      java.util.Arrays.equals(exp.bytes, act.bytes) shouldBe true withClue
        s"PostOffenders mismatch at index $idx in test case: $testCaseName"
    }
  }

  /**
   * Compare validator key lists with detailed error messages.
   */
  private def assertValidatorListEquals(
    expected: List[io.forge.jam.core.types.epoch.ValidatorKey],
    actual: List[io.forge.jam.core.types.epoch.ValidatorKey],
    fieldName: String,
    testCaseName: String
  ): Unit = {
    expected.zip(actual).zipWithIndex.foreach { case ((exp, act), idx) =>
      java.util.Arrays.equals(exp.bandersnatch.bytes, act.bandersnatch.bytes) shouldBe true withClue
        s"$fieldName bandersnatch mismatch at index $idx in test case: $testCaseName"
      java.util.Arrays.equals(exp.ed25519.bytes, act.ed25519.bytes) shouldBe true withClue
        s"$fieldName ed25519 mismatch at index $idx in test case: $testCaseName"
      java.util.Arrays.equals(exp.bls.bytes, act.bls.bytes) shouldBe true withClue
        s"$fieldName bls mismatch at index $idx in test case: $testCaseName"
      java.util.Arrays.equals(exp.metadata.toArray, act.metadata.toArray) shouldBe true withClue
        s"$fieldName metadata mismatch at index $idx in test case: $testCaseName"
    }
  }
