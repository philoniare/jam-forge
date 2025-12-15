package io.forge.jam.protocol.history

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.JamBytes
import io.forge.jam.core.codec.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.TestHelpers.hashFilled
import io.forge.jam.protocol.history.HistoryTypes.*
import io.forge.jam.protocol.history.HistoryTransition

/**
 * Tests for the History State Transition Function.
 */
class HistoryTest extends AnyFunSuite with Matchers:

  test("MMR append with single peak") {
    // When we append to an empty MMR, we should get a single peak at index 0
    val emptyMmr = HistoricalMmr(List.empty)
    val peak = hashFilled(1)

    val result = HistoryTransition.appendToMmr(emptyMmr, peak)

    result.peaks should have size 1
    result.peaks.head shouldBe Some(peak)
  }

  test("MMR merge when peaks combine") {
    // When we have a peak at index 0 and add another, they should merge
    // creating a single peak at index 1
    val firstPeak = hashFilled(1)
    val mmrWithOnePeak = HistoricalMmr(List(Some(firstPeak)))
    val secondPeak = hashFilled(2)

    val result = HistoryTransition.appendToMmr(mmrWithOnePeak, secondPeak)

    // After merging, index 0 should be None and index 1 should have the merged peak
    result.peaks should have size 2
    result.peaks(0) shouldBe None
    result.peaks(1) shouldBe defined

    // Adding a third peak should fill index 0 again
    val thirdPeak = hashFilled(3)
    val result2 = HistoryTransition.appendToMmr(result, thirdPeak)

    result2.peaks should have size 2
    result2.peaks(0) shouldBe Some(thirdPeak)
    result2.peaks(1) shouldBe defined
  }

  test("beefy root calculation") {
    // Test beefy root calculation with known values
    val singlePeakMmr = HistoricalMmr(List(Some(hashFilled(1))))
    val beefyRoot = HistoryTransition.calculateBeefyRoot(singlePeakMmr)

    // With a single peak, beefy root should equal that peak
    beefyRoot shouldBe hashFilled(1)

    // Empty MMR should return zero hash
    val emptyMmr = HistoricalMmr(List.empty)
    val emptyBeefyRoot = HistoryTransition.calculateBeefyRoot(emptyMmr)
    emptyBeefyRoot shouldBe Hash.zero
  }

  test("history limit enforcement (8 blocks)") {
    // Create a state with 8 history entries
    val initialHistory = (1 to 8).map { i =>
      HistoricalBeta(
        headerHash = hashFilled(i),
        beefyRoot = hashFilled(i + 10),
        stateRoot = hashFilled(i + 20),
        reported = List.empty
      )
    }.toList

    val preState = HistoricalState(
      beta = HistoricalBetaContainer(
        history = initialHistory,
        mmr = HistoricalMmr(List(Some(hashFilled(100))))
      )
    )

    // Add a 9th block - should evict the first one
    val input = HistoricalInput(
      headerHash = hashFilled(9),
      parentStateRoot = hashFilled(99),
      accumulateRoot = hashFilled(199),
      workPackages = List.empty
    )

    val postState = HistoryTransition.stfInternal(input, preState)

    // History should still have 8 entries
    postState.beta.history should have size 8

    // First entry should now be what was entry 2 (headerHash = hashFilled(2))
    postState.beta.history.head.headerHash shouldBe hashFilled(2)

    // Last entry should be the new block
    postState.beta.history.last.headerHash shouldBe hashFilled(9)
  }

  test("tiny config state transition") {
    val folderPath = "stf/history/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadTestDataFromTestVectors[HistoricalCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right((testCase, expectedBinaryData)) =>
          // Test encoding
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"

          // Test state transition
          val postState = HistoryTransition.stfInternal(testCase.input, testCase.preState)
          assertHistoryStateEquals(testCase.postState, postState, testCaseName)
  }

  test("full config state transition") {
    val folderPath = "stf/history/full"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadTestDataFromTestVectors[HistoricalCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right((testCase, expectedBinaryData)) =>
          // Test encoding
          val encoded = testCase.encode
          encoded.toArray shouldBe expectedBinaryData withClue s"Encoding mismatch for $testCaseName"

          // Test state transition
          val postState = HistoryTransition.stfInternal(testCase.input, testCase.preState)
          assertHistoryStateEquals(testCase.postState, postState, testCaseName)
  }

  // Helper method to compare HistoricalState instances with detailed error messages
  private def assertHistoryStateEquals(
    expected: HistoricalState,
    actual: HistoricalState,
    testCaseName: String
  ): Unit =
    // Compare history list size
    expected.beta.history.size shouldBe actual.beta.history.size withClue
      s"$testCaseName: history list size mismatch"

    // Compare each history entry
    for (i <- expected.beta.history.indices) do
      assertHistoricalBetaEquals(
        expected.beta.history(i),
        actual.beta.history(i),
        s"$testCaseName: beta.history[$i]"
      )

    // Compare MMR
    assertMmrEquals(expected.beta.mmr, actual.beta.mmr, s"$testCaseName: beta.mmr")

  private def assertHistoricalBetaEquals(
    expected: HistoricalBeta,
    actual: HistoricalBeta,
    path: String
  ): Unit =
    expected.headerHash shouldBe actual.headerHash withClue s"$path: headerHash mismatch"
    expected.beefyRoot shouldBe actual.beefyRoot withClue s"$path: beefyRoot mismatch"
    expected.stateRoot shouldBe actual.stateRoot withClue s"$path: stateRoot mismatch"
    expected.reported.size shouldBe actual.reported.size withClue s"$path: reported size mismatch"

    for (i <- expected.reported.indices) do
      expected.reported(i).hash shouldBe actual.reported(i).hash withClue
        s"$path: reported[$i].hash mismatch"
      expected.reported(i).exportsRoot shouldBe actual.reported(i).exportsRoot withClue
        s"$path: reported[$i].exportsRoot mismatch"

  private def assertMmrEquals(
    expected: HistoricalMmr,
    actual: HistoricalMmr,
    path: String
  ): Unit =
    expected.peaks.size shouldBe actual.peaks.size withClue s"$path: peaks size mismatch"

    for (i <- expected.peaks.indices) do
      expected.peaks(i) shouldBe actual.peaks(i) withClue s"$path: peaks[$i] mismatch"
