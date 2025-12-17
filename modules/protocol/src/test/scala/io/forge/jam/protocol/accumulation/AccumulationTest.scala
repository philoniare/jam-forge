package io.forge.jam.protocol.accumulation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.scodec.JamCodecs.encode
import _root_.scodec.Codec
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.pvm.engine.PvmTraceWriter

class AccumulationTest extends AnyFunSuite with Matchers:

  val TinyConfig: ChainConfig = ChainConfig.TINY
  val FullConfig: ChainConfig = ChainConfig.FULL

  test("JSON test vector parsing for tiny config") {
    val folderPath = "stf/accumulate/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult
      .isRight shouldBe true withClue s"Failed to list tiny test vectors: ${testCaseNamesResult.left.getOrElse("")}"

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty withClue "Should have test cases in tiny folder"

    info(s"Found ${testCaseNames.size} test vectors")

    // Parse first test case to verify JSON parsing works
    val testCaseName = testCaseNames.head
    val testDataResult = TestFileLoader.loadJsonFromTestVectors[AccumulationCase](folderPath, testCaseName)

    testDataResult match
      case Left(error) =>
        fail(s"Failed to parse JSON test case $testCaseName: $error")
      case Right(testCase) =>
        // Verify structure matches TINY config
        testCase.preState.readyQueue.size shouldBe TinyConfig.epochLength withClue "Ready queue should have epoch length"
        testCase.preState.accumulated.size shouldBe TinyConfig.epochLength withClue "Accumulated should have epoch length"
        testCase.preState.privileges.assign.size shouldBe TinyConfig.coresCount withClue "Assigners should have cores count"
        testCase.preState.entropy.length shouldBe 32 withClue "Entropy should be 32 bytes"
  }

  test("binary test vector parsing for tiny config") {
    val folderPath = "stf/accumulate/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    // Load both JSON and binary for first test case
    val testCaseName = testCaseNames.head
    val testDataResult = TestFileLoader.loadTestDataFromTestVectors[AccumulationCase](folderPath, testCaseName)

    testDataResult match
      case Left(error) =>
        fail(s"Failed to load test data for $testCaseName: $error")
      case Right((testCase, binaryData)) =>
        // Verify binary data is not empty
        binaryData should not be empty withClue "Binary data should not be empty"
        // Verify binary data has reasonable size
        binaryData.length should be > 100 withClue "Binary data should have reasonable size for tiny config"
  }

  test("individual test - no_available_reports-1") {
    val folderPath = "stf/accumulate/tiny"
    val testCaseName = "no_available_reports-1"
    val testDataResult = TestFileLoader.loadJsonFromTestVectors[AccumulationCase](folderPath, testCaseName)

    testDataResult match
      case Left(error) =>
        fail(s"Failed to load test case $testCaseName: $error")
      case Right(testCase) =>
        // Test state transition
        val (postState, _, _, output) = AccumulationTransition.stfInternal(
          testCase.input,
          testCase.preState,
          List.empty, // initStagingSet - not part of accumulation test vectors
          List.empty, // initAuthQueues - not part of accumulation test vectors
          TinyConfig
        )

        assertAccumulationOutputEquals(testCase.output, output, testCaseName)
        assertAccumulationStateEquals(testCase.postState, postState, testCaseName)
  }

  test("tiny config state transition - all test vectors") {
    val folderPath = "stf/accumulate/tiny"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true withClue s"Failed to list tiny test vectors"

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    info(s"Running ${testCaseNames.size} tiny config test vectors")

    var passed = 0
    var failed = 0
    val failures = scala.collection.mutable.ListBuffer.empty[String]

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadJsonFromTestVectors[AccumulationCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right(testCase) =>
          try
            // Test state transition
            val (postState, _, _, output) = AccumulationTransition.stfInternal(
              testCase.input,
              testCase.preState,
              List.empty, // initStagingSet - not part of accumulation test vectors
              List.empty, // initAuthQueues - not part of accumulation test vectors
              TinyConfig
            )

            // Compare output
            assertAccumulationOutputEquals(testCase.output, output, testCaseName)

            // Compare post-state
            assertAccumulationStateEquals(testCase.postState, postState, testCaseName)

            passed += 1
            info(s"PASSED: $testCaseName")
          catch
            case e: Throwable =>
              failed += 1
              val errorMsg = s"$testCaseName - ${e.getMessage}"
              failures += errorMsg
              info(s"FAILED: $errorMsg")

    info(s"Results: Passed=$passed, Failed=$failed out of ${testCaseNames.size}")

    if failed > 0 then
      info("Failures:")
      failures.foreach(f => info(s"  - $f"))

    failed shouldBe 0 withClue s"$failed test vectors failed"
  }

  test("ready queue management") {
    // Test that ready queue is properly updated when no reports are accumulated
    val folderPath = "stf/accumulate/tiny"
    val testCaseName = "no_available_reports-1"
    val testDataResult = TestFileLoader.loadJsonFromTestVectors[AccumulationCase](folderPath, testCaseName)

    testDataResult match
      case Left(error) =>
        fail(s"Failed to load test case: $error")
      case Right(testCase) =>
        val (postState, _, _, _) = AccumulationTransition.stfInternal(
          testCase.input,
          testCase.preState,
          List.empty,
          List.empty,
          TinyConfig
        )

        // Verify slot is updated
        postState.slot shouldBe testCase.input.slot
        // Verify entropy is preserved
        java.util.Arrays.equals(postState.entropy.toArray, testCase.preState.entropy.toArray) shouldBe true
  }

  test("privileges preservation") {
    // Test that privileges are preserved when no services accumulate
    val folderPath = "stf/accumulate/tiny"
    val testCaseName = "no_available_reports-1"
    val testDataResult = TestFileLoader.loadJsonFromTestVectors[AccumulationCase](folderPath, testCaseName)

    testDataResult match
      case Left(error) =>
        fail(s"Failed to load test case: $error")
      case Right(testCase) =>
        val (postState, _, _, _) = AccumulationTransition.stfInternal(
          testCase.input,
          testCase.preState,
          List.empty,
          List.empty,
          TinyConfig
        )

        // Verify privileges are preserved (or updated correctly)
        postState.privileges.bless shouldBe testCase.postState.privileges.bless
        postState.privileges.designate shouldBe testCase.postState.privileges.designate
        postState.privileges.register shouldBe testCase.postState.privileges.register
        postState.privileges.assign shouldBe testCase.postState.privileges.assign
  }

  /**
   * Compare AccumulationOutput instances with detailed error messages.
   */
  private def assertAccumulationOutputEquals(
    expected: AccumulationOutput,
    actual: AccumulationOutput,
    testCaseName: String
  ): Unit =
    expected.isRight shouldBe true withClue s"Expected output should be defined in test case: $testCaseName"
    actual.isRight shouldBe true withClue s"Actual output should be defined in test case: $testCaseName"

    val expectedData = expected.toOption.get
    val actualData = actual.toOption.get

    java.util.Arrays.equals(expectedData.ok.toArray, actualData.ok.toArray) shouldBe true withClue
      s"Output mismatch in test case: $testCaseName. Expected: ${expectedData.ok.toHex.take(64)}, Actual: ${actualData.ok.toHex.take(64)}"

  /**
   * Compare AccumulationState instances with detailed error messages.
   */
  private def assertAccumulationStateEquals(
    expected: AccumulationState,
    actual: AccumulationState,
    testCaseName: String
  ): Unit =
    // Compare slot
    expected.slot shouldBe actual.slot withClue s"Slot mismatch in test case: $testCaseName"

    // Compare entropy
    java.util.Arrays.equals(expected.entropy.toArray, actual.entropy.toArray) shouldBe true withClue
      s"Entropy mismatch in test case: $testCaseName"

    // Compare ready queue
    expected.readyQueue.size shouldBe actual.readyQueue.size withClue
      s"Ready queue size mismatch in test case: $testCaseName"
    for (expSlot, actSlot, idx) <-
        expected.readyQueue.zip(actual.readyQueue).zipWithIndex.map(x => (x._1._1, x._1._2, x._2))
    do
      expSlot.size shouldBe actSlot.size withClue
        s"Ready queue slot $idx size mismatch in test case: $testCaseName. Expected: ${expSlot.size}, Actual: ${actSlot.size}"

    // Compare accumulated
    expected.accumulated.size shouldBe actual.accumulated.size withClue
      s"Accumulated size mismatch in test case: $testCaseName"
    for (expSlot, actSlot, idx) <-
        expected.accumulated.zip(actual.accumulated).zipWithIndex.map(x => (x._1._1, x._1._2, x._2))
    do
      expSlot.size shouldBe actSlot.size withClue
        s"Accumulated slot $idx size mismatch in test case: $testCaseName. Expected: ${expSlot.size}, Actual: ${actSlot.size}"
      for (exp, act) <- expSlot.zip(actSlot) do
        java.util.Arrays.equals(exp.toArray, act.toArray) shouldBe true withClue
          s"Accumulated hash mismatch at slot $idx in test case: $testCaseName"

    // Compare privileges
    assertPrivilegesEquals(expected.privileges, actual.privileges, testCaseName)

    // Compare statistics
    expected.statistics.size shouldBe actual.statistics.size withClue
      s"Statistics size mismatch in test case: $testCaseName. Expected: ${expected.statistics.size}, Actual: ${actual.statistics.size}"
    for (exp, act) <- expected.statistics.zip(actual.statistics) do
      exp.id shouldBe act.id withClue s"Statistics ID mismatch in test case: $testCaseName"
      assertServiceActivityRecordEquals(exp.record, act.record, testCaseName, s"service ${exp.id}")

    // Compare accounts
    expected.accounts.size shouldBe actual.accounts.size withClue
      s"Accounts size mismatch in test case: $testCaseName. Expected: ${expected.accounts.size}, Actual: ${actual.accounts.size}"

    val expectedAccountsMap = expected.accounts.map(a => a.id -> a).toMap
    val actualAccountsMap = actual.accounts.map(a => a.id -> a).toMap

    for id <- expectedAccountsMap.keys do
      actualAccountsMap.contains(id) shouldBe true withClue
        s"Missing account $id in actual state for test case: $testCaseName"
      assertAccumulationServiceItemEquals(expectedAccountsMap(id), actualAccountsMap(id), testCaseName)

    for id <- actualAccountsMap.keys do
      expectedAccountsMap.contains(id) shouldBe true withClue
        s"Unexpected account $id in actual state for test case: $testCaseName"

  /**
   * Compare Privileges instances.
   */
  private def assertPrivilegesEquals(
    expected: Privileges,
    actual: Privileges,
    testCaseName: String
  ): Unit =
    expected.bless shouldBe actual.bless withClue s"Privileges bless mismatch in test case: $testCaseName"
    expected.designate shouldBe actual.designate withClue s"Privileges designate mismatch in test case: $testCaseName"
    expected.register shouldBe actual.register withClue s"Privileges register mismatch in test case: $testCaseName"
    expected.assign shouldBe actual.assign withClue s"Privileges assign mismatch in test case: $testCaseName"
    expected.alwaysAcc.size shouldBe actual.alwaysAcc.size withClue
      s"Privileges alwaysAcc size mismatch in test case: $testCaseName"
    for (exp, act) <- expected.alwaysAcc.zip(actual.alwaysAcc) do
      exp.id shouldBe act.id withClue s"AlwaysAcc id mismatch in test case: $testCaseName"
      exp.gas shouldBe act.gas withClue s"AlwaysAcc gas mismatch in test case: $testCaseName"

  /**
   * Compare ServiceActivityRecord instances.
   */
  private def assertServiceActivityRecordEquals(
    expected: ServiceActivityRecord,
    actual: ServiceActivityRecord,
    testCaseName: String,
    context: String
  ): Unit =
    expected.providedCount shouldBe actual.providedCount withClue
      s"ServiceActivityRecord providedCount mismatch for $context in test case: $testCaseName"
    expected.providedSize shouldBe actual.providedSize withClue
      s"ServiceActivityRecord providedSize mismatch for $context in test case: $testCaseName"
    expected.refinementCount shouldBe actual.refinementCount withClue
      s"ServiceActivityRecord refinementCount mismatch for $context in test case: $testCaseName"
    expected.refinementGasUsed shouldBe actual.refinementGasUsed withClue
      s"ServiceActivityRecord refinementGasUsed mismatch for $context in test case: $testCaseName"
    expected.imports shouldBe actual.imports withClue
      s"ServiceActivityRecord imports mismatch for $context in test case: $testCaseName"
    expected.extrinsicCount shouldBe actual.extrinsicCount withClue
      s"ServiceActivityRecord extrinsicCount mismatch for $context in test case: $testCaseName"
    expected.extrinsicSize shouldBe actual.extrinsicSize withClue
      s"ServiceActivityRecord extrinsicSize mismatch for $context in test case: $testCaseName"
    expected.exports shouldBe actual.exports withClue
      s"ServiceActivityRecord exports mismatch for $context in test case: $testCaseName"
    expected.accumulateCount shouldBe actual.accumulateCount withClue
      s"ServiceActivityRecord accumulateCount mismatch for $context in test case: $testCaseName"
    expected.accumulateGasUsed shouldBe actual.accumulateGasUsed withClue
      s"ServiceActivityRecord accumulateGasUsed mismatch for $context in test case: $testCaseName"

  /**
   * Compare AccumulationServiceItem instances.
   */
  private def assertAccumulationServiceItemEquals(
    expected: AccumulationServiceItem,
    actual: AccumulationServiceItem,
    testCaseName: String
  ): Unit =
    expected.id shouldBe actual.id withClue s"ServiceItem id mismatch in test case: $testCaseName"

    // Compare service info
    val expService = expected.data.service
    val actService = actual.data.service
    // expService.version shouldBe actService.version withClue
    // s"ServiceInfo version mismatch for service ${expected.id} in test case: $testCaseName"
    java.util.Arrays.equals(expService.codeHash.bytes, actService.codeHash.bytes) shouldBe true withClue
      s"ServiceInfo codeHash mismatch for service ${expected.id} in test case: $testCaseName"
    expService.balance shouldBe actService.balance withClue
      s"ServiceInfo balance mismatch for service ${expected.id} in test case: $testCaseName"
    expService.minItemGas shouldBe actService.minItemGas withClue
      s"ServiceInfo minItemGas mismatch for service ${expected.id} in test case: $testCaseName"
    expService.minMemoGas shouldBe actService.minMemoGas withClue
      s"ServiceInfo minMemoGas mismatch for service ${expected.id} in test case: $testCaseName"
    expService.bytesUsed shouldBe actService.bytesUsed withClue
      s"ServiceInfo bytesUsed mismatch for service ${expected.id} in test case: $testCaseName"
    expService.depositOffset shouldBe actService.depositOffset withClue
      s"ServiceInfo depositOffset mismatch for service ${expected.id} in test case: $testCaseName"
    expService.items shouldBe actService.items withClue
      s"ServiceInfo items mismatch for service ${expected.id} in test case: $testCaseName"
    expService.creationSlot shouldBe actService.creationSlot withClue
      s"ServiceInfo creationSlot mismatch for service ${expected.id} in test case: $testCaseName"
    expService.lastAccumulationSlot shouldBe actService.lastAccumulationSlot withClue
      s"ServiceInfo lastAccumulationSlot mismatch for service ${expected.id} in test case: $testCaseName"
    expService.parentService shouldBe actService.parentService withClue
      s"ServiceInfo parentService mismatch for service ${expected.id} in test case: $testCaseName"

    // Compare storage
    expected.data.storage.size shouldBe actual.data.storage.size withClue
      s"Storage size mismatch for service ${expected.id} in test case: $testCaseName"

    // Compare preimages
    expected.data.preimages.size shouldBe actual.data.preimages.size withClue
      s"Preimages size mismatch for service ${expected.id} in test case: $testCaseName"

    // Compare preimageRequests
    expected.data.preimageRequests.size shouldBe actual.data.preimageRequests.size withClue
      s"PreimageRequests size mismatch for service ${expected.id} in test case: $testCaseName"

  test("trace single test case for debugging") {
    val folderPath = "stf/accumulate/tiny"
    // Set the test case name to debug a specific case
    val testCaseName = "process_one_immediate_report-1"
    val testDataResult = TestFileLoader.loadJsonFromTestVectors[AccumulationCase](folderPath, testCaseName)

    testDataResult match
      case Left(error) =>
        fail(s"Failed to load test case $testCaseName: $error")
      case Right(testCase) =>
        try
          val (postState, _, _, output) = AccumulationTransition.stfInternal(
            testCase.input,
            testCase.preState,
            List.empty,
            List.empty,
            TinyConfig
          )

          // Compare output
          assertAccumulationOutputEquals(testCase.output, output, testCaseName)
          assertAccumulationStateEquals(testCase.postState, postState, testCaseName)
          info(s"PASSED: $testCaseName")
  }

  test("full config state transition - all test vectors") {
    val folderPath = "stf/accumulate/full"
    val testCaseNamesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testCaseNamesResult.isRight shouldBe true withClue s"Failed to list full test vectors"

    val testCaseNames = testCaseNamesResult.getOrElse(List.empty)
    testCaseNames should not be empty

    info(s"Running ${testCaseNames.size} full config test vectors")

    var passed = 0
    var failed = 0
    val failures = scala.collection.mutable.ListBuffer.empty[String]

    for testCaseName <- testCaseNames do
      val testDataResult = TestFileLoader.loadJsonFromTestVectors[AccumulationCase](folderPath, testCaseName)
      testDataResult match
        case Left(error) =>
          fail(s"Failed to load test case $testCaseName: $error")
        case Right(testCase) =>
          try
            val (postState, _, _, output) = AccumulationTransition.stfInternal(
              testCase.input,
              testCase.preState,
              List.empty,
              List.empty,
              FullConfig
            )

            assertAccumulationOutputEquals(testCase.output, output, testCaseName)
            assertAccumulationStateEquals(testCase.postState, postState, testCaseName)

            passed += 1
            info(s"PASSED: $testCaseName")
          catch
            case e: Throwable =>
              failed += 1
              val errorMsg = s"$testCaseName - ${e.getMessage}"
              failures += errorMsg
              info(s"FAILED: $errorMsg")

    info(s"Results: Passed=$passed, Failed=$failed out of ${testCaseNames.size}")

    if failed > 0 then
      info("Failures:")
      failures.foreach(f => info(s"  - $f"))

    failed shouldBe 0 withClue s"$failed test vectors failed"
  }
