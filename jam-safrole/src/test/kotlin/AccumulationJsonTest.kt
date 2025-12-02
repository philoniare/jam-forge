package io.forge.jam.core.encoding

import io.forge.jam.safrole.accumulation.*
import io.forge.jam.safrole.report.AccumulationServiceItem
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

class AccumulationJsonTest {
    fun assertAccumulationStateEquals(
        expected: AccumulationState,
        actual: AccumulationState,
        testCase: String
    ) {
        try {
            assertEquals(
                expected.slot,
                actual.slot,
                "Slot mismatch in test case: $testCase"
            )
            assertEquals(
                expected.entropy,
                actual.entropy,
                "Entropy mismatch in test case: $testCase"
            )

            assertEquals(
                expected.readyQueue.size,
                actual.readyQueue.size,
                "Ready queue size mismatch in test case: $testCase"
            )

            expected.readyQueue.forEachIndexed { queueIndex, expectedQueue ->
                val actualQueue = actual.readyQueue[queueIndex]
                assertEquals(
                    expectedQueue.size,
                    actualQueue.size,
                    "Ready queue[$queueIndex] size mismatch in test case: $testCase"
                )

                expectedQueue.forEachIndexed { recordIndex, expectedRecord ->
                    val actualRecord = actualQueue[recordIndex]
                    assertReadyRecordEquals(
                        expectedRecord,
                        actualRecord,
                        "Ready queue[$queueIndex][$recordIndex]",
                        testCase
                    )
                }
            }

            assertEquals(
                expected.accumulated.size,
                actual.accumulated.size,
                "Accumulated size mismatch in test case: $testCase"
            )

            expected.accumulated.forEachIndexed { listIndex, expectedList ->
                val actualList = actual.accumulated[listIndex]
                assertEquals(
                    expectedList.size,
                    actualList.size,
                    "Accumulated[$listIndex] size mismatch in test case: $testCase"
                )

                expectedList.forEachIndexed { itemIndex, expectedItem ->
                    val actualItem = actualList[itemIndex]
                    assertEquals(
                        expectedItem,
                        actualItem,
                        "Accumulated[$listIndex][$itemIndex] mismatch in test case: $testCase"
                    )
                }
            }

            assertEquals(
                expected.privileges,
                actual.privileges,
                "Privileges mismatch in test case: $testCase"
            )

            assertEquals(
                expected.accounts.size,
                actual.accounts.size,
                "Accounts size mismatch in test case: $testCase"
            )

            expected.accounts.forEachIndexed { index, expectedAccount ->
                val actualAccount = actual.accounts[index]
                assertServiceItemEquals(
                    expectedAccount,
                    actualAccount,
                    index,
                    testCase
                )
            }
        } catch (e: Throwable) {
            throw e
        }
    }

    private fun assertReadyRecordEquals(
        expected: ReadyRecord,
        actual: ReadyRecord,
        path: String,
        testCase: String
    ) {
        assertEquals(
            expected.report,
            actual.report,
            "Report mismatch at $path in test case: $testCase"
        )
        assertEquals(
            expected.dependencies,
            actual.dependencies,
            "Dependencies mismatch at $path in test case: $testCase"
        )
    }

    private fun assertServiceItemEquals(
        expected: AccumulationServiceItem,
        actual: AccumulationServiceItem,
        index: Int,
        testCase: String
    ) {
        assertEquals(
            expected,
            actual,
            "Service item[$index] mismatch in test case: $testCase"
        )
    }

    fun assertAccumulationOutputEquals(
        expected: AccumulationOutput,
        actual: AccumulationOutput,
        testCase: String
    ) {
        assertEquals(expected.ok, actual.ok, "Output.ok mismatch in test case: $testCase")
    }

    @Test
    fun testTinyAccumulationsEncoding() {
        val folderName = "stf/accumulate/tiny"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderName)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) =
                TestFileLoader.loadTestDataFromTestVectors<AccumulationCase>(
                    folderName,
                    testCaseName,
                    ".bin"
                )
            assertContentEquals(
                expectedBinaryData,
                testCase.encode(),
                "Encoding mismatch for $testCaseName"
            )
        }
    }

    @Test
    fun testTinyAccumulationsStateTransition() {
        runAccumulationTests("stf/accumulate/tiny", epochLength = 12)
    }

    @Test
    fun testFullAccumulationsStateTransition() {
        runAccumulationTests("stf/accumulate/full", epochLength = 600)
    }

    private fun runAccumulationTests(folderName: String, epochLength: Int) {
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderName)

        for (testCaseName in testCaseNames) {
            val (testCase, _) =
                TestFileLoader.loadTestDataFromTestVectors<AccumulationCase>(
                    folderName,
                    testCaseName,
                    ".bin"
                )

            val report =
                AccumulationStateTransition(
                    AccumulationConfig(
                        EPOCH_LENGTH = epochLength,
                        MAX_BLOCK_HISTORY = 8,
                        AUTH_QUEUE_SIZE = 80
                    )
                )
            val (postState, output) =
                report.transition(testCase.input, testCase.preState)
            assertAccumulationStateEquals(testCase.postState, postState, testCaseName)
            assertAccumulationOutputEquals(testCase.output, output, testCaseName)
        }
    }

    @Test
    fun testFullAccumulationsEncoding() {
        val folderName = "stf/accumulate/full"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderName)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) =
                TestFileLoader.loadTestDataFromTestVectors<AccumulationCase>(
                    folderName,
                    testCaseName,
                    ".bin"
                )
            assertContentEquals(
                expectedBinaryData,
                testCase.encode(),
                "Encoding mismatch for $testCaseName"
            )
        }
    }
}
