package io.forge.jam.core.encoding

import io.forge.jam.safrole.accumulation.*
import io.forge.jam.safrole.report.ServiceItem
import kotlin.test.Test
import kotlin.test.assertEquals

class AccumulationJsonTest {
    fun assertAccumulationStateEquals(expected: AccumulationState, actual: AccumulationState, testCase: String) {
        assertEquals(expected.slot, actual.slot, "Slot mismatch in test case: $testCase")
        assertEquals(expected.entropy, actual.entropy, "Entropy mismatch in test case: $testCase")

        assertEquals(
            expected.readyQueue.size,
            actual.readyQueue.size,
            "Ready queue size mismatch in test case: $testCase"
        )

        expected.readyQueue.forEachIndexed { queueIndex, expectedQueue ->
            val actualQueue = actual.readyQueue[queueIndex]
            println("actualQueue: $actualQueue")
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

        assertEquals(expected.privileges, actual.privileges, "Privileges mismatch in test case: $testCase")

        assertEquals(
            expected.accounts.size,
            actual.accounts.size,
            "Accounts size mismatch in test case: $testCase"
        )

        expected.accounts.forEachIndexed { index, expectedAccount ->
            val actualAccount = actual.accounts[index]
            assertServiceItemEquals(expectedAccount, actualAccount, index, testCase)
        }
    }

    private fun assertReadyRecordEquals(expected: ReadyRecord, actual: ReadyRecord, path: String, testCase: String) {
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

    private fun assertServiceItemEquals(expected: ServiceItem, actual: ServiceItem, index: Int, testCase: String) {
        assertEquals(
            expected,
            actual,
            "Service item[$index] mismatch in test case: $testCase"
        )
    }

    fun assertAccumulationOutputEquals(expected: AccumulationOutput, actual: AccumulationOutput, testCase: String) {
        assertEquals(
            expected.ok,
            actual.ok,
            "Output.ok mismatch in test case: $testCase"
        )
    }

    @Test
    fun testTinyAccumulations() {
        val folderName = "accumulation/tiny"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestData<AccumulationCase>(
                "$folderName/$testCase",
                ".bin"
            )

            val report = AccumulationStateTransition(
                AccumulationConfig(EPOCH_LENGTH = 12)
            )
            val (postState, output) = report.transition(inputCase.input, inputCase.preState)
            println("Running test case: $testCase")
            assertAccumulationStateEquals(inputCase.postState, postState, testCase)
            assertAccumulationOutputEquals(inputCase.output, output, testCase)
        }
    }
}
