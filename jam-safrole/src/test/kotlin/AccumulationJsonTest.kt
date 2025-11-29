package io.forge.jam.core.encoding

import io.forge.jam.safrole.accumulation.*
import io.forge.jam.safrole.report.ServiceItem
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

class AccumulationJsonTest {
    fun assertAccumulationStateEquals(
            expected: AccumulationState,
            actual: AccumulationState,
            testCase: String
    ) {
        assertEquals(expected.slot, actual.slot, "Slot mismatch in test case: $testCase")
        assertEquals(expected.entropy, actual.entropy, "Entropy mismatch in test case: $testCase")

        assertEquals(
                expected.readyQueue.size,
                actual.readyQueue.size,
                "Ready queue size mismatch in test case: $testCase"
        )

        println("ReadyQueue Expect: ${expected.readyQueue}")
        println("ReadyQueue Actual: ${actual.readyQueue}")
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

        println("Accumulated Expect: ${expected.accumulated}")
        println("Accumulated Actual: ${actual.accumulated}")
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
            assertServiceItemEquals(expectedAccount, actualAccount, index, testCase)
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
            expected: ServiceItem,
            actual: ServiceItem,
            index: Int,
            testCase: String
    ) {
        if (expected != actual) {
            println("Service item[$index] mismatch details:")
            println("  Expected id: ${expected.id}, Actual id: ${actual.id}")
            println("  Expected service: ${expected.data.service}")
            println("  Actual service:   ${actual.data.service}")
            println(
                    "  Expected storage size: ${expected.data.storage.size}, Actual: ${actual.data.storage.size}"
            )
            println(
                    "  Expected preimages size: ${expected.data.preimages.size}, Actual: ${actual.data.preimages.size}"
            )
            if (expected.data.storage.size == actual.data.storage.size) {
                expected.data.storage.forEachIndexed { i, exp ->
                    val act = actual.data.storage.getOrNull(i)
                    if (exp != act) {
                        println("  Storage[$i] mismatch: expected $exp, actual $act")
                    }
                }
            }
        }
        assertEquals(expected, actual, "Service item[$index] mismatch in test case: $testCase")
    }

    fun assertAccumulationOutputEquals(
            expected: AccumulationOutput,
            actual: AccumulationOutput,
            testCase: String
    ) {
        assertEquals(expected.ok, actual.ok, "Output.ok mismatch in test case: $testCase")
    }

    @Test
    fun testTinyAccumulations() {
        val folderName = "stf/accumulate/tiny"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderName)

        for (testCaseName in testCaseNames) {
            println("Running test case: $testCaseName")
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

            // val report =
            //         AccumulationStateTransition(
            //                 AccumulationConfig(
            //                         EPOCH_LENGTH = 12,
            //                         MAX_BLOCK_HISTORY = 8,
            //                         AUTH_QUEUE_SIZE = 80
            //                 )
            //         )
            // val (postState, output) = report.transition(testCase.input, testCase.preState)
            // assertAccumulationStateEquals(testCase.postState, postState, testCaseName)
            // assertAccumulationOutputEquals(testCase.output, output, testCaseName)
        }
    }

    @Test
    fun testFullAccumulations() {
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

            // val report =
            //         AccumulationStateTransition(
            //                 AccumulationConfig(
            //                         EPOCH_LENGTH = 600,
            //                         MAX_BLOCK_HISTORY = 8,
            //                         AUTH_QUEUE_SIZE = 80
            //                 )
            //         )
            // val (postState, output) = report.transition(testCase.input, testCase.preState)
            // assertAccumulationStateEquals(testCase.postState, postState, testCaseName)
            // assertAccumulationOutputEquals(testCase.output, output, testCaseName)
        }
    }
}
