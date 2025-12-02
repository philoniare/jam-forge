package io.forge.jam.core.encoding

import io.forge.jam.safrole.preimage.PreimageCase
import io.forge.jam.safrole.preimage.PreimageOutput
import io.forge.jam.safrole.preimage.PreimageState
import io.forge.jam.safrole.preimage.PreimageStateTransition
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals
import kotlin.test.fail

class PreimageJsonTest {

    fun assertPreimageOutputEquals(expected: PreimageOutput, actual: PreimageOutput, testCase: String) {
        when {
            expected.err != null && actual.err == null -> {
                fail("$testCase: Expected error ${expected.err} but got success")
            }

            expected.err == null && actual.err != null -> {
                fail("$testCase: Expected success but got error ${actual.err}")
            }

            expected.err != null && actual.err != null -> {
                assertEquals(expected.err, actual.err, "$testCase: Error codes don't match")
                return
            }
        }

        if (expected.ok == null && actual.ok != null) {
            fail("$testCase: Expected null ok but got ${actual.ok}")
        }
        if (expected.ok != null && actual.ok == null) {
            fail("$testCase: Expected ${expected.ok} but got null ok")
        }

        expected.ok?.let { expectedMarks ->
            actual.ok?.let { actualMarks ->
                assertEquals<Any?>(
                    expectedMarks,
                    actualMarks,
                    "$testCase: Mismatch in ok value"
                )
            }
        }
    }

    fun assertPreimageStateEquals(expected: PreimageState, actual: PreimageState, testCase: String) {
        assertEquals(expected.accounts.size, actual.accounts.size)
        expected.accounts.forEachIndexed { index, expectedAccount ->
            val actualAccount = actual.accounts[index]

            // Compare account IDs
            assertEquals(
                expectedAccount.id,
                actualAccount.id,
                "Account ID mismatch at index $index in test case: $testCase"
            )

            val expectedInfo = expectedAccount.data
            val actualInfo = actualAccount.data

            // Compare preimages
            assertEquals(
                expectedInfo.preimages.size,
                actualInfo.preimages.size,
                "Preimage size mismatch for account ${expectedAccount.id} in test case: $testCase"
            )

            expectedInfo.preimages.forEachIndexed { preimageIndex, expectedPreimage ->
                val actualPreimage = actualInfo.preimages[preimageIndex]
                assertEquals(
                    expectedPreimage.hash,
                    actualPreimage.hash,
                    "Preimage hash mismatch at index $preimageIndex for account ${expectedAccount.id} in test case: $testCase"
                )
                assertEquals(
                    expectedPreimage.blob,
                    actualPreimage.blob,
                    "Preimage blob mismatch at index $preimageIndex for account ${expectedAccount.id} in test case: $testCase"
                )
            }

            // Compare history
            assertEquals(
                expectedInfo.lookupMeta.size,
                actualInfo.lookupMeta.size,
                "History size mismatch for account ${expectedAccount.id} in test case: $testCase"
            )

            expectedInfo.lookupMeta.forEachIndexed { historyIndex, expectedHistory ->
                val actualHistory = actualInfo.lookupMeta[historyIndex]

                // Compare history key
                assertEquals(
                    expectedHistory.key.hash,
                    actualHistory.key.hash,
                    "History key hash mismatch at index $historyIndex for account ${expectedAccount.id} in test case: $testCase"
                )
                assertEquals(
                    expectedHistory.key.length,
                    actualHistory.key.length,
                    "History key length mismatch at index $historyIndex for account ${expectedAccount.id} in test case: $testCase"
                )

                // Compare history values
                assertEquals(
                    expectedHistory.value.size,
                    actualHistory.value.size,
                    "History value size mismatch at index $historyIndex for account ${expectedAccount.id} in test case: $testCase"
                )
                expectedHistory.value.forEachIndexed { valueIndex, expectedValue ->
                    assertEquals(
                        expectedValue,
                        actualHistory.value[valueIndex],
                        "History value mismatch at index $valueIndex in history entry $historyIndex for account ${expectedAccount.id} in test case: $testCase"
                    )
                }
            }
        }
    }

    @Test
    fun testTinyPreimages() {
        val folderPath = "stf/preimages/tiny"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<PreimageCase>(folderPath, testCaseName)
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val stf = PreimageStateTransition()
            val (postState, output) = stf.transition(testCase.input, testCase.preState)
            assertPreimageOutputEquals(testCase.output, output, testCaseName)
            assertPreimageStateEquals(
                testCase.postState,
                postState,
                testCaseName
            )
        }
    }

    @Test
    fun testFullPreimages() {
        val folderPath = "stf/preimages/full"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<PreimageCase>(folderPath, testCaseName)
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val stf = PreimageStateTransition()
            val (postState, output) = stf.transition(testCase.input, testCase.preState)
            assertPreimageOutputEquals(testCase.output, output, testCaseName)
            assertPreimageStateEquals(
                testCase.postState,
                postState,
                testCaseName
            )
        }
    }

    @Test
    fun testTinyPreimagesDecoding() {
        val folderPath = "stf/preimages/tiny"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, binaryData) = TestFileLoader.loadTestDataFromTestVectors<PreimageCase>(folderPath, testCaseName)

            // Decode from binary
            val (decodedCase, bytesConsumed) = PreimageCase.fromBytes(binaryData, 0)

            assertEquals(binaryData.size, bytesConsumed, "Bytes consumed mismatch for $testCaseName")
            assertEquals(testCase.input.slot, decodedCase.input.slot, "Input slot mismatch for $testCaseName")

            // Verify round-trip
            assertContentEquals(binaryData, decodedCase.encode(), "Round-trip encoding mismatch for $testCaseName")
        }
    }

    @Test
    fun testFullPreimagesDecoding() {
        val folderPath = "stf/preimages/full"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, binaryData) = TestFileLoader.loadTestDataFromTestVectors<PreimageCase>(folderPath, testCaseName)

            // Decode from binary
            val (decodedCase, bytesConsumed) = PreimageCase.fromBytes(binaryData, 0)

            assertEquals(binaryData.size, bytesConsumed, "Bytes consumed mismatch for $testCaseName")
            assertEquals(testCase.input.slot, decodedCase.input.slot, "Input slot mismatch for $testCaseName")

            // Verify round-trip
            assertContentEquals(binaryData, decodedCase.encode(), "Round-trip encoding mismatch for $testCaseName")
        }
    }
}
