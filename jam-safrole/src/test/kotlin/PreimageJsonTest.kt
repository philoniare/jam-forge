package io.forge.jam.core.encoding

import io.forge.jam.safrole.preimage.PreimageCase
import io.forge.jam.safrole.preimage.PreimageOutput
import io.forge.jam.safrole.preimage.PreimageState
import io.forge.jam.safrole.preimage.PreimageStateTransition
import kotlin.test.Test
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
                assertEquals(
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
            println("Expected: ${expectedAccount}")
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
    fun testPreimages() {
        val folderName = "preimages"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestData<PreimageCase>(
                "$folderName/$testCase",
                ".bin"
            )

            val stf = PreimageStateTransition()
            val (postState, output) = stf.transition(inputCase.input, inputCase.preState)
            assertPreimageOutputEquals(inputCase.output, output, testCase)
            assertPreimageStateEquals(
                inputCase.postState,
                postState,
                testCase
            )
        }
    }
}
