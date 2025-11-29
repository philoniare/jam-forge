package io.forge.jam.core.encoding

import io.forge.jam.core.JamByteArray
import io.forge.jam.safrole.authorization.AuthCase
import io.forge.jam.safrole.authorization.AuthConfig
import io.forge.jam.safrole.authorization.AuthState
import io.forge.jam.safrole.authorization.AuthorizationStateTransition
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

class AuthorizationJsonTest {

    fun assertAuthStateEquals(expected: AuthState, actual: AuthState, testCase: String) {
        // Compare auth pools
        assertAuthPoolsEqual(expected.authPools, actual.authPools, testCase)

        // Compare auth queues
        assertAuthQueuesEqual(expected.authQueues, actual.authQueues, testCase)
    }

    private fun assertAuthPoolsEqual(
        expected: List<List<JamByteArray>>,
        actual: List<List<JamByteArray>>,
        testCase: String
    ) {
        assertEquals(
            expected.size,
            actual.size,
            "Auth pools size mismatch in test case: $testCase"
        )

        expected.zip(actual).forEachIndexed { index, (expectedPool, actualPool) ->
            assertContentEquals(
                expectedPool,
                actualPool,
                "Auth pool $index mismatch in test case: $testCase. Expected: ${expectedPool}, Actual: ${actualPool}"
            )
        }
    }

    private fun assertAuthQueuesEqual(
        expected: List<List<JamByteArray>>,
        actual: List<List<JamByteArray>>,
        testCase: String
    ) {
        assertEquals(
            expected.size,
            actual.size,
            "Auth queues size mismatch in test case: $testCase"
        )

        expected.zip(actual).forEachIndexed { index, (expectedQueue, actualQueue) ->
            assertContentEquals(
                expectedQueue,
                actualQueue,
                "Auth queue $index mismatch in test case: $testCase\n" +
                    "Expected: ${expectedQueue.joinToString("\n")}\n" +
                    "Actual: ${actualQueue.joinToString("\n")}"
            )
        }
    }

    @Test
    fun testTinyAuthorizations() {
        val folderPath = "stf/authorizations/tiny"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<AuthCase>(folderPath, testCaseName)
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val stf = AuthorizationStateTransition(AuthConfig(CORE_COUNT = 2))
            val (postState, output) = stf.transition(testCase.input, testCase.preState)
            assertAuthStateEquals(testCase.postState, postState, testCaseName)
        }
    }

    @Test
    fun testFullAuthorizations() {
        val folderPath = "stf/authorizations/full"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<AuthCase>(folderPath, testCaseName)
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val stf = AuthorizationStateTransition(AuthConfig(CORE_COUNT = 341))
            val (postState, output) = stf.transition(testCase.input, testCase.preState)
            assertAuthStateEquals(testCase.postState, postState, testCaseName)
        }
    }
}
