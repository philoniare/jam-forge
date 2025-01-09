package io.forge.jam.core.encoding

import io.forge.jam.safrole.safrole.*
import kotlin.test.Test
import kotlin.test.assertEquals

class SafroleJsonTest {
    fun assertSafroleOutputEquals(expected: SafroleOutput, actual: SafroleOutput, testCase: String) {
        if (expected.err != null) {
            assertEquals(expected.err, actual.err, "$testCase: Mismatch in error")
        }

        if (expected.ok != null) {
            assertEquals(expected.ok!!.epochMark, actual.ok?.epochMark, "$testCase: Mismatch in EpochMark")
            assertEquals(expected.ok!!.ticketsMark, actual.ok?.ticketsMark, "$testCase: Mismatch in TicketsMark")
        }
    }

    fun assertSafroleStateEquals(expected: SafroleState, actual: SafroleState, testCase: String) {
        assertEquals(expected.tau, actual.tau, "Mismatch in tau.  TestCase: $testCase")

        assertEquals(expected.eta.size, actual.eta.size, "Mismatch in eta size")
        for (i in expected.eta.indices) {
            assertEquals(
                expected.eta[i],
                actual.eta[i],
                "Mismatch in eta at index $i. Expected: ${expected.eta[i].toHex()}, Actual: ${actual.eta[i].toHex()}"
            )
        }

        assertEquals(expected.lambda, actual.lambda, "Mismatch in lambda")
        assertEquals(expected.kappa, actual.kappa, "Mismatch in kappa")
        assertEquals(expected.gammaK, actual.gammaK, "Mismatch in gammaK")
        assertEquals(expected.iota, actual.iota, "Mismatch in iota")
        assertEquals(expected.gammaA, actual.gammaA, "Mismatch in gammaA")
        assertEquals(expected.gammaS, actual.gammaS, "Mismatch in gammaS")
        assertEquals(expected.gammaZ, actual.gammaZ, "Mismatch in gammaZ")
    }

    @Test
    fun testTinySafrole() {
        val folderName = "safrole/tiny"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestData<SafroleCase>(
                "$folderName/$testCase",
                ".bin"
            )

            val safrole = SafroleStateTransition(
                SafroleConfig(
                    validatorCount = 6,
                    epochDuration = 12,
                    ticketCutoff = 10,
                    ringSize = 6,
                    maxTicketAttempts = 3
                )
            )
            val (postState, output) = safrole.transition(inputCase.input, inputCase.preState)

            // Compare the expected and actual output
            assertSafroleOutputEquals(inputCase.output, output, testCase)

            // Compare the expected and actual post_state
            assertSafroleStateEquals(
                inputCase.postState,
                postState,
                testCase,
            )
        }
    }

    @Test
    fun testFullSafrole() {
        val folderName = "safrole/full"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestData<SafroleCase>(
                "$folderName/$testCase",
                ".bin"
            )

            val safrole = SafroleStateTransition(
                SafroleConfig(
                    validatorCount = 1023,
                    epochDuration = 600,
                    ticketCutoff = 500,
                    ringSize = 1023,
                    maxTicketAttempts = 2
                )
            )
            val (postState, output) = safrole.transition(inputCase.input, inputCase.preState)

            // Compare the expected and actual output
            assertSafroleOutputEquals(inputCase.output, output, testCase)

            // Compare the expected and actual post_state
            assertSafroleStateEquals(
                inputCase.postState,
                postState,
                testCase
            )
        }
    }
}
