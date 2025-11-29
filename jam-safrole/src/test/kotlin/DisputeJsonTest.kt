package io.forge.jam.core.encoding

import io.forge.jam.safrole.dispute.DisputeCase
import io.forge.jam.safrole.dispute.DisputeOutput
import io.forge.jam.safrole.safrole.*
import kotlin.test.*
import kotlin.test.assertContentEquals

class DisputeJsonTest {

    private fun assertDisputeOutputEquals(expected: DisputeOutput, actual: SafroleOutput, testCase: String) {
        if (expected.err != null) {
            assertNotNull(actual.err, "$testCase: Expected error but got OK")
            // Compare error names since DisputeErrorCode and SafroleErrorCode are separate enums
            assertEquals(expected.err.name, actual.err!!.name, "$testCase: Mismatch in error")
        }

        if (expected.ok != null) {
            assertNotNull(actual.ok, "$testCase: Expected OK but got error: ${actual.err}")
            assertNull(actual.ok!!.epochMark, "$testCase: Expected no EpochMark for disputes")
            assertNull(actual.ok!!.ticketsMark, "$testCase: Expected no TicketsMark for disputes")

            if (expected.ok!!.offendersMark != null && actual.ok?.offendersMark != null) {
                assertEquals(
                    expected.ok!!.offendersMark!!.size,
                    actual.ok?.offendersMark!!.size,
                    "$testCase: Mismatch in OffendersMark size"
                )
                for (i in expected.ok!!.offendersMark!!.indices) {
                    assertEquals(
                        expected.ok!!.offendersMark!![i],
                        actual.ok?.offendersMark!![i],
                        "$testCase: Mismatch in OffendersMark at index $i"
                    )
                }
            } else {
                assertEquals(
                    expected.ok!!.offendersMark,
                    actual.ok?.offendersMark,
                    "$testCase: Mismatch in OffendersMark"
                )
            }
        }
    }

    private fun assertDisputeStateEquals(expected: DisputeState, actual: SafroleState, testCase: String) {
        assertEquals(expected.tau, actual.tau, "Mismatch in tau. TestCase: $testCase")
        assertEquals(expected.lambda, actual.lambda, "Mismatch in lambda. TestCase: $testCase")
        assertEquals(expected.kappa, actual.kappa, "Mismatch in kappa. TestCase: $testCase")

        // Check rho
        assertEquals(expected.rho.size, actual.rho?.size, "Mismatch in rho size. TestCase: $testCase")
        if (actual.rho != null) {
            for (i in expected.rho.indices) {
                assertEquals(expected.rho[i], actual.rho!![i], "Mismatch in rho at index $i. TestCase: $testCase")
            }
        }

        // Check psi
        assertNotNull(actual.psi, "Actual psi should not be null. TestCase: $testCase")
        assertNotNull(expected.psi, "Expected psi should not be null. TestCase: $testCase")
        assertEquals(expected.psi!!.bad, actual.psi!!.bad, "Mismatch in bad. TestCase: $testCase")
        assertEquals(expected.psi!!.good, actual.psi!!.good, "Mismatch in good. TestCase: $testCase")
        assertEquals(expected.psi!!.wonky, actual.psi!!.wonky, "Mismatch in wonky. TestCase: $testCase")
    }

    private fun disputeStateToSafroleState(disputeState: DisputeState): SafroleState {
        return SafroleState(
            tau = disputeState.tau,
            kappa = disputeState.kappa,
            lambda = disputeState.lambda,
            rho = disputeState.rho.toMutableList(),
            psi = disputeState.psi
        )
    }

    private fun disputeInputToSafroleInput(disputeInput: DisputeInput): SafroleInput {
        return SafroleInput(
            disputes = disputeInput.disputes,
            slot = null
        )
    }

    @Test
    fun testTinyDisputes() {
        val folderPath = "stf/disputes/tiny"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<DisputeCase>(folderPath, testCaseName)
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val safrole = SafroleStateTransition(
                SafroleConfig(
                    maxTicketAttempts = 3,
                    epochLength = 12,
                    ticketCutoff = 10,
                    ringSize = 6,
                    validatorsCount = 6,
                    coresCount = 2
                )
            )

            val safroleInput = disputeInputToSafroleInput(testCase.input)
            val safrolePreState = disputeStateToSafroleState(testCase.preState)
            val (postState, output) = safrole.transition(safroleInput, safrolePreState)

            assertDisputeOutputEquals(testCase.output, output, testCaseName)
            assertDisputeStateEquals(testCase.postState, postState, testCaseName)
        }
    }

    @Test
    fun testFullDisputes() {
        val folderPath = "stf/disputes/full"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<DisputeCase>(folderPath, testCaseName)
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val safrole = SafroleStateTransition(
                SafroleConfig(
                    epochLength = 600,
                    ticketCutoff = 500,
                    ringSize = 6,
                    validatorsCount = 1023,
                    maxTicketAttempts = 3,
                    coresCount = 341
                )
            )

            val safroleInput = disputeInputToSafroleInput(testCase.input)
            val safrolePreState = disputeStateToSafroleState(testCase.preState)
            val (postState, output) = safrole.transition(safroleInput, safrolePreState)

            assertDisputeOutputEquals(testCase.output, output, testCaseName)
            assertDisputeStateEquals(testCase.postState, postState, testCaseName)
        }
    }
}
