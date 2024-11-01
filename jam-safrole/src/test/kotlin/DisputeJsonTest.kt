package io.forge.jam.core.encoding

import io.forge.jam.core.toHex
import io.forge.jam.safrole.*
import org.junit.jupiter.api.Assertions.assertArrayEquals
import kotlin.test.Test
import kotlin.test.assertEquals

class DisputeJsonTest {

    fun assertDisputeOutputEquals(expected: SafroleOutput, actual: SafroleOutput, testCase: String) {
        if (expected.err != null) {
            assertEquals(expected.err, actual.err, "$testCase: Mismatch in error")
        }

        if (expected.ok != null) {
            assertEquals(expected.ok!!.epochMark, actual.ok?.epochMark, "$testCase: Mismatch in EpochMark")
            assertEquals(expected.ok!!.ticketsMark, actual.ok?.ticketsMark, "$testCase: Mismatch in TicketsMark")
            if (expected.ok!!.offendersMark != null && actual.ok?.offendersMark != null) {
                assertEquals(
                    expected.ok!!.offendersMark!!.size,
                    actual.ok?.offendersMark!!.size,
                    "$testCase: Mismatch in OffendersMark size"
                )
                for (i in expected.ok!!.offendersMark!!.indices) {
                    assertArrayEquals(
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

    fun assertDisputeStateEquals(expected: SafroleState, actual: SafroleState) {
        assertEquals(expected.tau, actual.tau, "Mismatch in tau.")

        assertEquals(expected.eta.size, actual.eta.size, "Mismatch in eta size")
        for (i in expected.eta.indices) {
            assertArrayEquals(
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
        assertArrayEquals(expected.gammaZ, actual.gammaZ, "Mismatch in gammaZ")

        // Check rho
        assertEquals(expected.rho?.size, actual.rho?.size, "Mismatch in rho size")
        if (expected.rho != null && actual.rho != null) {
            for (i in expected.rho!!.indices) {
                assertEquals(expected.rho!![i], actual.rho!![i], "Mismatch in rho at index $i")
            }
        }

        // Check psi
        assertEquals(expected.psi, actual.psi, "Mismatch in psi")
    }

    @Test
    fun testTinyDisputes() {
        val folderName = "disputes/tiny"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            println("Running test case: $testCase")
            val (inputCase) = TestFileLoader.loadTestData<SafroleCase>(
                "$folderName/$testCase",
                ".scale"
            )

            val safrole = SafroleStateTransition(
                SafroleConfig(
                    epochLength = 12,
                    ticketCutoff = 10,
                    ringSize = 6
                )
            )
            val (postState, output) = safrole.transition(inputCase.input, inputCase.preState)
            println("Output: $output")
            println("Output: $postState")

            assertDisputeOutputEquals(inputCase.output, output, testCase)

            assertDisputeStateEquals(inputCase.postState, postState)
        }
    }
}
