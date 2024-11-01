package io.forge.jam.core.encoding

import io.forge.jam.core.toHex
import io.forge.jam.safrole.SafroleCase
import io.forge.jam.safrole.SafroleConfig
import io.forge.jam.safrole.SafroleState
import io.forge.jam.safrole.SafroleStateTransition
import org.junit.jupiter.api.Assertions.assertArrayEquals
import kotlin.test.Test
import kotlin.test.assertEquals

class DisputeJsonTest {

    fun assertSafroleStateEquals(expected: SafroleState, actual: SafroleState) {
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


    }

    @Test
    fun testTinyDisputes() {
        val folderName = "disputes/tiny"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
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

            assertSafroleStateEquals(inputCase.postState, postState)
        }
    }
}
