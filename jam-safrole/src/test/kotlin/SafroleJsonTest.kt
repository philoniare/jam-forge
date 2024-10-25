package io.forge.jam.core.encoding

import io.forge.jam.safrole.SafroleCase
import io.forge.jam.safrole.SafroleState
import io.forge.jam.safrole.SafroleStateTransition
import org.junit.jupiter.api.Assertions.assertArrayEquals
import kotlin.test.Test
import kotlin.test.assertEquals

class SafroleJsonTest {
    fun assertSafroleStateEquals(expected: SafroleState, actual: SafroleState) {
        assertEquals(expected.tau, actual.tau, "Mismatch in tau")

        assertEquals(expected.eta.size, actual.eta.size, "Mismatch in eta size")
        for (i in expected.eta.indices) {
            assertArrayEquals(expected.eta[i], actual.eta[i], "Mismatch in eta at index $i")
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
    fun testSafrole() {
        val folderName = "tiny"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestData<SafroleCase>(
                "$folderName/$testCase",
                ".scale"
            )

            val (postState, output) = SafroleStateTransition.transition(inputCase.input, inputCase.preState)

            // Compare the expected and actual output
            assertEquals(
                inputCase.output,
                output,
                "State transition outputs do not match"
            )

            // Compare the expected and actual post_state
            assertSafroleStateEquals(
                inputCase.postState,
                postState,
            )
        }

    }
}
