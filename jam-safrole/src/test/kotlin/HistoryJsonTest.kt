package io.forge.jam.core.encoding

import io.forge.jam.core.toHex
import io.forge.jam.safrole.*
import org.junit.jupiter.api.Assertions.assertArrayEquals
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

class HistoryJsonTest {
    fun assertHistoryStateEquals(expected: HistoricalState, actual: HistoricalState) {
        // Compare beta lists size
        assertEquals(
            expected.beta.size,
            actual.beta.size,
            "Mismatch in beta list size. Expected: ${expected.beta.size}, Actual: ${actual.beta.size}"
        )

        // Compare each beta entry
        for (i in expected.beta.indices) {
            assertHistoricalBetaEquals(expected.beta[i], actual.beta[i], "beta[$i]")
        }
    }

    private fun assertHistoricalBetaEquals(expected: HistoricalBeta, actual: HistoricalBeta, path: String) {
        // Compare header hash
        assertArrayEquals(
            expected.headerHash,
            actual.headerHash,
            "$path: Mismatch in headerHash. Expected: ${expected.headerHash.toHex()}, Actual: ${actual.headerHash.toHex()}"
        )

        // Compare state root
        assertArrayEquals(
            expected.stateRoot,
            actual.stateRoot,
            "$path: Mismatch in stateRoot. Expected: ${expected.stateRoot.toHex()}, Actual: ${actual.stateRoot.toHex()}"
        )

        // Compare MMR
        assertMmrEquals(expected.mmr, actual.mmr, "$path.mmr")

        // Compare reported packages
        assertEquals(
            expected.reported.size,
            actual.reported.size,
            "$path: Mismatch in reported list size. Expected: ${expected.reported.size}, Actual: ${actual.reported.size}"
        )

        for (i in expected.reported.indices) {
            assertArrayEquals(
                expected.reported[i],
                actual.reported[i],
                "$path: Mismatch in reported[$i]. Expected: ${expected.reported[i].toHex()}, Actual: ${actual.reported[i].toHex()}"
            )
        }
    }

    private fun assertMmrEquals(expected: HistoricalMmr, actual: HistoricalMmr, path: String) {
        assertEquals(
            expected.peaks.size,
            actual.peaks.size,
            "$path: Mismatch in peaks list size. Expected: ${expected.peaks.size}, Actual: ${actual.peaks.size}"
        )

        for (i in expected.peaks.indices) {
            assertContentEquals(
                expected.peaks[i],
                actual.peaks[i],
                "$path: Mismatch in peaks[$i]. Expected: ${expected.peaks[i]?.toHex()}, Actual: ${actual.peaks[i]?.toHex()}"
            )
        }
    }

    @Test
    fun testHistory() {
        val folderName = "history"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            println("Current Test Case: $testCase")
            val (inputCase) = TestFileLoader.loadTestData<HistoricalCase>(
                "$folderName/$testCase",
                ".scale"
            )

            val transition = HistoryTransition()
            val postState = transition.stf(inputCase.input, inputCase.preState)
            println("Output beta: ${postState.beta}")

            assertHistoryStateEquals(
                inputCase.postState,
                postState,
            )
        }
    }
}
