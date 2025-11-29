package io.forge.jam.core.encoding

import io.forge.jam.safrole.historical.*
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

class HistoryJsonTest {
    private fun assertHistoryStateEquals(expected: HistoricalState, actual: HistoricalState) {
        // Compare history lists size
        assertEquals(
            expected.beta.history.size,
            actual.beta.history.size,
            "Mismatch in beta.history list size. Expected: ${expected.beta.history.size}, Actual: ${actual.beta.history.size}"
        )

        // Compare each history entry
        for (i in expected.beta.history.indices) {
            assertHistoricalBetaEquals(expected.beta.history[i], actual.beta.history[i], "beta.history[$i]")
        }

        // Compare MMR at container level
        assertMmrEquals(expected.beta.mmr, actual.beta.mmr, "beta.mmr")
    }

    private fun assertHistoricalBetaEquals(expected: HistoricalBeta, actual: HistoricalBeta, path: String) {
        // Compare header hash
        assertEquals(
            expected.headerHash,
            actual.headerHash,
            "$path: Mismatch in headerHash. Expected: ${expected.headerHash.toHex()}, Actual: ${actual.headerHash.toHex()}"
        )

        // Compare beefy root
        assertEquals(
            expected.beefyRoot,
            actual.beefyRoot,
            "$path: Mismatch in beefyRoot. Expected: ${expected.beefyRoot.toHex()}, Actual: ${actual.beefyRoot.toHex()}"
        )

        // Compare state root
        assertEquals(
            expected.stateRoot,
            actual.stateRoot,
            "$path: Mismatch in stateRoot. Expected: ${expected.stateRoot.toHex()}, Actual: ${actual.stateRoot.toHex()}"
        )

        // Compare reported packages
        assertEquals(
            expected.reported.size,
            actual.reported.size,
            "$path: Mismatch in reported list size. Expected: ${expected.reported.size}, Actual: ${actual.reported.size}"
        )

        for (i in expected.reported.indices) {
            assertEquals(
                expected.reported[i].hash,
                actual.reported[i].hash,
                "$path: Mismatch in reported[$i]. Expected: ${expected.reported[i].hash.toHex()}, Actual: ${actual.reported[i].hash.toHex()}"
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
            assertEquals(
                expected.peaks[i],
                actual.peaks[i],
                "$path: Mismatch in peaks[$i]. Expected: ${expected.peaks[i]?.toHex()}, Actual: ${actual.peaks[i]?.toHex()}"
            )
        }
    }

    @Test
    fun testTinyHistory() {
        val folderPath = "stf/history/tiny"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<HistoricalCase>(folderPath, testCaseName)
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val transition = HistoryTransition()
            val postState = transition.stf(testCase.input, testCase.preState)
            assertHistoryStateEquals(
                testCase.postState,
                postState,
            )
        }
    }

    @Test
    fun testFullHistory() {
        val folderPath = "stf/history/full"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<HistoricalCase>(folderPath, testCaseName)
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val transition = HistoryTransition()
            val postState = transition.stf(testCase.input, testCase.preState)
            assertHistoryStateEquals(
                testCase.postState,
                postState,
            )
        }
    }
}
