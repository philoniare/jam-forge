package io.forge.jam.core.encoding

import io.forge.jam.safrole.stats.StatCase
import io.forge.jam.safrole.stats.StatConfig
import io.forge.jam.safrole.stats.StatState
import io.forge.jam.safrole.stats.StatStateTransition
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

class StatJsonTest {
    private fun assertStatStateEquals(expected: StatState, actual: StatState, testCase: String) {
        assertEquals(expected.slot, actual.slot, "slot values should match in $testCase")
        assertEquals(expected.currValidators.size, actual.currValidators.size, "currValidators list sizes should match in $testCase")

        assertEquals(
            expected.valsCurrStats.size,
            actual.valsCurrStats.size,
            "valsCurrStats list sizes should match in $testCase"
        )
        assertEquals(
            expected.valsLastStats.size,
            actual.valsLastStats.size,
            "valsLastStats list sizes should match in $testCase"
        )

        // Compare current stats
        expected.valsCurrStats.zip(actual.valsCurrStats).forEachIndexed { index, (exp, act) ->
            assertEquals(exp.blocks, act.blocks, "current blocks at index $index should match in $testCase")
            assertEquals(exp.tickets, act.tickets, "current tickets at index $index should match in $testCase")
            assertEquals(exp.preImages, act.preImages, "current preImages at index $index should match in $testCase")
            assertEquals(exp.preImagesSize, act.preImagesSize, "current preImagesSize at index $index should match in $testCase")
            assertEquals(exp.guarantees, act.guarantees, "current guarantees at index $index should match in $testCase")
            assertEquals(exp.assurances, act.assurances, "current assurances at index $index should match in $testCase")
        }

        // Compare last stats
        expected.valsLastStats.zip(actual.valsLastStats).forEachIndexed { index, (exp, act) ->
            assertEquals(exp.blocks, act.blocks, "last blocks at index $index should match in $testCase")
            assertEquals(exp.tickets, act.tickets, "last tickets at index $index should match in $testCase")
            assertEquals(exp.preImages, act.preImages, "last preImages at index $index should match in $testCase")
            assertEquals(exp.preImagesSize, act.preImagesSize, "last preImagesSize at index $index should match in $testCase")
            assertEquals(exp.guarantees, act.guarantees, "last guarantees at index $index should match in $testCase")
            assertEquals(exp.assurances, act.assurances, "last assurances at index $index should match in $testCase")
        }

        // Compare validator keys
        expected.currValidators.zip(actual.currValidators).forEachIndexed { index, (exp, act) ->
            assertEquals(exp, act, "validator key at index $index should match in $testCase")
        }
    }

    @Test
    fun testTinyStats() {
        val folderName = "stf/statistics/tiny"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderName)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<StatCase>(
                folderName,
                testCaseName,
                ".bin"
            )
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val stf = StatStateTransition(StatConfig(EPOCH_LENGTH = 12))
            val (postState, _) = stf.transition(testCase.input, testCase.preState)
            assertStatStateEquals(testCase.postState, postState, testCaseName)
        }
    }

    @Test
    fun testFullStats() {
        val folderName = "stf/statistics/full"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderName)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<StatCase>(
                folderName,
                testCaseName,
                ".bin"
            )
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val stf = StatStateTransition(StatConfig(EPOCH_LENGTH = 600))
            val (postState, _) = stf.transition(testCase.input, testCase.preState)
            assertStatStateEquals(testCase.postState, postState, testCaseName)
        }
    }

    @Test
    fun testTinyStatsDecoding() {
        val folderName = "stf/statistics/tiny"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderName)

        for (testCaseName in testCaseNames) {
            val (testCase, binaryData) = TestFileLoader.loadTestDataFromTestVectors<StatCase>(
                folderName,
                testCaseName,
                ".bin"
            )

            // Decode from binary (tiny: 2 cores, 6 validators)
            val (decodedCase, bytesConsumed) = StatCase.fromBytes(binaryData, 0, coresCount = 2, validatorsCount = 6)

            assertEquals(binaryData.size, bytesConsumed, "Bytes consumed mismatch for $testCaseName")
            assertEquals(testCase.input.slot, decodedCase.input.slot, "Input slot mismatch for $testCaseName")
            assertEquals(testCase.preState.slot, decodedCase.preState.slot, "PreState slot mismatch for $testCaseName")
            assertEquals(testCase.postState.slot, decodedCase.postState.slot, "PostState slot mismatch for $testCaseName")

            // Verify round-trip
            assertContentEquals(binaryData, decodedCase.encode(), "Round-trip encoding mismatch for $testCaseName")
        }
    }

    @Test
    fun testFullStatsDecoding() {
        val folderName = "stf/statistics/full"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderName)

        for (testCaseName in testCaseNames) {
            val (testCase, binaryData) = TestFileLoader.loadTestDataFromTestVectors<StatCase>(
                folderName,
                testCaseName,
                ".bin"
            )

            // Decode from binary (full: 341 cores, 1023 validators)
            val (decodedCase, bytesConsumed) = StatCase.fromBytes(binaryData, 0, coresCount = 341, validatorsCount = 1023)

            assertEquals(binaryData.size, bytesConsumed, "Bytes consumed mismatch for $testCaseName")
            assertEquals(testCase.input.slot, decodedCase.input.slot, "Input slot mismatch for $testCaseName")
            assertEquals(testCase.preState.slot, decodedCase.preState.slot, "PreState slot mismatch for $testCaseName")
            assertEquals(testCase.postState.slot, decodedCase.postState.slot, "PostState slot mismatch for $testCaseName")

            // Verify round-trip
            assertContentEquals(binaryData, decodedCase.encode(), "Round-trip encoding mismatch for $testCaseName")
        }
    }
}
