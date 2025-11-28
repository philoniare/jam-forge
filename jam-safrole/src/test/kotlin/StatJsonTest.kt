package io.forge.jam.core.encoding

import io.forge.jam.safrole.stats.StatCase
import io.forge.jam.safrole.stats.StatConfig
import io.forge.jam.safrole.stats.StatState
import io.forge.jam.safrole.stats.StatStateTransition
import kotlin.test.Test
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
        val testCases = TestFileLoader.getTestFilenamesFromTestVectors(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestDataFromTestVectors<StatCase>(
                folderName,
                testCase,
                ".bin"
            )

            val stf = StatStateTransition(StatConfig(EPOCH_LENGTH = 12))
            val (postState, _) = stf.transition(inputCase.input, inputCase.preState)
            assertStatStateEquals(inputCase.postState, postState, testCase)
        }
    }

    @Test
    fun testFullStats() {
        val folderName = "stf/statistics/full"
        val testCases = TestFileLoader.getTestFilenamesFromTestVectors(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestDataFromTestVectors<StatCase>(
                folderName,
                testCase,
                ".bin"
            )

            val stf = StatStateTransition(StatConfig(EPOCH_LENGTH = 600))
            val (postState, _) = stf.transition(inputCase.input, inputCase.preState)
            assertStatStateEquals(inputCase.postState, postState, testCase)
        }
    }
}
