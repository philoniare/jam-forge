package io.forge.jam.core.encoding

import io.forge.jam.safrole.stats.StatCase
import io.forge.jam.safrole.stats.StatConfig
import io.forge.jam.safrole.stats.StatState
import io.forge.jam.safrole.stats.StatStateTransition
import kotlin.test.Test
import kotlin.test.assertEquals

class StatJsonTest {
    private fun assertStatStateEquals(expected: StatState, actual: StatState, testCase: String) {
        assertEquals(expected.tau, actual.tau, "tau values should match")
        assertEquals(expected.kappaPrime.size, actual.kappaPrime.size, "kappaPrime list sizes should match")

        // Compare StatPi
        assertEquals(
            expected.pi.current.size,
            actual.pi.current.size,
            "current stats list sizes should match"
        )
        assertEquals(
            expected.pi.last.size,
            actual.pi.last.size,
            "last stats list sizes should match"
        )

        // Compare current stats
        expected.pi.current.zip(actual.pi.current).forEachIndexed { index, (exp, act) ->
            assertEquals(exp.blocks, act.blocks, "current blocks at index $index should match. Testcase: ${testCase}")
            assertEquals(exp.tickets, act.tickets, "current tickets at index $index should match")
            assertEquals(exp.preImages, act.preImages, "current preImages at index $index should match")
            assertEquals(exp.preImagesSize, act.preImagesSize, "current preImagesSize at index $index should match")
            assertEquals(exp.guarantees, act.guarantees, "current guarantees at index $index should match")
            assertEquals(exp.assurances, act.assurances, "current assurances at index $index should match")
        }

        // Compare last stats
        expected.pi.last.zip(actual.pi.last).forEachIndexed { index, (exp, act) ->
            assertEquals(exp.blocks, act.blocks, "last blocks at index $index should match")
            assertEquals(exp.tickets, act.tickets, "last tickets at index $index should match")
            assertEquals(exp.preImages, act.preImages, "last preImages at index $index should match")
            assertEquals(exp.preImagesSize, act.preImagesSize, "last preImagesSize at index $index should match")
            assertEquals(exp.guarantees, act.guarantees, "last guarantees at index $index should match")
            assertEquals(exp.assurances, act.assurances, "last assurances at index $index should match")
        }

        // Compare validator keys
        expected.kappaPrime.zip(actual.kappaPrime).forEachIndexed { index, (exp, act) ->
            assertEquals(exp, act, "validator key at index $index should match")
        }
    }

    @Test
    fun testTinyStats() {
        val folderName = "stats/tiny"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestData<StatCase>(
                "$folderName/$testCase",
                ".bin"
            )

            val stf = StatStateTransition(StatConfig(EPOCH_LENGTH = 12))
            val (postState, output) = stf.transition(inputCase.input, inputCase.preState)
            assertStatStateEquals(inputCase.postState, postState, testCase)
        }
    }

    @Test
    fun testFullStats() {
        val folderName = "stats/full"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestData<StatCase>(
                "$folderName/$testCase",
                ".bin"
            )

            val stf = StatStateTransition(StatConfig(EPOCH_LENGTH = 600))
            val (postState, output) = stf.transition(inputCase.input, inputCase.preState)
            assertStatStateEquals(inputCase.postState, postState, testCase)
        }
    }
}
