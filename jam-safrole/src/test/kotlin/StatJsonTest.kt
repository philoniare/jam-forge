package io.forge.jam.core.encoding

import io.forge.jam.safrole.stats.StatCase
import io.forge.jam.safrole.stats.StatStateTransition
import kotlin.test.Test

class StatJsonTest {
    @Test
    fun testStats() {
        val folderName = "stats"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestData<StatCase>(
                "$folderName/$testCase",
                ".bin"
            )

            val stf = StatStateTransition()
            val (postState, output) = stf.transition(inputCase.input, inputCase.preState)
        }
    }
}
