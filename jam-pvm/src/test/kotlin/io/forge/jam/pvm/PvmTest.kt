package io.forge.jam.pvm

import io.forge.jam.core.encoding.TestFileLoader
import io.forge.jam.pvm.engine.Config
import io.forge.jam.pvm.engine.Engine
import org.junit.jupiter.api.Test

class PvmTest {
    @Test
    fun runTest() {
        val folderName = "pvm"
        // Init the config and engine
        val config = Config.new()
        val engine = Engine.new(config)


        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val inputCase = TestFileLoader.loadJsonData<PvmCase>(
                "$folderName/$testCase",
            )

            println("Running test case: ${inputCase.expectedGas}.")
            assert(false)
        }
    }
}
