package io.forge.jam.pvm

import io.forge.jam.core.encoding.TestFileLoader
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.ArcBytes
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.program.ProgramParts
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class PvmTest {
    @Test
    fun runTest() {
        val folderName = "pvm"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val inputCase = TestFileLoader.loadJsonData<PvmCase>(
                "$folderName/$testCase",
            )

            val config = Config.new()
            val engine = Engine.new(config)
            
            val parts = ProgramParts.fromRaw(inputCase.program).getOrThrow()
            val blob = ProgramBlob.fromParts(parts).getOrThrow()

            val moduleConfig = ModuleConfig.new()
            moduleConfig.setStrict(true)
            moduleConfig.setGasMetering(GasMeteringKind.Sync)
            moduleConfig.setStepTracing(true)

            val module = Module.fromBlob(engine, moduleConfig, blob).getOrThrow()
            val instance = module.instantiate().getOrThrow()

            // Init instance state
            module.setGas(inputCase.initialGas)
            module.setNextProgramCounter(inputCase.initialPc)

            inputCase.initialRegs.forEachIndexed { index, value ->
                module.setRegister(index, value)
            }

            var finalPc = inputCase.initialPc
            val expectedStatus = run {
                while (true) {
                    when (val result = instance.run().getOrThrow()) {
                        InterruptKind.Finished -> return@run "halt"
                        InterruptKind.Trap -> return@run "trap"
                        InterruptKind.Ecalli -> TODO()
                        InterruptKind.NotEnoughGas -> return@run "out-of-gas"
                        InterruptKind.Segfault -> TODO()
                        InterruptKind.Step -> {
                            finalPc = instance.programCounter().getOrThrow()
                            continue
                        }
                    }
                }
            }
            if (expectedStatus != "halt") {
                finalPc = instance.programCounter().getOrThrow()
            }

            // Validate output state
            assertEquals(inputCase.expectedPc, finalPc, "Program counter mismatch.")
            // Validate reg values
//            inputCase.expectedRegs.forEachIndexed { index, value ->
//                assertEquals(value, instance.reg(index).getOrThrow(), "Register $index mismatch.")
//            }
//
//            // Validate memory update
//            inputCase.initialPageMap.forEachIndexed { index, page ->
//                assertEquals(page, instance.pageMap(index).getOrThrow(), "Page map $index mismatch.")
//            }
//            assertEquals(inputCase.expectedGas, instance.gas(), "Gas mismatch.")
        }
    }
}
