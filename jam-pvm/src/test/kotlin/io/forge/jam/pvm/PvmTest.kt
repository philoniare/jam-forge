package io.forge.jam.pvm

import io.forge.jam.core.encoding.TestFileLoader
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.program.ProgramCounter
import io.forge.jam.pvm.program.ProgramParts
import io.forge.jam.pvm.program.Reg
import org.junit.jupiter.api.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

class PvmTest {
    fun assertUIntListMatchesBytes(expected: List<UInt>, actual: Result<ByteArray>) {
        assertContentEquals(
            expected = expected.map { it.toUByte() },
            actual = actual.getOrThrow().map { it.toUByte() },
            message = "Memory mismatch."
        )
    }

    @Test
    fun runTest() {
        val folderName = "pvm"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val inputCase = TestFileLoader.loadJsonData<PvmCase>(
                "$folderName/$testCase",
            )

            val config = Config.new()
            val engine = Engine.new(config).getOrThrow()

            var parts = ProgramParts()
            parts.setCodeJumpTable(inputCase.program.toByteArray())
            val blob = ProgramBlob.fromParts(parts).getOrThrow()


            val moduleConfig = ModuleConfig.new()
            moduleConfig.setStrict(true)
            moduleConfig.setGasMetering(GasMeteringKind.Sync)
            moduleConfig.setStepTracing(true)

            val module = Module.fromBlob(engine, moduleConfig, blob).getOrThrow()
            val instance = module.instantiate().getOrThrow()

            // Init instance state
            instance.setGas(inputCase.initialGas)
            val programCounter = ProgramCounter(inputCase.initialPc)
            instance.setNextProgramCounter(programCounter)

            inputCase.initialRegs.forEachIndexed { index, value ->
                instance.setReg(Reg.fromRaw(index)!!, value.toULong())
            }

            var finalPc = inputCase.initialPc
            val actualStatus = run {
                while (true) {
                    val result = instance.run().getOrThrow()
                    when (result) {
                        InterruptKind.Finished -> return@run PvmStatus.HALT
                        InterruptKind.Trap -> return@run PvmStatus.TRAP
                        InterruptKind.NotEnoughGas -> return@run "out-of-gas"
                        InterruptKind.Step -> {
                            finalPc = instance.programCounter()!!.value
                            continue
                        }

                        is InterruptKind.Ecalli -> TODO()
                        is InterruptKind.Segfault -> TODO()
                    }
                }
            }
            if (actualStatus != PvmStatus.HALT) {
                finalPc = instance.programCounter()!!.value
            }

            // Validate status
            assertEquals(inputCase.expectedStatus, actualStatus, "Status mismatch.")

            // Validate output state
            assertEquals(inputCase.expectedPc, finalPc, "Program counter mismatch.")
            // Validate reg values
            inputCase.expectedRegs.forEachIndexed { index, value ->
                assertEquals(value, instance.reg(Reg.fromRaw(index)!!), "Register $index mismatch.")
            }
            // Validate memory update
            inputCase.initialMemory.forEachIndexed { index, memory ->
                val actualMemory = instance.readMemoryInto(memory.address, byteArrayOf(0))
                assertUIntListMatchesBytes(memory.contents, actualMemory)
            }
            assertEquals(inputCase.expectedGas, instance.gas(), "Gas mismatch.")
        }
    }
}
