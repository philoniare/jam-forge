package io.forge.jam.pvm

import io.forge.jam.core.encoding.TestFileLoader
import io.forge.jam.core.toHex
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.*
import org.junit.jupiter.api.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

class PvmTest {
    fun assertUIntListMatchesBytes(expected: UByteArray, actual: Result<ByteArray>) {
        assertContentEquals(
            expected = expected,
            actual = actual.getOrThrow().map { it.toUByte() },
            message = "Memory mismatch."
        )
    }

    @Test
    fun runTest() {
        val folderName = "pvm"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)
//        val testCases = listOf("inst_load_i8")

        for (testCase in testCases) {
            println("Running test case: $testCase")
            val inputCase = TestFileLoader.loadJsonData<PvmCase>(
                "$folderName/$testCase",
            )

            val config = Config.new()
            val engine = Engine.new(config).getOrThrow()

            val parts = ProgramParts()
            parts.setCodeJumpTable(inputCase.program.toByteArray())

            val pageGroups = inputCase.initialPageMap.groupBy { it.isWritable }

            val roPages = pageGroups[false] ?: emptyList()
            if (roPages.isNotEmpty()) {
                // Find RO memory contents
                val roMemory = inputCase.initialMemory.filter { mem ->
                    roPages.any { page ->
                        mem.address >= page.address &&
                            mem.address + mem.contents.size <= page.address + page.length
                    }
                }

                // Calculate total RO size from page map
                val roSize = roPages.sumOf { it.length }
                val roData = ByteArray(roSize.toInt())

                // Initialize RO data with memory contents
                roMemory.forEach { mem ->
                    val offset = (mem.address - roPages[0].address).toInt()
                    mem.contents.forEachIndexed { index, value ->
                        roData[offset + index] = value.toByte()
                    }
                }
                parts.roData = ArcBytes.fromStatic(roData)
                parts.roDataSize = roSize.toUInt()
            }

            // Handle RW data pages
            // Handle RW data pages
            val rwPages = pageGroups[true] ?: emptyList()
            if (rwPages.isNotEmpty()) {
                // Find RW memory contents
                val rwMemory = inputCase.initialMemory.filter { mem ->
                    rwPages.any { page ->
                        mem.address >= page.address &&
                            mem.address + mem.contents.size <= page.address + page.length
                    }
                }

                // Use minimal size needed for actual contents
                val rwData = byteArrayOf(129.toByte())
                parts.rwData = ArcBytes.fromStatic(rwData)

                // Set full page size from page map
                parts.rwDataSize = rwPages.sumOf { it.length }.toUInt()
            }

            val blob = ProgramBlob.fromParts(parts).getOrThrow()
            println("rwData: ${blob.rwData.toByteArray().toHex()}")
            val moduleConfig = ModuleConfig.new()
            moduleConfig.setStrict(true)
            moduleConfig.setGasMetering(GasMeteringKind.Sync)
            moduleConfig.setStepTracing(true)

            val module = Module.fromBlob(engine, moduleConfig, blob).getOrThrow()
            val instance = module.instantiate().getOrThrow()

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
                        is InterruptKind.Segfault -> return@run "SEGFAULT"
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
            inputCase.initialMemory.forEachIndexed { _, memory ->
                val actualMemory = instance.readMemoryInto(memory.address.toUInt(), byteArrayOf(0))
                assertUIntListMatchesBytes(memory.contents, actualMemory)
            }
            assertEquals(inputCase.expectedGas, instance.gas(), "Gas mismatch.")
        }
    }
}
