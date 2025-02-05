package io.forge.jam.pvm

import io.forge.jam.core.encoding.TestFileLoader
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.*
import org.junit.jupiter.api.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

class PvmTest {
    fun assertUIntListMatchesBytes(expected: ByteArray, actual: ByteArray) {
        assertContentEquals(
            expected = expected,
            actual = actual,
            message = "Memory mismatch."
        )
    }

    @Test
    fun runTest() {
        val folderName = "pvm"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            println("Running test case: $testCase")
            val inputCase = TestFileLoader.loadJsonData<PvmCase>(
                "$folderName/$testCase",
            )

            val config = Config.new(false)
            val engine = Engine.new(config).getOrThrow()

            val parts = ProgramParts(is64Bit = true)
            parts.setCodeJumpTable(inputCase.program.toByteArray())

            val pageGroups = inputCase.initialPageMap.groupBy { it.isWritable }

            val roPages = pageGroups[false] ?: emptyList()
            if (roPages.isNotEmpty()) {
                // Find RO memory contents
                val roMemory = inputCase.initialMemory.filter { mem ->
                    roPages.any { page ->
                        mem.address >= page.address &&
                            mem.address.toInt() + mem.contents.size <= (page.address + page.length).toInt()
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
            val rwPages = pageGroups[true] ?: emptyList()
            if (rwPages.isNotEmpty()) {
                // Find RW memory contents
                val rwMemory = inputCase.initialMemory.filter { mem ->
                    rwPages.any { page ->
                        mem.address >= page.address &&
                            mem.address.toInt() + mem.contents.size <= (page.address + page.length).toInt()
                    }
                }

                // Initialize memory based on rwMemory contents
                if (rwMemory.isNotEmpty()) {
                    // Calculate size needed for memory contents
                    val minAddress = rwMemory.minOf { it.address }
                    val maxAddress = rwMemory.maxOf { it.address.toInt() + it.contents.size }
                    val totalSize = maxAddress - minAddress.toInt()
                    val rwData = ByteArray(totalSize.toInt())

                    // Copy contents from each memory segment
                    rwMemory.forEach { mem ->
                        val offset = (mem.address - minAddress).toInt()
                        mem.contents.forEachIndexed { index, value ->
                            rwData[offset + index] = value.toByte()
                        }
                    }
                    parts.rwData = ArcBytes.fromStatic(rwData)
                }

                // Set full page size from page map
                parts.rwDataSize = rwPages.sumOf { it.length }.toUInt()
            }

            val blob = ProgramBlob.fromParts(parts).getOrThrow()
            val moduleConfig = ModuleConfig.new(false)
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
            var pageFaultAddress = 0u
            val actualStatus = run {
                while (true) {
                    when (val result = instance.run().getOrThrow()) {
                        InterruptKind.Finished -> return@run PvmStatus.HALT
                        InterruptKind.Panic -> return@run PvmStatus.PANIC
                        is InterruptKind.Segfault -> {
                            pageFaultAddress = result.fault.pageAddress
                            return@run PvmStatus.PAGE_FAULT
                        }

                        InterruptKind.NotEnoughGas -> return@run "out-of-gas"
                        InterruptKind.Step -> {
                            finalPc = instance.programCounter()!!.value
                            continue
                        }

                        is InterruptKind.Ecalli -> TODO()

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
                // Create a buffer big enough for the expected memory contents
                val buffer = ByteArray(memory.contents.size)
                val actualMemory = instance.readMemoryInto(memory.address.toUInt(), buffer)
                assertUIntListMatchesBytes(buffer, actualMemory.getOrThrow())
            }
            assertEquals(inputCase.expectedGas, instance.gas(), "Gas mismatch.")
            inputCase.expectedPageFaultAddress?.let {
                if (it != 0u) {
                    assertEquals(it, pageFaultAddress, "Page fault address mismatch.")
                }
            }
        }
    }
}
