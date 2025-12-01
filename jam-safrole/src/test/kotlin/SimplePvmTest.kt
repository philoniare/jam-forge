package io.forge.jam.core.encoding

import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.*
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.nio.ByteBuffer
import java.nio.ByteOrder

/**
 * Tests for PVM execution with standard programs.
 */
class SimplePvmTest {

    private val fibonacci = byteArrayOf(
        0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 61, 0, 0, 0, 0, 0, 51, 128.toByte(), 119, 0,
        51, 8, 1, 51, 9, 1, 40, 3, 0, 149.toByte(), 119, 255.toByte(), 81, 7, 12, 100, 138.toByte(), 200.toByte(),
        152.toByte(), 8, 100, 169.toByte(), 40, 243.toByte(), 100, 135.toByte(), 51, 8, 51, 9, 61, 7, 0, 0, 2, 0,
        51, 8, 4, 51, 7, 0, 0, 2, 0, 1, 50, 0, 73, 154.toByte(), 148.toByte(), 170.toByte(), 130.toByte(), 4, 3,
    )

    private val sumToN = byteArrayOf(
        0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 46, 0, 0, 0, 0, 0, 38, 128.toByte(), 119, 0,
        51, 8, 0, 100, 121, 40, 3, 0, 200.toByte(), 137.toByte(), 8, 149.toByte(), 153.toByte(), 255.toByte(),
        86, 9, 250.toByte(), 61, 8, 0, 0, 2, 0, 51, 8, 4, 51, 7, 0, 0, 2, 0, 1, 50, 0, 73, 77, 18, 36, 24,
    )

    // Empty program - just triggers trap
    private val empty = byteArrayOf(
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0
    )

    // sumToN program with host call (ecalli instruction)
    // The ecalli instruction triggers an external call that we can handle
    private val sumToNWithHostCall = byteArrayOf(
        0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 48, 0, 0, 0, 0, 0, 40, 128.toByte(), 119, 0,
        51, 8, 0, 100, 121, 40, 3, 0, 200.toByte(), 137.toByte(), 8, 149.toByte(), 153.toByte(), 255.toByte(),
        86, 9, 250.toByte(), 61, 8, 0, 0, 2, 0, 51, 8, 4, 51, 7, 0, 0, 2, 0, 10, 1, 1, 50, 0, 73,
        77, 18, 36, 104,
    )


    @Test
    fun testFibonacci() {
        val testCases = listOf(
            Triple(2, 2, 999_980L),
            Triple(8, 34, 999_944L),
            Triple(9, 55, 999_938L)
        )

        for ((input, output, _) in testCases) {
            println("Testing Fibonacci with input: $input")
            val result = invokePVM(fibonacci, byteArrayOf(input.toByte()))
            if (result.exitReason != InterruptKind.Finished) {
                println("Fibonacci failed with exit reason: ${result.exitReason}")
            }
            assertEquals(InterruptKind.Finished, result.exitReason)

            val outputValue = ByteBuffer.wrap(result.output).order(ByteOrder.LITTLE_ENDIAN).int
            assertEquals(output, outputValue)
            println("Passed: Input=$input, Output=$outputValue")
        }
    }


    @Test
    fun testSumToN() {
        val testCases = listOf(
            Triple(1, 1, 999_988L),
            Triple(4, 10, 999_979L),
            Triple(5, 15, 999_976L)
        )

        for ((input, output, _) in testCases) {
            println("Testing SumToN with input: $input")
            val result = invokePVM(sumToN, byteArrayOf(input.toByte()))
            assertEquals(InterruptKind.Finished, result.exitReason)

            val outputValue = ByteBuffer.wrap(result.output).order(ByteOrder.LITTLE_ENDIAN).int
            assertEquals(output, outputValue)
            println("Passed: Input=$input, Output=$outputValue")
        }
    }

    /**
     * Test host call invocation context.
     *
     * The sumToNWithHostCall program computes sum(1..n) and then calls ecalli(1)
     * which triggers a host call. The host call handler reads the output value,
     * doubles it (value << 1), and writes it back.
     *
     * For input 5: sum(1..5) = 15, then host call doubles it to 30.
     */
    @Test
    fun testInvocationContext() {
        val result = invokePVMWithHostCall(
            blobBytes = sumToNWithHostCall,
            argumentData = byteArrayOf(5),
            hostCallHandler = { instance, ecalliIndex ->
                // Host call handler: perform output * 2
                // Read output address and length from registers A0 (r7) and A1 (r8)
                val outputAddr = instance.reg(Reg.A0).toUInt()
                val outputLen = instance.reg(Reg.A1).toUInt()

                // Read current output value
                val outputBuffer = ByteArray(outputLen.toInt())
                instance.readMemoryInto(outputAddr, outputBuffer).getOrThrow()
                val value = ByteBuffer.wrap(outputBuffer).order(ByteOrder.LITTLE_ENDIAN).int

                // Double the value (value << 1)
                val newValue = value shl 1
                val newOutputBuffer = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(newValue).array()

                // Write back the doubled value
                instance.writeMemory(outputAddr, newOutputBuffer).getOrThrow()

                // Return true to continue execution
                true
            }
        )

        assertEquals(InterruptKind.Finished, result.exitReason)
        val outputValue = ByteBuffer.wrap(result.output).order(ByteOrder.LITTLE_ENDIAN).int
        assertEquals(30, outputValue) // sum(1..5) = 15, doubled = 30
        println("Host call test passed: Input=5, Output=$outputValue (15 doubled)")
    }

    data class PvmResult(val exitReason: InterruptKind, val output: ByteArray, val remainingGas: Long)

    /**
     * Invoke PVM with a JAM format blob.
     * Parses the JAM blob format and executes the program.
     */
    private fun invokePVM(blobBytes: ByteArray, argumentData: ByteArray): PvmResult {
        val config = ModuleConfig.new(dynamicPaging = false)
        config.setGasMetering(GasMeteringKind.Sync)
        config.setPageSize(4096u)
        // 16MB + 128KB to cover standard PolkaVM layout
        config.setAuxDataSize(16908288u)

        val engine = Engine(
            selectedBackend = BackendKind.Interpreter,
            selectedSandbox = null,
            interpreterEnabled = true,
            crosscheck = false,
            state = EngineState(null, null),
            allowDynamicPaging = false
        )

        // Parse JAM blob format
        val parts = ProgramParts.fromJamBytes(ArcBytes.fromStatic(blobBytes)).getOrThrow()
        val blob = ProgramBlob.fromParts(parts).getOrThrow()

        val module = Module.fromBlob(engine, config, blob).getOrThrow()
        val instance = module.instantiate().getOrThrow()

        // Standard PolkaVM Initialization Logic
        val RA_INIT = 0xFFFF0000uL
        val SP_INIT = 0xFEFE0000uL
        val INPUT_ADDR = 0xFEFF0000u

        // Write input to high memory
        instance.writeMemory(INPUT_ADDR, argumentData, isExternal = true).getOrThrow()

        instance.setReg(Reg.RA, RA_INIT)
        instance.setReg(Reg.SP, SP_INIT)
        instance.setReg(Reg.A0, INPUT_ADDR.toULong())
        instance.setReg(Reg.A1, argumentData.size.toULong())

        instance.setNextProgramCounter(ProgramCounter(0u))
        instance.setGas(1_000_000L)

        val result = instance.run()
        val exitReason = result.getOrThrow()

        var output = ByteArray(0)
        if (exitReason == InterruptKind.Finished) {
            val addr = instance.reg(Reg.A0).toUInt()
            val len = instance.reg(Reg.A1).toUInt()
            val buffer = ByteArray(len.toInt())
            instance.readMemoryInto(addr, buffer).getOrThrow()
            output = buffer
        }

        return PvmResult(exitReason, output, instance.gas())
    }

    /**
     * Invoke PVM with host call support.
     * When ecalli instruction is executed, the hostCallHandler is invoked.
     * If it returns true, execution continues; if false, execution stops with panic.
     */
    private fun invokePVMWithHostCall(
        blobBytes: ByteArray,
        argumentData: ByteArray,
        hostCallHandler: (RawInstance, UInt) -> Boolean
    ): PvmResult {
        val config = ModuleConfig.new(dynamicPaging = false)
        config.setGasMetering(GasMeteringKind.Sync)
        config.setPageSize(4096u)
        config.setAuxDataSize(16908288u)

        val engine = Engine(
            selectedBackend = BackendKind.Interpreter,
            selectedSandbox = null,
            interpreterEnabled = true,
            crosscheck = false,
            state = EngineState(null, null),
            allowDynamicPaging = false
        )

        val parts = ProgramParts.fromJamBytes(ArcBytes.fromStatic(blobBytes)).getOrThrow()
        val blob = ProgramBlob.fromParts(parts).getOrThrow()

        val module = Module.fromBlob(engine, config, blob).getOrThrow()
        val instance = module.instantiate().getOrThrow()

        val RA_INIT = 0xFFFF0000uL
        val SP_INIT = 0xFEFE0000uL
        val INPUT_ADDR = 0xFEFF0000u

        instance.writeMemory(INPUT_ADDR, argumentData, isExternal = true).getOrThrow()

        instance.setReg(Reg.RA, RA_INIT)
        instance.setReg(Reg.SP, SP_INIT)
        instance.setReg(Reg.A0, INPUT_ADDR.toULong())
        instance.setReg(Reg.A1, argumentData.size.toULong())

        instance.setNextProgramCounter(ProgramCounter(0u))
        instance.setGas(1_000_000L)

        // Run with host call handling loop
        var exitReason: InterruptKind
        while (true) {
            val result = instance.run()
            exitReason = result.getOrThrow()

            when (exitReason) {
                is InterruptKind.Ecalli -> {
                    // Host call triggered - invoke handler
                    val ecalliIndex = (exitReason as InterruptKind.Ecalli).value
                    val shouldContinue = hostCallHandler(instance, ecalliIndex)
                    if (!shouldContinue) {
                        exitReason = InterruptKind.Panic
                        break
                    }
                    // Continue execution - the next PC should already be set by the interpreter
                }

                else -> break
            }
        }

        var output = ByteArray(0)
        if (exitReason == InterruptKind.Finished) {
            val addr = instance.reg(Reg.A0).toUInt()
            val len = instance.reg(Reg.A1).toUInt()
            val buffer = ByteArray(len.toInt())
            instance.readMemoryInto(addr, buffer).getOrThrow()
            output = buffer
        }

        return PvmResult(exitReason, output, instance.gas())
    }
}
