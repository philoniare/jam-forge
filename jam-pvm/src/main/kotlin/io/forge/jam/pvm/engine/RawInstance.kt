package io.forge.jam.pvm.engine

import io.forge.jam.pvm.program.ProgramCounter
import io.forge.jam.pvm.program.Reg

class RawInstance(
    private val module: Module,
    private val backend: InstanceBackend,
    private val crosscheckInstance: InterpretedInstance? = null
) {
    fun module(): Module = module

    fun run(): Result<InterruptKind> {
        if (nextProgramCounter() == null) {
            return Result.failure(PvmError.fromStaticStr("failed to run: next program counter is not set"))
        }

        if (gas() < 0) {
            return Result.success(InterruptKind.NotEnoughGas)
        }

        while (true) {

            val interruption =
                backend.access { backend ->
                    backend.run()
                        .getOrElse { error -> return Result.failure(PvmError.fromDisplay("execution failed: $error")) }
                }

            crosscheckInstance?.let { crosscheck ->
                val isStep = interruption == InterruptKind.Step
                val expectedInterruption = crosscheck.run()
                    .getOrElse { throw Exception("crosscheck failed") }

                require(interruption == expectedInterruption) {
                    "run: crosscheck mismatch, interpreter = $expectedInterruption, backend = $interruption"
                }

                val crosscheckGas = crosscheck.gas()
                val crosscheckProgramCounter = crosscheck.programCounter()
                val crosscheckNextProgramCounter = crosscheck.nextProgramCounter()

                if (module.gasMetering() == GasMeteringKind.Sync) {
                    require(gas() == crosscheckGas)
                }

                require(programCounter() == crosscheckProgramCounter)
                require(nextProgramCounter() == crosscheckNextProgramCounter)

                if (isStep && !module.state().stepTracing) {
                    continue
                }
            }

            if (gas() < 0) {
                return Result.success(InterruptKind.NotEnoughGas)
            }

            return Result.success(interruption)
        }
    }

    fun reg(reg: Reg): ULong = backend.access { backend -> backend.reg(reg) }

    fun setReg(reg: Reg, value: ULong) {
        crosscheckInstance?.setReg(reg, value)
        backend.access { backend -> backend.setReg(reg, value) }
    }

    fun gas(): Long = backend.access { backend -> backend.gas() }

    fun setGas(gas: Long) {
        crosscheckInstance?.setGas(gas)
        backend.access { backend -> backend.setGas(gas) }
    }

    fun programCounter(): ProgramCounter? = backend.access { backend -> backend.programCounter() }

    fun nextProgramCounter(): ProgramCounter? = backend.access { backend -> backend.nextProgramCounter() }

    fun setNextProgramCounter(pc: ProgramCounter) {
        crosscheckInstance?.setNextProgramCounter(pc)
        backend.access { backend -> backend.setNextProgramCounter(pc) }
    }

    fun clearRegs() {
        for (reg in Reg.ALL) {
            setReg(reg, 0u)
        }
    }

    fun resetMemory(): Result<Unit> {
        crosscheckInstance?.resetMemory()
        return backend.access { backend -> backend.resetMemory() }
    }

    fun readMemoryInto(address: UInt, buffer: ByteArray): Result<ByteArray> {
        if (buffer.isEmpty()) {
            return Result.success(ByteArray(0))
        }

        if (address < 0x10000u) {
            return Result.failure(MemoryAccessError.outOfRangeAccess(address, buffer.size.toULong()))
        }

        if (address.toULong() + buffer.size.toULong() > 0x100000000u) {
            return Result.failure(MemoryAccessError.outOfRangeAccess(address, buffer.size.toULong()))
        }

        val result = backend.access { backend -> backend.readMemoryInto(address, buffer) }

        crosscheckInstance?.let { crosscheck ->
            val expectedData = ByteArray(buffer.size) { 0xFA.toByte() }
            val expectedResult = crosscheck.readMemoryInto(address, expectedData)

            val expected = expectedResult.isSuccess
            val actual = result.isSuccess

            require(expected == actual) {
                val addressEnd = address.toULong() + buffer.size.toULong()
                "read_memory: crosscheck mismatch, range = 0x${address.toString(16)}..0x${addressEnd.toString(16)}, " +
                    "interpreter = $expected, backend = $actual"
            }
        }

        return result
    }

    fun writeMemory(address: UInt, data: ByteArray): Result<Unit> {
        if (data.isEmpty()) {
            return Result.success(Unit)
        }

        if (address < 0x10000u) {
            return Result.failure(MemoryAccessError.outOfRangeAccess(address, data.size.toULong()))
        }

        if (address.toULong() + data.size.toULong() > 0x100000000u) {
            return Result.failure(MemoryAccessError.outOfRangeAccess(address, data.size.toULong()))
        }

        val result = backend.writeMemory(address, data)

        crosscheckInstance?.let { crosscheck ->
            val expectedResult = crosscheck.writeMemory(address, data)
            val expected = expectedResult.isSuccess
            val actual = result.isSuccess

            require(expected == actual) {
                val addressEnd = address.toULong() + data.size.toULong()
                "write_memory: crosscheck mismatch, range = 0x${address.toString(16)}..0x${addressEnd.toString(16)}, " +
                    "interpreter = $expected, backend = $actual"
            }
        }

        return result
    }

    fun prepareCallUntyped(pc: ProgramCounter, args: List<RegValue>) {
        require(args.size <= Reg.ARG_REGS.size) { "too many arguments" }

        clearRegs()
        setReg(Reg.SP, module.defaultSp())
        setReg(Reg.RA, VM_ADDR_RETURN_TO_HOST.toULong())
        setNextProgramCounter(pc)

        args.forEachIndexed { index, value ->
            setReg(Reg.ARG_REGS[index], value)
        }
    }

    fun heapSize(): UInt = backend.heapSize()

    fun sbrk(size: UInt): Result<UInt?> {
        val result = backend.sbrk(size)

        crosscheckInstance?.let { crosscheck ->
            val expectedResult = crosscheck.sbrk(size)
            val expected = expectedResult.isSuccess
            val actual = result.isSuccess

            require(expected == actual) {
                "sbrk: crosscheck mismatch, size = $size, interpreter = $expected, backend = $actual"
            }
        }

        return result
    }

    fun pid(): UInt? = backend.pid()

    fun nextNativeProgramCounter(): ULong? = backend.nextNativeProgramCounter()
}
