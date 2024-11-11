package io.forge.jam.pvm.engine

import io.forge.jam.pvm.program.ProgramCounter
import io.forge.jam.pvm.program.Reg

typealias Target = UInt

/**
 * Visitor implementation for interpreting instructions
 */
class Visitor(
    val inner: InterpretedInstance
) {
    /**
     * Gets a 32-bit value from either a register or immediate
     */
    @Suppress("NOTHING_TO_INLINE")
    inline fun get32(regimm: RegImm): UInt = when (regimm) {
        is RegImm.RegValue -> inner.regs[regimm.reg.toIndex()].toUInt()
        is RegImm.ImmValue -> regimm.value
    }

    /**
     * Gets a 64-bit value from either a register or immediate
     */
    @Suppress("NOTHING_TO_INLINE")
    inline fun get64(regimm: RegImm): ULong = when (regimm) {
        is RegImm.RegValue -> inner.regs[regimm.reg.toIndex()]
        is RegImm.ImmValue -> {
            Cast(Cast(Cast(regimm.value).uintToSigned()).intToI64SignExtend()).longToUnsigned()
        }
    }

    /**
     * Moves to the next instruction
     */
    @Suppress("NOTHING_TO_INLINE")
    inline fun goToNextInstruction(): Target = inner.compiledOffset + 1u

    /**
     * Sets a 32-bit value in a register.
     */
    @Suppress("NOTHING_TO_INLINE")
    inline fun set32(dst: Reg, value: UInt) {
        // The chain of casts: u32 -> i32 -> i64 -> u64
        val finalValue = Cast(value)
            .uintToSigned()
            .let { Cast(it) }
            .intToI64SignExtend()
            .let { Cast(it) }
            .longToUnsigned()

        inner.regs[dst.toIndex()] = finalValue
    }

    /**
     * Sets a 64-bit value in a register
     */
    @Suppress("NOTHING_TO_INLINE")
    inline fun set64(dst: Reg, value: ULong) {
        inner.regs[dst.toIndex()] = value
    }

    /**
     * Performs a three-operand 32-bit operation
     */
    inline fun set3_32(
        dst: Reg,
        s1: RegImm,
        s2: RegImm,
        crossinline callback: (UInt, UInt) -> UInt
    ): Target {
        val v1 = get32(s1)
        val v2 = get32(s2)
        set32(dst, callback(v1, v2))
        return goToNextInstruction()
    }

    /**
     * Performs a three-operand 64-bit operation
     */
    inline fun set3_64(
        debug: Boolean,
        dst: Reg,
        s1: RegImm,
        s2: RegImm,
        crossinline callback: (ULong, ULong) -> ULong
    ): Target {
        val v1 = get64(s1)
        val v2 = get64(s2)
        set64(dst, callback(v1, v2))
        return goToNextInstruction()
    }

    /**
     * Performs a conditional branch
     */
    inline fun branch(
        debug: Boolean,
        s1: RegImm,
        s2: RegImm,
        targetTrue: Target,
        targetFalse: Target,
        crossinline callback: (ULong, ULong) -> Boolean
    ): Target {
        val v1 = get64(s1)
        val v2 = get64(s2)
        return if (callback(v1, v2)) targetTrue else targetFalse
    }

    /**
     * Handles segmentation faults
     */
    fun segfaultImpl(programCounter: ProgramCounter, pageAddress: UInt): Target? {
        inner.apply {
            this.programCounter = programCounter
            this.programCounterValid = true
            this.nextProgramCounter = programCounter
            this.interrupt = InterruptKind.Segfault(
                SegfaultInfo(
                    pageAddress = pageAddress,
                    pageSize = module.memoryMap().pageSize
                )
            )
        }
        return null
    }
}
