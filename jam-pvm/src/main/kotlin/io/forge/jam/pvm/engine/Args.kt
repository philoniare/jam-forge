package io.forge.jam.pvm.engine

import io.forge.jam.pvm.program.ProgramCounter
import io.forge.jam.pvm.program.RawReg
import io.forge.jam.pvm.program.toU32

/**
 * Arguments structure for VM instructions.
 */
data class Args(
    val a0: UInt = 0u,
    val a1: UInt = 0u,
    val a2: UInt = 0u,
    val a3: UInt = 0u
) {
    companion object {
        /**
         * Creates a default Args instance with all values set to 0
         */
        fun default(): Args = Args()

        fun trap(programCounter: ProgramCounter) = Args(a0 = programCounter.value)

        fun step(programCounter: ProgramCounter) = Args(a0 = programCounter.value)

        fun chargeGas(programCounter: ProgramCounter, gasCost: UInt) = Args(
            a0 = programCounter.value,
            a1 = gasCost
        )

        fun stepOutOfRange() = Args()
        fun outOfRange(gasCost: UInt) = Args(a0 = gasCost)

        fun moveReg(d: RawReg, s: RawReg) = Args(a0 = d.toU32(), a1 = s.toU32())

        fun add32(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun addImm32(a0: RawReg, a1: RawReg, imm: UInt) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = imm
        )
    }
}
