package io.forge.jam.pvm.program

import io.forge.jam.pvm.engine.InstructionSet

interface OpcodeVisitor<R, I : InstructionSet> {
    val state: S
    val instructionSet: I
    fun dispatch(opcode: UInt, chunk: ULong, offset: UInt, skip: UInt): R
}
