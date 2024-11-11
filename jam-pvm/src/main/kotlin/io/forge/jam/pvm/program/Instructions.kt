package io.forge.jam.pvm.program

import io.forge.jam.pvm.PvmLogger
import io.forge.jam.pvm.engine.InstructionSet

data class Instructions<I : InstructionSet>(
    private val code: ByteArray,
    private val bitmask: ByteArray,
    var offset: UInt = 0u,
    private var invalidOffset: UInt? = null,
    private var isBounded: Boolean = false,
    private var isDone: Boolean = false,
    private val instructionSet: I
) : Iterator<ParsedInstruction>, Cloneable {
    override fun hasNext(): Boolean {
        return !isDone
    }

    override fun next(): ParsedInstruction {
        invalidOffset?.let { invalid ->
            invalidOffset = null
            return ParsedInstruction(
                kind = Instruction.Invalid,
                offset = ProgramCounter(invalid),
                nextOffset = ProgramCounter(offset)
            )
        }

        if (isDone || offset.toUInt() >= code.size.toUInt()) {
            throw NoSuchElementException("No more instructions")
        }

        val currentOffset = offset

        require(Program.getBitForOffset(bitmask, code.size, currentOffset)) {
            "bit at $currentOffset is zero"
        }

        val (nextOffset, instruction, isNextInstructionInvalid) =
            Program.parseInstruction(instructionSet, code, bitmask, currentOffset)
        logger.debug("next_offset: $nextOffset, instruction: $instruction, isNextInstructionInvalid: $isNextInstructionInvalid")

        require(nextOffset > currentOffset) {
            "assertion failed: next_offset > self.offset"
        }

        if (!isNextInstructionInvalid) {
            offset = nextOffset
            require(
                offset == code.size.toUInt() ||
                    Program.getBitForOffset(bitmask, code.size, offset)
            ) { "bit at $offset is zero" }
        } else {
            if (nextOffset.toUInt() == code.size.toUInt()) {
                offset = code.size.toUInt() + 1u
            } else if (isBounded) {
                isDone = true
                if (instruction.opcode().canFallthrough()) {
                    offset = code.size.toUInt()
                } else {
                    offset = nextOffset
                }
            } else {
                offset = Program.findNextOffsetUnbounded(bitmask, code.size.toUInt(), nextOffset)
                require(
                    offset.toUInt() == code.size.toUInt() ||
                        Program.getBitForOffset(bitmask, code.size, offset)
                ) { "bit at $offset is zero" }
            }

            if (instruction.opcode().canFallthrough()) {
                invalidOffset = nextOffset
            }
        }

        return ParsedInstruction(
            kind = instruction,
            offset = ProgramCounter(currentOffset),
            nextOffset = ProgramCounter(nextOffset)
        )
    }


    companion object {
        private val logger = PvmLogger(Instructions::class.java)

        fun <I : InstructionSet> new(
            instructionSet: I,
            code: ByteArray,
            bitmask: ByteArray,
            offset: UInt,
            isBounded: Boolean
        ): Instructions<I> {
            require(code.size <= Int.MAX_VALUE && code.size.toUInt() <= UInt.MAX_VALUE) {
                "Code size exceeds maximum value"
            }
            require(bitmask.size == (code.size + 7) / 8) {
                "Bitmask size does not match code size"
            }

            val isValid = Program.getBitForOffset(bitmask, code.size, offset)
            var isDone = false
            val (finalOffset, invalidOffset) = if (isValid) {
                offset to null
            } else if (isBounded) {
                isDone = true
                minOf(offset + 1u, code.size.toUInt()) to offset
            } else {
                val nextOffset = Program.findNextOffsetUnbounded(bitmask, code.size.toUInt(), offset)
                nextOffset to offset
            }

            return Instructions(
                code = code,
                bitmask = bitmask,
                offset = finalOffset,
                invalidOffset = invalidOffset,
                isBounded = isBounded,
                isDone = isDone,
                instructionSet = instructionSet
            )
        }
    }

    override fun clone(): Instructions<I> = Instructions(
        code = code.clone(),
        bitmask = bitmask.clone(),
        offset = offset,
        invalidOffset = invalidOffset,
        isBounded = isBounded,
        isDone = isDone,
        instructionSet = instructionSet
    )
}
