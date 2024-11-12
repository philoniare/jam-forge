package io.forge.jam.pvm

import io.forge.jam.pvm.engine.RuntimeInstructionSet
import io.forge.jam.pvm.program.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import kotlin.test.assertEquals

class InstructionsTest {

    private val defaultInstructionSet = RuntimeInstructionSet(
        allowSbrk = true,
        is64Bit = false
    )

    @Test
    fun `test instructions iterator with implicit trap`() {
        listOf(false, true).forEach { isBounded ->
            val instructions = Instructions.new(
                instructionSet = defaultInstructionSet,
                code = byteArrayOf(Opcode.fallthrough.value.toByte()),
                bitmask = byteArrayOf(0b00000001),
                offset = 0u,
                isBounded = isBounded
            )

            // First instruction
            assertEquals(
                expected = ParsedInstruction(
                    kind = Instruction.Fallthrough,
                    offset = ProgramCounter(0u),
                    nextOffset = ProgramCounter(1u)
                ),
                actual = instructions.next()
            )

            // Invalid instruction
            assertEquals(
                expected = ParsedInstruction(
                    kind = Instruction.Invalid,
                    offset = ProgramCounter(1u),
                    nextOffset = ProgramCounter(2u)
                ),
                actual = instructions.next()
            )

            assertThrows<NoSuchElementException> { instructions.next() }
        }
    }

    @Test
    fun `test instructions iterator without implicit trap`() {
        listOf(false, true).forEach { isBounded ->
            val instructions = Instructions.new(
                instructionSet = defaultInstructionSet,
                code = byteArrayOf(Opcode.trap.value.toByte()),
                bitmask = byteArrayOf(0b00000001),
                offset = 0u,
                isBounded = isBounded
            )

            // First instruction
            assertEquals(
                expected = ParsedInstruction(
                    kind = Instruction.Trap,
                    offset = ProgramCounter(0u),
                    nextOffset = ProgramCounter(1u)
                ),
                actual = instructions.next()
            )

            // No more instructions
            assertThrows<NoSuchElementException> { instructions.next() }
        }
    }

    @Test
    fun `test instructions iterator very long bitmask bounded`() {
        val code = ByteArray(64) { 0 }.apply {
            this[0] = Opcode.fallthrough.value.toByte()
        }
        val bitmask = ByteArray(8) { 0 }.apply {
            this[0] = 0b00000001
            this[7] = 0b10000000.toByte()
        }

        val instructions = Instructions.new(
            instructionSet = defaultInstructionSet,
            code = code,
            bitmask = bitmask,
            offset = 0u,
            isBounded = true
        )

        assertEquals(
            expected = ParsedInstruction(
                kind = Instruction.Fallthrough,
                offset = ProgramCounter(0u),
                nextOffset = ProgramCounter(25u)
            ),
            actual = instructions.next()
        )

        assertEquals(
            expected = ParsedInstruction(
                kind = Instruction.Invalid,
                offset = ProgramCounter(25u),
                nextOffset = ProgramCounter(64u)
            ),
            actual = instructions.next()
        )

        assertThrows<NoSuchElementException> { instructions.next() }
    }

    @Test
    fun `test instructions iterator very long bitmask unbounded`() {
        val code = ByteArray(64) { 0 }.apply {
            this[0] = Opcode.fallthrough.value.toByte()
        }
        val bitmask = ByteArray(8) { 0 }.apply {
            this[0] = 0b00000001
            this[7] = 0b10000000.toByte()
        }

        val instructions = Instructions.new(
            instructionSet = defaultInstructionSet,
            code = code,
            bitmask = bitmask,
            offset = 0u,
            isBounded = false
        )

        assertEquals(
            expected = ParsedInstruction(
                kind = Instruction.Fallthrough,
                offset = ProgramCounter(0u),
                nextOffset = ProgramCounter(25u)
            ),
            actual = instructions.next()
        )

        assertEquals(
            expected = ParsedInstruction(
                kind = Instruction.Invalid,
                offset = ProgramCounter(25u),
                nextOffset = ProgramCounter(63u)
            ),
            actual = instructions.next()
        )

        assertEquals(
            expected = ParsedInstruction(
                kind = Instruction.Trap,
                offset = ProgramCounter(63u),
                nextOffset = ProgramCounter(64u)
            ),
            actual = instructions.next()
        )

        assertThrows<NoSuchElementException> { instructions.next() }
    }

    @Test
    fun `test instructions iterator start at invalid offset bounded`() {
        val instructions = Instructions.new(
            instructionSet = defaultInstructionSet,
            code = ByteArray(8) {
                Opcode.trap.value.toByte()
            },
            bitmask = byteArrayOf(0b10000001.toByte()),
            offset = 1u,
            isBounded = true
        )

        assertEquals(
            expected = ParsedInstruction(
                kind = Instruction.Invalid,
                offset = ProgramCounter(1u),
                nextOffset = ProgramCounter(2u)
            ),
            actual = instructions.next()
        )

        assertThrows<NoSuchElementException> { instructions.next() }
    }

    @Test
    fun `test instructions iterator start at invalid offset unbounded`() {
        val instructions = Instructions.new(
            instructionSet = defaultInstructionSet,
            code = ByteArray(8) { Opcode.trap.value.toByte() },
            bitmask = byteArrayOf(0b10000001.toByte()),
            offset = 1u,
            isBounded = false
        )

        assertEquals(
            expected = ParsedInstruction(
                kind = Instruction.Invalid,
                offset = ProgramCounter(1u),
                nextOffset = ProgramCounter(7u)
            ),
            actual = instructions.next()
        )

        assertEquals(
            expected = ParsedInstruction(
                kind = Instruction.Trap,
                offset = ProgramCounter(7u),
                nextOffset = ProgramCounter(8u)
            ),
            actual = instructions.next()
        )

        assertThrows<NoSuchElementException> { instructions.next() }
    }

    @Test
    fun `test instructions iterator does not emit unnecessary invalid instructions if bounded and ends with trap`() {
        val code = ByteArray(32) { Opcode.trap.value.toByte() }
        val bitmask = byteArrayOf(0b00000001, 0b00000000, 0b00000000, 0b00000100)

        val instructions = Instructions.new(
            instructionSet = defaultInstructionSet,
            code = code,
            bitmask = bitmask,
            offset = 0u,
            isBounded = true
        )

        assertEquals(0u, instructions.offset)
        assertEquals(
            expected = ParsedInstruction(
                kind = Instruction.Trap,
                offset = ProgramCounter(0u),
                nextOffset = ProgramCounter(25u)
            ),
            actual = instructions.next()
        )
        assertEquals(25u, instructions.offset)
        assertThrows<NoSuchElementException> { instructions.next() }
    }

    @Test
    fun `test instructions iterator does not emit unnecessary invalid instructions if unbounded and ends with trap`() {
        val code = ByteArray(32) { Opcode.trap.value.toByte() }
        val bitmask = byteArrayOf(0b00000001, 0b00000000, 0b00000000, 0b00000100)

        val instructions = Instructions.new(
            instructionSet = defaultInstructionSet,
            code = code,
            bitmask = bitmask,
            offset = 0u,
            isBounded = false
        )

        assertEquals(0u, instructions.offset)
        assertEquals(
            expected = ParsedInstruction(
                kind = Instruction.Trap,
                offset = ProgramCounter(0u),
                nextOffset = ProgramCounter(25u)
            ),
            actual = instructions.next()
        )
        assertEquals(26u, instructions.offset)
        assertEquals(
            expected = ParsedInstruction(
                kind = Instruction.Trap,
                offset = ProgramCounter(26u),
                nextOffset = ProgramCounter(32u)
            ),
            actual = instructions.next()
        )
        assertThrows<NoSuchElementException> { instructions.next() }
    }
}
