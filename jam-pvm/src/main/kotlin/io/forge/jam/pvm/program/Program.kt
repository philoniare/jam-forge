package io.forge.jam.pvm.program

import io.forge.jam.pvm.PvmLogger
import io.forge.jam.pvm.U128
import io.forge.jam.pvm.engine.InstructionSet
import io.forge.jam.pvm.engine.RuntimeInstructionSet
import io.forge.jam.pvm.readSimpleVarint

/**
 * Static container for program-wide operations and lookup tables.
 */
class Program {
    companion object {
        private val logger = PvmLogger(Program::class.java)
        const val BITMASK_MAX: UInt = 24u
        private const val INVALID_INSTRUCTION_INDEX: UInt = 256u
        val TABLE_1: LookupTable = LookupTable.build(1)
        val TABLE_2: LookupTable = LookupTable.build(2)

        fun <I : InstructionSet> parseInstruction(
            instructionSet: I,
            code: ByteArray,
            bitmask: ByteArray,
            offset: UInt
        ): Triple<UInt, Instruction, Boolean> {
            val visitor = EnumVisitor(instructionSet)
            return if (offset.toInt() + 32 <= code.size) {
                visitorStepFast(code, bitmask, offset, visitor)
            } else {
                visitorStepSlow(code, bitmask, offset, visitor)
            }
        }

        fun <I : InstructionSet> visitorStepFast(
            code: ByteArray,
            bitmask: ByteArray,
            offset: UInt,
            opcodeVisitor: OpcodeVisitor<Instruction, I>
        ): Triple<UInt, Instruction, Boolean> {
            assert(code.size <= Int.MAX_VALUE && code.size.toUInt() <= UInt.MAX_VALUE) { "Code size exceeds maximum allowed" }
            assert(bitmask.size == (code.size + 7) / 8) { "Invalid bitmask size" }
            assert(offset.toInt() <= code.size) { "Offset exceeds code size" }
            assert(getBitForOffset(bitmask, code.size, offset)) { "bit at $offset is zero" }
            assert(offset.toInt() + 32 <= code.size) { "Insufficient code size for fast path" }

            // Get chunk of code
            val chunk = code.slice(offset.toInt() until offset.toInt() + 32)
            val skip = parseBitmaskFast(bitmask, offset) ?: throw IllegalStateException("Failed to parse bitmask")
            val opcode = chunk[0].toInt() and 0xFF

            // Create U128 from 16 bytes in little-endian order
            val chunkValue = U128.fromLEBytes(chunk.toByteArray(), 1)

            assert(skip <= BITMASK_MAX.toUInt()) { "Skip value exceeds maximum allowed" }
            assert(
                opcodeVisitor.instructionSet.opcodeFromU8(opcode.toUByte()) != null ||
                    !isJumpTargetValid(opcodeVisitor.instructionSet, code, bitmask, offset + skip + 1u)
            )

            val nextOffset = offset + skip + 1u
            val isNextInstructionInvalid = skip == 24u && !getBitForOffset(bitmask, code.size, nextOffset)

            return Triple(
                nextOffset,
                opcodeVisitor.dispatch(
                    opcode = opcode.toUInt(),
                    chunk = chunkValue,
                    offset = offset,
                    skip = skip
                ),
                isNextInstructionInvalid
            )
        }

        fun <I : InstructionSet> visitorStepSlow(
            code: ByteArray,
            bitmask: ByteArray,
            offset: UInt,
            opcodeVisitor: OpcodeVisitor<Instruction, I>
        ): Triple<UInt, Instruction, Boolean> {
            if (offset.toInt() >= code.size) {
                return Triple(offset + 1u, visitorStepInvalidInstruction(offset, opcodeVisitor), true)
            }

            assert(code.size <= Int.MAX_VALUE && code.size.toUInt() <= UInt.MAX_VALUE) { "Code size exceeds maximum allowed" }
            assert(bitmask.size == (code.size + 7) / 8) { "Invalid bitmask size" }
            assert(offset.toInt() <= code.size) { "Offset exceeds code size" }
            assert(getBitForOffset(bitmask, code.size, offset)) { "bit at $offset is zero" }

            val (skip, isNextInstructionInvalid) = parseBitmaskSlow(bitmask, code.size.toUInt(), offset)
            val chunkSize = minOf(offset.toInt() + 17, code.size) - offset.toInt()
            val chunk = code.slice(offset.toInt() until (offset.toInt() + chunkSize))
            val opcode = chunk[0].toInt() and 0xFF

            var finalIsNextInstructionInvalid = isNextInstructionInvalid
            if (isNextInstructionInvalid && offset.toInt() + skip.toInt() + 1 >= code.size) {
                // Last instruction handling
                opcodeVisitor.instructionSet.opcodeFromU8(opcode.toUByte())?.let { opcodeValue ->
                    if (!opcodeValue.canFallthrough()) {
                        finalIsNextInstructionInvalid = false
                    }
                }
            }

            val t = ByteArray(16)
            chunk.drop(1).take(16).forEachIndexed { index, byte ->
                t[index] = byte
            }

            val chunkValue = U128.fromLEBytes(t)

            assert(
                opcodeVisitor.instructionSet.opcodeFromU8(opcode.toUByte()) != null ||
                    !isJumpTargetValid(opcodeVisitor.instructionSet, code, bitmask, offset + skip + 1u)
            )
            return Triple(
                offset + skip + 1u,
                opcodeVisitor.dispatch(opcode.toUInt(), chunkValue, offset, skip),
                finalIsNextInstructionInvalid
            )
        }

        fun parseBitmaskSlow(
            bitmask: ByteArray,
            codeLength: UInt,
            offset: UInt
        ): Pair<UInt, Boolean> {
            var currentOffset = offset.toInt() + 1
            var isNextInstructionInvalid = true
            val origin = currentOffset

            while (true) {
                val byteIndex = currentOffset shr 3
                if (byteIndex >= bitmask.size) break

                val byte = bitmask[byteIndex]
                val shift = currentOffset and 7
                val mask = (byte.toInt() and 0xFF) shr shift

                if (mask == 0) {
                    currentOffset += 8 - shift
                    if ((currentOffset - origin) < BITMASK_MAX.toInt()) {
                        continue
                    }
                } else {
                    currentOffset += mask.countTrailingZeroBits()
                    isNextInstructionInvalid = currentOffset >= codeLength.toInt() ||
                        (currentOffset - origin) > BITMASK_MAX.toInt()
                }
                break
            }

            val finalOffset = minOf(currentOffset, codeLength.toInt())
            val skip = minOf((finalOffset - origin).toUInt(), BITMASK_MAX)

            return skip to isNextInstructionInvalid
        }

        /**
         * Parses bitmask using fast path
         */
        @Suppress("NOTHING_TO_INLINE")
        inline fun parseBitmaskFast(bitmask: ByteArray, offset: UInt): UInt? {
            assert(offset < UInt.MAX_VALUE) { "Offset too large" }
            assert(getBitForOffset(bitmask, offset.toInt() + 1, offset)) { "Invalid bit at offset" }

            val currentOffset = offset + 1u
            val byteIndex = (currentOffset.toInt() shr 3)

            // Ensure we have enough bytes to read
            if (byteIndex + 4 > bitmask.size) return null

            val shift = (currentOffset and 7u).toInt()

            // Read 4 bytes and convert to UInt
            val value = bitmask.slice(byteIndex until byteIndex + 4)
                .foldIndexed(0u) { index, acc, byte ->
                    acc or ((byte.toUInt() and 0xFFu) shl (8 * index))
                }

            // Create mask with trailing 1
            val mask = (value shr shift) or (1u shl BITMASK_MAX.toInt())

            return mask.countTrailingZeroBits().toUInt()
        }

        fun <I : InstructionSet> visitorStepInvalidInstruction(
            offset: UInt,
            opcodeVisitor: OpcodeVisitor<Instruction, I>
        ): Instruction {
            return opcodeVisitor.dispatch(
                opcode = INVALID_INSTRUCTION_INDEX,
                chunk = U128(0u, 0u),
                offset = offset,
                skip = 0u
            )
        }

        fun <I : InstructionSet> isJumpTargetValid(
            instructionSet: I,
            code: ByteArray,
            bitmask: ByteArray,
            offset: UInt
        ): Boolean {
            if (!getBitForOffset(bitmask, code.size, offset)) {
                return false
            }

            if (offset == 0u) {
                return true
            }

            val skip = getPreviousInstructionSkip(bitmask, offset) ?: run {
                return false
            }

            val previousOffset = (offset - skip - 1u).toInt()
            val opcode = instructionSet.opcodeFromU8(code[previousOffset].toUByte()) ?: run {
                return false
            }

            return opcode.startsNewBasicBlock()
        }

        fun getPreviousInstructionSkip(bitmask: ByteArray, offset: UInt): UInt? {
            val shift = offset.toInt() and 7
            var mask = (bitmask[offset.toInt() shr 3].toUInt() and 0xFFu) shl 24

            // Build up the mask from previous bytes
            if (offset >= 8u) {
                mask = mask or ((bitmask[(offset.toInt() shr 3) - 1].toUInt() and 0xFFu) shl 16)
            }
            if (offset >= 16u) {
                mask = mask or ((bitmask[(offset.toInt() shr 3) - 2].toUInt() and 0xFFu) shl 8)
            }
            if (offset >= 24u) {
                mask = mask or (bitmask[(offset.toInt() shr 3) - 3].toUInt() and 0xFFu)
            }

            mask = mask shl (8 - shift)
            mask = mask shr 1

            val skip = (mask.countLeadingZeroBits() - 1).toUInt()

            return if (skip > BITMASK_MAX) null else skip
        }

        fun getBitForOffset(bitmask: ByteArray, codeLen: Int, offset: UInt): Boolean {
            val offsetInt = offset.toInt()
            val byteIndex = offsetInt shr 3

            if (byteIndex >= bitmask.size || offsetInt > codeLen) {
                return false
            }

            val shift = offsetInt and 7
            return ((bitmask[byteIndex].toInt() shr shift) and 1) == 1
        }

        fun findNextOffsetUnbounded(bitmask: ByteArray, codeLen: UInt, offsetStart: UInt): UInt {
            var offset = offsetStart
            while (true) {
                val byteIndex = (offset.toLong() shr 3).toInt()
                if (byteIndex >= bitmask.size) break

                val byte = bitmask[byteIndex].toUByte()
                val shift = (offset and 7u).toInt()
                val mask = byte.toUInt() shr shift

                if (mask == 0u) {
                    offset += (8u - shift.toUInt())
                } else {
                    offset += mask.countTrailingZeroBits().toUInt()
                    break
                }
            }

            return minOf(codeLen, offset)
        }

        fun findStartOfBasicBlock(
            instructionSet: RuntimeInstructionSet,
            code: ByteArray,
            bitmask: ByteArray,
            initialOffset: UInt
        ): UInt? {
            var offset = initialOffset
            if (!getBitForOffset(bitmask, code.size, offset)) {
                return null
            }

            if (offset == 0u) {
                return 0u
            }

            while (true) {
                val skip = getPreviousInstructionSkip(bitmask, offset) ?: return null
                val previousOffset = offset - skip - 1u
                val opcode = instructionSet.opcodeFromU8(
                    code[previousOffset.toInt()].toUByte()
                ) ?: Opcode.panic
                if (opcode.startsNewBasicBlock()) {
                    return offset
                }

                offset = previousOffset
                if (offset == 0u) {
                    return 0u
                }
            }
        }


        /**
         * Sign extends a value at a specific bit position
         */
        private fun signExtendAt(value: UInt, bitsToCut: UInt): UInt {
            // Simulate Rust's wrapping behavior
            return ((((value.toLong() shl bitsToCut.toInt()) and 0xFFFFFFFF).toInt()
                shr bitsToCut.toInt()) and 0xFFFFFFFF.toInt()).toUInt()
        }

        /**
         * Reads immediate argument from chunk
         */
        fun readArgsImm(chunk: U128, skip: UInt): UInt =
            readSimpleVarint(chunk.toUInt(), skip)

        /**
         * Reads offset argument from chunk
         */
        fun readArgsOffset(chunk: U128, instructionOffset: UInt, skip: UInt): UInt =
            instructionOffset + readArgsImm(chunk, skip)

        /**
         * Reads two immediate arguments from chunk
         */
        fun readArgsImm2(chunk: U128, skip: UInt): Pair<UInt, UInt> {
            val (imm1Bits, imm1Skip, imm2Bits) = TABLE_1.get(skip, chunk.toUInt())
            val shiftedChunk = chunk shr 8
            val chunk64 = shiftedChunk.low
            val imm1 = signExtendAt(chunk64.toUInt(), imm1Bits)
            val finalChunk = chunk64 shr imm1Skip.toInt()
            val imm2 = signExtendAt(finalChunk.toUInt(), imm2Bits)
            return Pair(imm1, imm2)
        }

        /**
         * Reads register and immediate arguments from chunk
         */
        fun readArgsRegImm(chunk: U128, skip: UInt): Pair<RawReg, UInt> {
            val chunk64 = chunk.low
            val reg = RawReg(chunk64.toUInt())
            val shiftedChunk = chunk64 shr 8
            val (_, _, immBits) = TABLE_1.get(skip, 0u)
            val imm = signExtendAt(shiftedChunk.toUInt(), immBits)
            return Pair(reg, imm)
        }

        /**
         * Reads register and two immediate arguments from chunk
         */
        fun readArgsRegImm2(chunk: U128, skip: UInt): Triple<RawReg, UInt, UInt> {
            val reg = RawReg(chunk.toUInt())
            val (imm1Bits, imm1Skip, imm2Bits) = TABLE_1.get(skip, chunk.toUInt() shr 4)
            var shiftedChunk = chunk shr 8
            val chunk64 = shiftedChunk.low
            val imm1 = signExtendAt(chunk64.toUInt(), imm1Bits)
            val finalChunk = chunk64 shr imm1Skip.toInt()
            val imm2 = signExtendAt(finalChunk.toUInt(), imm2Bits)
            return Triple(reg, imm1, imm2)
        }

        /**
         * Reads register and immediate offset arguments from chunk
         */
        fun readArgsRegImmOffset(
            chunk: U128,
            instructionOffset: UInt,
            skip: UInt
        ): Triple<RawReg, UInt, UInt> {
            val (reg, imm1, imm2) = readArgsRegImm2(chunk, skip)
            return Triple(reg, imm1, instructionOffset.plus(imm2))
        }

        /**
         * Reads two registers and two immediate arguments from chunk
         */
        fun readArgsRegs2Imm2(chunk: U128, skip: UInt): Quadruple<RawReg, RawReg, UInt, UInt> {
            val value = chunk.toUInt()
            val reg1 = RawReg(value)
            val reg2 = RawReg(value shr 4)
            val imm1Aux = value shr 8

            val (imm1Bits, imm1Skip, imm2Bits) = TABLE_2.get(skip, imm1Aux)
            var shiftedChunk = chunk shr 16
            val chunk64 = shiftedChunk.low
            val imm1 = signExtendAt(chunk64.toUInt(), imm1Bits)
            val finalChunk = chunk64 shr imm1Skip.toInt()
            val imm2 = signExtendAt(finalChunk.toUInt(), imm2Bits)
            return Quadruple(reg1, reg2, imm1, imm2)
        }

        /**
         * Reads two registers and immediate argument from chunk
         */
        fun readArgsRegs2Imm(chunk: U128, skip: UInt): Triple<RawReg, RawReg, UInt> {
            val chunk64 = chunk.low
            val value = chunk64.toUInt()
            val reg1 = RawReg(value)
            val reg2 = RawReg(value shr 4)
            val shiftedChunk = chunk64 shr 8
            val (_, _, immBits) = TABLE_1.get(skip, 0u)
            val imm = signExtendAt(shiftedChunk.toUInt(), immBits)
            return Triple(reg1, reg2, imm)
        }

        /**
         * Reads two registers and offset argument from chunk
         */
        fun readArgsRegs2Offset(
            chunk: U128,
            instructionOffset: UInt,
            skip: UInt
        ): Triple<RawReg, RawReg, UInt> {
            val (reg1, reg2, imm) = readArgsRegs2Imm(chunk, skip)
            return Triple(reg1, reg2, instructionOffset + imm)
        }

        /**
         * Reads three registers from chunk
         */
        fun readArgsRegs3(chunk: U128): Triple<RawReg, RawReg, RawReg> {
            val value = chunk.toUInt()
            val reg2 = RawReg(value)
            val reg3 = RawReg(value shr 4)
            val reg1 = RawReg(value shr 8)

            return Triple(
                reg1, reg2, reg3
            )
        }

        /**
         * Reads two registers from chunk
         */
        fun readArgsRegs2(chunk: U128): Pair<RawReg, RawReg> {
            val value = chunk.toUInt()
            return Pair(RawReg(value), RawReg(value shr 4))
        }

        /**
         * Reads register and 64-bit immediate arguments from chunk
         */
        fun readArgsRegImm64(chunk: U128, skip: UInt): Pair<RawReg, ULong> {
            val reg = RawReg(chunk.toUInt())
            val imm = (chunk shr 8).low
            return Pair(reg, imm)
        }
    }
}
