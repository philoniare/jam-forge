package io.forge.jam.pvm.engine

import io.forge.jam.pvm.program.Opcode

class Program {
    companion object {
        private const val BITMASK_MAX: UInt = 24u

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
                ) ?: Opcode.trap
                if (opcode.startsNewBasicBlock()) {
                    return offset
                }

                offset = previousOffset
                if (offset == 0u) {
                    return 0u
                }
            }
        }
    }
}
