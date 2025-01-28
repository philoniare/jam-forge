package io.forge.jam.pvm.engine

import io.forge.jam.pvm.program.Opcode

/**
 * Interface defining the contract for instruction sets
 */
interface InstructionSet {
    fun opcodeFromU8(byte: UByte): Opcode?
}

/**
 * Runtime instruction set configuration that determines the instruction set version and capabilities
 */
data class RuntimeInstructionSet(
    val allowSbrk: Boolean,
    val is64Bit: Boolean
) : InstructionSet {
    companion object {
        private const val I_32 = 0
        private const val I_64 = 1
        private const val I_SBRK = 2

        fun isInstructionValid(instruction: Int, supportedInstructions: Array<Int>): Boolean {
            return when (instruction) {
                0, 1, 2 -> arrayOf(I_64, I_32)
                50, 51, 52, 53, 54, 55, 57, 59, 60, 61 -> arrayOf(I_64, I_32)
                80, 81, 82, 83, 87, 85, 89, 88, 84, 90, 86 -> arrayOf(I_64, I_32)
                70, 71, 72, 120, 121, 122, 124, 125, 126, 129 -> arrayOf(I_64, I_32)
                131, 132, 133, 134, 135, 136, 137, 138, 127 -> arrayOf(I_64, I_32)
                56, 58, 62, 73, 123, 128, 130, 149, 150, 151 -> arrayOf(I_64)
                139, 140, 141, 142, 143, 145, 146, 144, 147, 148 -> arrayOf(I_64, I_32)
                160, 161, 170, 171, 172, 173, 174, 175, 190, 191 -> arrayOf(I_64, I_32)
                210, 211, 212, 192, 213, 214, 215, 216, 217, 197 -> arrayOf(I_64, I_32)
                152, 153, 154, 157, 155, 158, 159, 200, 201, 202 -> arrayOf(I_64)
                198, 199, 193, 194, 195, 196, 218, 219, 224, 225 -> arrayOf(I_64, I_32)
                226, 227, 228, 229, 230, 221, 223, 40, 10, 30, 31, 32 -> arrayOf(I_64, I_32)
                207, 208, 209, 203, 204, 205, 206, 220, 222, 33, 156 -> arrayOf(I_64)
                100, 105, 107, 103, 108, 109, 110, 111, 180 -> arrayOf(I_64, I_32)
                104, 106, 102, 20 -> arrayOf(I_64)
                101 -> arrayOf(I_SBRK)
                else -> arrayOf()
            }.any { it in supportedInstructions }
        }

        object ISA32_V1 : InstructionSet {
            val validSet = arrayOf(I_32, I_SBRK)
            override fun opcodeFromU8(byte: UByte): Opcode? {
                if (!isInstructionValid(byte.toInt(), validSet)) {
                    return null
                }
                return Opcode.fromUByteAny(byte)
            }
        }

        private object ISA32_V1_NoSbrk : InstructionSet {
            val validSet = arrayOf(I_32)
            override fun opcodeFromU8(byte: UByte): Opcode? {
                if (!isInstructionValid(byte.toInt(), validSet)) {
                    return null
                }
                return Opcode.fromUByteAny(byte)
            }
        }

        private object ISA64_V1 : InstructionSet {
            val validSet = arrayOf(I_64, I_SBRK)
            override fun opcodeFromU8(byte: UByte): Opcode? {
                if (!isInstructionValid(byte.toInt(), validSet)) {
                    return null
                }
                return Opcode.fromUByteAny(byte)
            }
        }
    }

    override fun opcodeFromU8(byte: UByte): Opcode? {
        return when {
            !is64Bit -> when {
                allowSbrk -> ISA32_V1.opcodeFromU8(byte)
                else -> ISA32_V1_NoSbrk.opcodeFromU8(byte)
            }

            else -> ISA64_V1.opcodeFromU8(byte)
        }
    }

}
