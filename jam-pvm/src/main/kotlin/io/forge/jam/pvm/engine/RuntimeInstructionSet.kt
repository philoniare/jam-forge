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

    override fun opcodeFromU8(byte: UByte): Opcode? {
        return when {
            !is64Bit -> when {
                allowSbrk -> ISA32_V1.opcodeFromU8(byte)
                else -> ISA32_V1_NoSbrk.opcodeFromU8(byte)
            }

            else -> ISA64_V1.opcodeFromU8(byte)
        }
    }

    companion object {
        private object ISA32_V1 : InstructionSet {
            private val isIstructionValid = BooleanArray(256) { false }.apply {
                val I_32 = 0
                val I_64 = 1
                val I_SBRK = 2
                val b = arrayOf(I_32, I_64)
                fun validateInstruction(a: Array<Int>): Boolean {
                    for (i in a.indices) {
                        for (j in b.indices) {
                            if (a[i] == b[j]) {
                                return true
                            }
                        }
                    }
                    return false
                }

                this[0] = validateInstruction(arrayOf(I_64, I_32))
                this[17] = validateInstruction(arrayOf(I_64, I_32))
                this[19] = validateInstruction(arrayOf(I_64, I_32))
                this[4] = validateInstruction(arrayOf(I_64, I_32))
                this[60] = validateInstruction(arrayOf(I_64, I_32))
                this[74] = validateInstruction(arrayOf(I_64, I_32))
                this[76] = validateInstruction(arrayOf(I_64, I_32))
                this[66] = validateInstruction(arrayOf(I_64, I_32))
                this[10] = validateInstruction(arrayOf(I_64, I_32))
                this[102] = validateInstruction(arrayOf(I_64))
                this[95] = validateInstruction(arrayOf(I_64))
                this[71] = validateInstruction(arrayOf(I_64, I_32))
                this[69] = validateInstruction(arrayOf(I_64, I_32))
                this[22] = validateInstruction(arrayOf(I_64, I_32))
                this[96] = validateInstruction(arrayOf(I_64))
                this[6] = validateInstruction(arrayOf(I_64, I_32))
                this[7] = validateInstruction(arrayOf(I_64, I_32))
                this[15] = validateInstruction(arrayOf(I_64, I_32))
                this[44] = validateInstruction(arrayOf(I_64, I_32))
                this[32] = validateInstruction(arrayOf(I_64, I_32))
                this[52] = validateInstruction(arrayOf(I_64, I_32))
                this[46] = validateInstruction(arrayOf(I_64, I_32))
                this[53] = validateInstruction(arrayOf(I_64, I_32))
                this[26] = validateInstruction(arrayOf(I_64, I_32))
                this[54] = validateInstruction(arrayOf(I_64, I_32))
                this[19] = validateInstruction(arrayOf(I_64, I_32))
                this[93] = validateInstruction(arrayOf(I_64))
                this[16] = validateInstruction(arrayOf(I_64, I_32))
                this[29] = validateInstruction(arrayOf(I_64, I_32))
                this[3] = validateInstruction(arrayOf(I_64, I_32))
                this[90] = validateInstruction(arrayOf(I_64))
                this[11] = validateInstruction(arrayOf(I_64, I_32))
                this[21] = validateInstruction(arrayOf(I_64, I_32))
                this[37] = validateInstruction(arrayOf(I_64, I_32))
                this[33] = validateInstruction(arrayOf(I_64, I_32))
                this[1] = validateInstruction(arrayOf(I_64, I_32))
                this[99] = validateInstruction(arrayOf(I_64))
                this[91] = validateInstruction(arrayOf(I_64))
                this[2] = validateInstruction(arrayOf(I_64, I_32))
                this[104] = validateInstruction(arrayOf(I_64))
                this[18] = validateInstruction(arrayOf(I_64, I_32))
                this[31] = validateInstruction(arrayOf(I_64, I_32))
                this[49] = validateInstruction(arrayOf(I_64, I_32))
                this[35] = validateInstruction(arrayOf(I_64, I_32))
                this[121] = validateInstruction(arrayOf(I_64))
                this[27] = validateInstruction(arrayOf(I_64, I_32))
                this[56] = validateInstruction(arrayOf(I_64, I_32))
                this[9] = validateInstruction(arrayOf(I_64, I_32))
                this[105] = validateInstruction(arrayOf(I_64))
                this[14] = validateInstruction(arrayOf(I_64, I_32))
                this[106] = validateInstruction(arrayOf(I_64))
                this[25] = validateInstruction(arrayOf(I_64, I_32))
                this[107] = validateInstruction(arrayOf(I_64))
                this[40] = validateInstruction(arrayOf(I_64, I_32))
                this[136] = validateInstruction(arrayOf(I_64))
                this[39] = validateInstruction(arrayOf(I_64, I_32))
                this[61] = validateInstruction(arrayOf(I_64, I_32))
                this[72] = validateInstruction(arrayOf(I_64, I_32))
                this[103] = validateInstruction(arrayOf(I_64))
                this[80] = validateInstruction(arrayOf(I_64, I_32))
                this[111] = validateInstruction(arrayOf(I_64))
                this[75] = validateInstruction(arrayOf(I_64, I_32))
                this[100] = validateInstruction(arrayOf(I_64))
                this[85] = validateInstruction(arrayOf(I_64, I_32))
                this[86] = validateInstruction(arrayOf(I_64, I_32))
                this[24] = validateInstruction(arrayOf(I_64, I_32))
                this[30] = validateInstruction(arrayOf(I_64, I_32))
                this[47] = validateInstruction(arrayOf(I_64, I_32))
                this[48] = validateInstruction(arrayOf(I_64, I_32))
                this[41] = validateInstruction(arrayOf(I_64, I_32))
                this[43] = validateInstruction(arrayOf(I_64, I_32))
                this[8] = validateInstruction(arrayOf(I_64, I_32))
                this[101] = validateInstruction(arrayOf(I_64))
                this[20] = validateInstruction(arrayOf(I_64, I_32))
                this[112] = validateInstruction(arrayOf(I_64))
                this[23] = validateInstruction(arrayOf(I_64, I_32))
                this[28] = validateInstruction(arrayOf(I_64, I_32))
                this[12] = validateInstruction(arrayOf(I_64, I_32))
                this[34] = validateInstruction(arrayOf(I_64, I_32))
                this[113] = validateInstruction(arrayOf(I_64))
                this[67] = validateInstruction(arrayOf(I_64, I_32))
                this[57] = validateInstruction(arrayOf(I_64, I_32))
                this[81] = validateInstruction(arrayOf(I_64, I_32))
                this[36] = validateInstruction(arrayOf(I_64, I_32))
                this[58] = validateInstruction(arrayOf(I_64, I_32))
                this[55] = validateInstruction(arrayOf(I_64, I_32))
                this[100] = validateInstruction(arrayOf(I_64))
                this[51] = validateInstruction(arrayOf(I_64, I_32))
                this[108] = validateInstruction(arrayOf(I_64))
                this[77] = validateInstruction(arrayOf(I_64, I_32))
                this[109] = validateInstruction(arrayOf(I_64))
                this[68] = validateInstruction(arrayOf(I_64, I_32))
                this[114] = validateInstruction(arrayOf(I_64))
                this[64] = validateInstruction(arrayOf(I_64, I_32))
                this[115] = validateInstruction(arrayOf(I_64))
                this[73] = validateInstruction(arrayOf(I_64, I_32))
                this[116] = validateInstruction(arrayOf(I_64))
                this[70] = validateInstruction(arrayOf(I_64, I_32))
                this[117] = validateInstruction(arrayOf(I_64))
                this[83] = validateInstruction(arrayOf(I_64, I_32))
                this[84] = validateInstruction(arrayOf(I_64, I_32))
                this[5] = validateInstruction(arrayOf(I_64, I_32))
                this[78] = validateInstruction(arrayOf(I_64, I_32))
                this[62] = validateInstruction(arrayOf(I_64, I_32))
                this[79] = validateInstruction(arrayOf(I_64, I_32))
                this[38] = validateInstruction(arrayOf(I_64, I_32))
                this[98] = validateInstruction(arrayOf(I_64))
                this[82] = validateInstruction(arrayOf(I_64, I_32))
                this[87] = validateInstruction(arrayOf(I_SBRK))
                this[42] = validateInstruction(arrayOf(I_64, I_32))
                this[118] = validateInstruction(arrayOf(I_64))
            }


            override fun opcodeFromU8(byte: UByte): Opcode? {
                // Implementation for 32-bit ISA with sbrk
                if (!isIstructionValid[byte.toInt()]) {
                    return null
                }
                return Opcode.fromUByteAny(byte)
            }
        }

        private object ISA32_V1_NoSbrk : InstructionSet {
            override fun opcodeFromU8(byte: UByte): Opcode? {
                // Implementation for 32-bit ISA without sbrk
                TODO("Implement 32-bit ISA without sbrk")
            }
        }

        private object ISA64_V1 : InstructionSet {
            override fun opcodeFromU8(byte: UByte): Opcode? {
                // Implementation for 64-bit ISA
                TODO("Implement 64-bit ISA")
            }
        }
    }
}
