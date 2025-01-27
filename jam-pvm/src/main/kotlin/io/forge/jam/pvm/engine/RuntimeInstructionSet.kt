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

        private val isInstructionValid = BooleanArray(256) { false }.apply {
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
            this[1] = validateInstruction(arrayOf(I_64, I_32))
            this[2] = validateInstruction(arrayOf(I_64, I_32))

            this[50] = validateInstruction(arrayOf(I_64, I_32))
            this[51] = validateInstruction(arrayOf(I_64, I_32))
            this[52] = validateInstruction(arrayOf(I_64, I_32))
            this[53] = validateInstruction(arrayOf(I_64, I_32))
            this[54] = validateInstruction(arrayOf(I_64, I_32))
            this[55] = validateInstruction(arrayOf(I_64, I_32))
            this[57] = validateInstruction(arrayOf(I_64, I_32))
            this[56] = validateInstruction(arrayOf(I_64))
            this[58] = validateInstruction(arrayOf(I_64))
            this[59] = validateInstruction(arrayOf(I_64, I_32))
            this[60] = validateInstruction(arrayOf(I_64, I_32))
            this[61] = validateInstruction(arrayOf(I_64, I_32))
            this[62] = validateInstruction(arrayOf(I_64))

            this[80] = validateInstruction(arrayOf(I_64, I_32))
            this[81] = validateInstruction(arrayOf(I_64, I_32))
            this[82] = validateInstruction(arrayOf(I_64, I_32))
            this[83] = validateInstruction(arrayOf(I_64, I_32))
            this[87] = validateInstruction(arrayOf(I_64, I_32))
            this[85] = validateInstruction(arrayOf(I_64, I_32))
            this[89] = validateInstruction(arrayOf(I_64, I_32))
            this[88] = validateInstruction(arrayOf(I_64, I_32))
            this[84] = validateInstruction(arrayOf(I_64, I_32))
            this[90] = validateInstruction(arrayOf(I_64, I_32))
            this[86] = validateInstruction(arrayOf(I_64, I_32))

            this[70] = validateInstruction(arrayOf(I_64, I_32))
            this[71] = validateInstruction(arrayOf(I_64, I_32))
            this[72] = validateInstruction(arrayOf(I_64, I_32))
            this[73] = validateInstruction(arrayOf(I_64))

            this[120] = validateInstruction(arrayOf(I_64, I_32))
            this[121] = validateInstruction(arrayOf(I_64, I_32))
            this[122] = validateInstruction(arrayOf(I_64, I_32))
            this[123] = validateInstruction(arrayOf(I_64))
            this[124] = validateInstruction(arrayOf(I_64, I_32))
            this[125] = validateInstruction(arrayOf(I_64, I_32))
            this[126] = validateInstruction(arrayOf(I_64, I_32))
            this[127] = validateInstruction(arrayOf(I_64, I_32))
            this[129] = validateInstruction(arrayOf(I_64, I_32))
            this[128] = validateInstruction(arrayOf(I_64))
            this[130] = validateInstruction(arrayOf(I_64))
            this[131] = validateInstruction(arrayOf(I_64, I_32))
            this[149] = validateInstruction(arrayOf(I_64))
            this[132] = validateInstruction(arrayOf(I_64, I_32))
            this[133] = validateInstruction(arrayOf(I_64, I_32))
            this[134] = validateInstruction(arrayOf(I_64, I_32))
            this[135] = validateInstruction(arrayOf(I_64, I_32))
            this[150] = validateInstruction(arrayOf(I_64))
            this[136] = validateInstruction(arrayOf(I_64, I_32))
            this[137] = validateInstruction(arrayOf(I_64, I_32))
            this[138] = validateInstruction(arrayOf(I_64, I_32))
            this[151] = validateInstruction(arrayOf(I_64))
            this[139] = validateInstruction(arrayOf(I_64, I_32))
            this[152] = validateInstruction(arrayOf(I_64))
            this[140] = validateInstruction(arrayOf(I_64, I_32))
            this[153] = validateInstruction(arrayOf(I_64))
            this[141] = validateInstruction(arrayOf(I_64, I_32))
            this[154] = validateInstruction(arrayOf(I_64))
            this[142] = validateInstruction(arrayOf(I_64, I_32))
            this[143] = validateInstruction(arrayOf(I_64, I_32))
            this[145] = validateInstruction(arrayOf(I_64, I_32))
            this[154] = validateInstruction(arrayOf(I_64))
            this[146] = validateInstruction(arrayOf(I_64, I_32))
            this[157] = validateInstruction(arrayOf(I_64))
            this[144] = validateInstruction(arrayOf(I_64, I_32))
            this[155] = validateInstruction(arrayOf(I_64))

            this[147] = validateInstruction(arrayOf(I_64, I_32))
            this[148] = validateInstruction(arrayOf(I_64, I_32))

            this[160] = validateInstruction(arrayOf(I_64, I_32))
            this[161] = validateInstruction(arrayOf(I_64, I_32))
            this[158] = validateInstruction(arrayOf(I_64))
            this[159] = validateInstruction(arrayOf(I_64))

            this[170] = validateInstruction(arrayOf(I_64, I_32))
            this[171] = validateInstruction(arrayOf(I_64, I_32))
            this[172] = validateInstruction(arrayOf(I_64, I_32))
            this[173] = validateInstruction(arrayOf(I_64, I_32))
            this[174] = validateInstruction(arrayOf(I_64, I_32))
            this[175] = validateInstruction(arrayOf(I_64, I_32))

            this[190] = validateInstruction(arrayOf(I_64, I_32))
            this[200] = validateInstruction(arrayOf(I_64))
            this[191] = validateInstruction(arrayOf(I_64, I_32))
            this[201] = validateInstruction(arrayOf(I_64))
            this[210] = validateInstruction(arrayOf(I_64, I_32))
            this[211] = validateInstruction(arrayOf(I_64, I_32))
            this[212] = validateInstruction(arrayOf(I_64, I_32))
            this[192] = validateInstruction(arrayOf(I_64, I_32))
            this[202] = validateInstruction(arrayOf(I_64))
            this[213] = validateInstruction(arrayOf(I_64, I_32))
            this[214] = validateInstruction(arrayOf(I_64, I_32))
            this[215] = validateInstruction(arrayOf(I_64, I_32))
            this[216] = validateInstruction(arrayOf(I_64, I_32))
            this[217] = validateInstruction(arrayOf(I_64, I_32))
            this[197] = validateInstruction(arrayOf(I_64, I_32))
            this[207] = validateInstruction(arrayOf(I_64))
            this[198] = validateInstruction(arrayOf(I_64, I_32))
            this[208] = validateInstruction(arrayOf(I_64))
            this[199] = validateInstruction(arrayOf(I_64, I_32))
            this[209] = validateInstruction(arrayOf(I_64))
            this[193] = validateInstruction(arrayOf(I_64, I_32))
            this[203] = validateInstruction(arrayOf(I_64))
            this[194] = validateInstruction(arrayOf(I_64, I_32))
            this[204] = validateInstruction(arrayOf(I_64))
            this[195] = validateInstruction(arrayOf(I_64, I_32))
            this[205] = validateInstruction(arrayOf(I_64))
            this[196] = validateInstruction(arrayOf(I_64, I_32))
            this[206] = validateInstruction(arrayOf(I_64))

            this[218] = validateInstruction(arrayOf(I_64, I_32))
            this[219] = validateInstruction(arrayOf(I_64, I_32))

            this[224] = validateInstruction(arrayOf(I_64, I_32))
            this[225] = validateInstruction(arrayOf(I_64, I_32))
            this[226] = validateInstruction(arrayOf(I_64, I_32))
            this[227] = validateInstruction(arrayOf(I_64, I_32))
            this[228] = validateInstruction(arrayOf(I_64, I_32))
            this[229] = validateInstruction(arrayOf(I_64, I_32))
            this[230] = validateInstruction(arrayOf(I_64, I_32))
            this[221] = validateInstruction(arrayOf(I_64, I_32))
            this[220] = validateInstruction(arrayOf(I_64))
            this[223] = validateInstruction(arrayOf(I_64, I_32))
            this[222] = validateInstruction(arrayOf(I_64))

            this[40] = validateInstruction(arrayOf(I_64, I_32))
            this[10] = validateInstruction(arrayOf(I_64, I_32))
            this[30] = validateInstruction(arrayOf(I_64, I_32))
            this[31] = validateInstruction(arrayOf(I_64, I_32))
            this[32] = validateInstruction(arrayOf(I_64, I_32))
            this[33] = validateInstruction(arrayOf(I_64))

            this[100] = validateInstruction(arrayOf(I_64, I_32))
            this[101] = validateInstruction(arrayOf(I_SBRK))
            this[105] = validateInstruction(arrayOf(I_64, I_32))
            this[104] = validateInstruction(arrayOf(I_64))
            this[107] = validateInstruction(arrayOf(I_64, I_32))
            this[106] = validateInstruction(arrayOf(I_64))
            this[103] = validateInstruction(arrayOf(I_64, I_32))
            this[102] = validateInstruction(arrayOf(I_64))
            this[108] = validateInstruction(arrayOf(I_64, I_32))
            this[109] = validateInstruction(arrayOf(I_64, I_32))
            this[110] = validateInstruction(arrayOf(I_64, I_32))
            this[111] = validateInstruction(arrayOf(I_64, I_32))
            this[180] = validateInstruction(arrayOf(I_64, I_32))
            this[20] = validateInstruction(arrayOf(I_64))
        }

        object ISA32_V1 : InstructionSet {
            override fun opcodeFromU8(byte: UByte): Opcode? {
                // Implementation for 32-bit ISA with sbrk
                if (!isInstructionValid[byte.toInt()]) {
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
                if (!isInstructionValid[byte.toInt()]) {
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
