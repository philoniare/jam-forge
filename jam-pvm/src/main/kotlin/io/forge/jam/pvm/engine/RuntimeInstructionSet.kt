package io.forge.jam.pvm.engine

import io.forge.jam.pvm.program.Opcode

/**
 * Interface defining the contract for instruction sets
 */
interface InstructionSet {
    fun opcodeFromU8(byte: UByte): Opcode?
}

/**
 * Represents the different instruction set variants supported by the VM.
 * Matches polkavm's InstructionSetKind enum.
 */
enum class InstructionSetKind {
    Latest32,   // blob_version = 1, 32-bit
    Latest64,   // blob_version = 2, 64-bit, includes all instructions
    JamV1;      // blob_version = 3, 64-bit, excludes memset/unlikely

    companion object {
        fun fromBlobVersion(version: Byte): InstructionSetKind? = when (version.toInt()) {
            1 -> Latest32
            2 -> Latest64
            3 -> JamV1
            else -> null
        }
    }

    fun blobVersion(): Byte = when (this) {
        Latest32 -> 1
        Latest64 -> 2
        JamV1 -> 3
    }

    fun displayName(): String = when (this) {
        JamV1 -> "jam_v1"
        Latest32 -> "latest32"
        Latest64 -> "latest64"
    }

    fun is64Bit(): Boolean = this != Latest32
}

/**
 * Runtime instruction set configuration that determines the instruction set version and capabilities
 */
data class RuntimeInstructionSet(
    val allowSbrk: Boolean,
    val is64Bit: Boolean,
    val isaKind: InstructionSetKind? = null  // Optional for backward compatibility
) : InstructionSet {
    companion object {
        private const val I_32 = 0
        private const val I_64 = 1
        private const val I_SBRK = 2

        // Opcodes excluded from JamV1 and ReviveV1: memset (2), unlikely (3)
        private val JAM_V1_EXCLUDED_OPCODES = setOf(2, 3)

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

        object Latest32 : InstructionSet {
            private val validSet = arrayOf(I_32, I_SBRK)
            override fun opcodeFromU8(byte: UByte): Opcode? {
                if (!isInstructionValid(byte.toInt(), validSet)) return null
                return Opcode.fromUByteAny(byte)
            }
        }

        private object Latest32NoSbrk : InstructionSet {
            private val validSet = arrayOf(I_32)
            override fun opcodeFromU8(byte: UByte): Opcode? {
                if (!isInstructionValid(byte.toInt(), validSet)) return null
                return Opcode.fromUByteAny(byte)
            }
        }

        object Latest64 : InstructionSet {
            private val validSet = arrayOf(I_64, I_SBRK)
            override fun opcodeFromU8(byte: UByte): Opcode? {
                if (!isInstructionValid(byte.toInt(), validSet)) return null
                return Opcode.fromUByteAny(byte)
            }
        }

        object JamV1 : InstructionSet {
            private val validSet = arrayOf(I_64, I_SBRK)
            override fun opcodeFromU8(byte: UByte): Opcode? {
                val value = byte.toInt()
                if (value in JAM_V1_EXCLUDED_OPCODES) return null
                if (!isInstructionValid(value, validSet)) return null
                return Opcode.fromUByteAny(byte)
            }
        }

        /**
         * Helper function to validate JamV1 opcodes.
         * JamV1 excludes memset (2) and unlikely (3).
         */
        private fun isJamV1OpcodeValid(value: Int, allowSbrk: Boolean): Boolean {
            if (value in JAM_V1_EXCLUDED_OPCODES) return false
            val validSet = if (allowSbrk) arrayOf(I_64, I_SBRK) else arrayOf(I_64)
            return isInstructionValid(value, validSet)
        }

        /**
         * Create RuntimeInstructionSet from ISA kind
         */
        fun fromKind(kind: InstructionSetKind, allowSbrk: Boolean = true): RuntimeInstructionSet =
            RuntimeInstructionSet(allowSbrk, kind.is64Bit(), kind)

        /**
         * Create JamV1 instruction set
         */
        fun jamV1(allowSbrk: Boolean = true): RuntimeInstructionSet =
            fromKind(InstructionSetKind.JamV1, allowSbrk)
    }

    override fun opcodeFromU8(byte: UByte): Opcode? {
        // Use explicit ISA kind if provided
        isaKind?.let { kind ->
            return when (kind) {
                InstructionSetKind.JamV1 -> {
                    val value = byte.toInt()
                    if (!isJamV1OpcodeValid(value, allowSbrk)) return null
                    Opcode.fromUByteAny(byte)
                }
                InstructionSetKind.Latest64 -> Latest64.opcodeFromU8(byte)
                InstructionSetKind.Latest32 -> when {
                    allowSbrk -> Latest32.opcodeFromU8(byte)
                    else -> Latest32NoSbrk.opcodeFromU8(byte)
                }
            }
        }

        // Fall back to legacy behavior for backward compatibility
        return when {
            !is64Bit -> when {
                allowSbrk -> Latest32.opcodeFromU8(byte)
                else -> Latest32NoSbrk.opcodeFromU8(byte)
            }
            else -> Latest64.opcodeFromU8(byte)
        }
    }
}
