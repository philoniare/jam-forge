package io.forge.jam.pvm.program

import io.forge.jam.pvm.PvmLogger
import io.forge.jam.pvm.engine.InstructionSet
import io.forge.jam.pvm.engine.InstructionSetKind
import io.forge.jam.pvm.engine.JumpTable
import io.forge.jam.pvm.engine.RuntimeInstructionSet

/**
 * A partially deserialized PolkaVM program.
 */
data class ProgramBlob(
    var isaKind: InstructionSetKind? = null,  // Track the ISA kind for proper instruction validation
    var is64Bit: Boolean = false,
    var roDataSize: UInt = 0u,
    var rwDataSize: UInt = 0u,
    var actualRwDataLen: UInt = 0u,  // The actual rwData content length (without heap pages)
    var stackSize: UInt = 0u,
    var roData: ArcBytes = ArcBytes.empty(),
    var rwData: ArcBytes = ArcBytes.empty(),
    var code: ArcBytes = ArcBytes.empty(),
    var jumpTable: ArcBytes = ArcBytes.empty(),
    var jumpTableEntrySize: Byte = 0,
    var bitmask: ArcBytes = ArcBytes.empty(),
    private var importOffsets: ArcBytes = ArcBytes.empty(),
    private var importSymbols: ArcBytes = ArcBytes.empty(),
    private var exports: ArcBytes = ArcBytes.empty(),
    private var debugStrings: ArcBytes = ArcBytes.empty(),
    private var debugLineProgramRanges: ArcBytes = ArcBytes.empty(),
    private var debugLinePrograms: ArcBytes = ArcBytes.empty()
) {

    fun jumpTable(): JumpTable {
        return JumpTable(jumpTable, jumpTableEntrySize.toUInt())
    }

    fun <I : InstructionSet> instructionsBoundedAt(instructionSet: I, offset: ProgramCounter): Instructions<I> {
        return Instructions.new(
            code = code.toByteArray(),
            bitmask = bitmask.toByteArray(),
            offset = offset.value,
            instructionSet = instructionSet,
            isBounded = true
        )
    }

    fun isJumpTargetValid(instructionSet: RuntimeInstructionSet, offset: ProgramCounter): Boolean {
        return Program.isJumpTargetValid(instructionSet, code.toByteArray(), bitmask.asRef(), offset.value)
    }

    /**
     * Get the instruction set for this program blob.
     * Uses the ISA kind if available, otherwise falls back to legacy is64Bit-based selection.
     */
    fun instructionSet(allowSbrk: Boolean = true): RuntimeInstructionSet =
        isaKind?.let { RuntimeInstructionSet.fromKind(it, allowSbrk) }
            ?: RuntimeInstructionSet(allowSbrk, is64Bit)

    companion object {
        private val logger = PvmLogger(ProgramBlob::class.java)
        private val VM_MAXIMUM_JUMP_TABLE_ENTRIES: UInt = 16u * 1024u * 1024u
        private val VM_MAXIMUM_CODE_SIZE: UInt = 32u * 1024u * 1024u
        private const val VERSION_DEBUG_LINE_PROGRAM_V1: Byte = 1

        fun fromParts(parts: ProgramParts): Result<ProgramBlob> = runCatching {
            val blob = ProgramBlob(
                isaKind = parts.isaKind,
                is64Bit = parts.is64Bit,
                roDataSize = parts.roDataSize,
                rwDataSize = parts.rwDataSize,
                actualRwDataLen = parts.actualRwDataLen,
                stackSize = parts.stackSize,
                roData = parts.roData,
                rwData = parts.rwData,
                exports = parts.exports,
                importSymbols = parts.importSymbols,
                importOffsets = parts.importOffsets,
                debugStrings = parts.debugStrings,
                debugLineProgramRanges = parts.debugLineProgramRanges,
                debugLinePrograms = parts.debugLinePrograms
            )
            if (blob.roData.asRef().size > blob.roDataSize.toInt()) {
                throw ProgramParseError(
                    ProgramParseErrorKind.Other("size of the read-only data payload exceeds the declared size of the section")
                )
            }

            if (blob.rwData.asRef().size > blob.rwDataSize.toInt()) {
                throw ProgramParseError(
                    ProgramParseErrorKind.Other("size of the read-write data payload exceeds the declared size of the section")
                )
            }

            if (parts.codeAndJumpTable.asRef().isEmpty()) {
                throw ProgramParseError(ProgramParseErrorKind.Other("no code found"))
            }

            // Use JAM-specific parsing for JamV1 ISA
            if (parts.isaKind == InstructionSetKind.JamV1) {
                parseJamCodeSection(parts, blob)
            } else {
                parsePolkavmCodeSection(parts, blob)
            }

            blob
        }

        /**
         * Parse JAM format code section
         */
        private fun parseJamCodeSection(parts: ProgramParts, blob: ProgramBlob) {
            val data = parts.codeAndJumpTable.toByteArray()

            if (data.size < 6) {
                throw ProgramParseError(ProgramParseErrorKind.Other("JAM code section too short"))
            }

            // Check for JAM-specific header (0x80)
            // If not present, try parsing as standard PolkaVM code section
            if ((data[0].toInt() and 0xFF) != 0x80) {
                parsePolkavmCodeSection(parts, blob)
                return
            }

            // Parse JAM header
            // byte 0 is format indicator (0x80 = 2-byte entries), currently unused
            val jumpTableEntryCount = data[1].toInt() and 0xFF
            // bytes 2-3 are reserved/unknown
            val codeLength = ((data[4].toInt() and 0xFF) or ((data[5].toInt() and 0xFF) shl 8))

            // JAM format uses 2-byte entries
            val jumpTableEntrySize: Byte = 2
            val jumpTableLength = jumpTableEntryCount * jumpTableEntrySize

            // Calculate offsets
            val headerSize = 6
            val jumpTableStart = headerSize
            val jumpTableEnd = jumpTableStart + jumpTableLength
            val codeStart = jumpTableEnd
            val codeEnd = codeStart + codeLength

            if (codeEnd > data.size) {
                throw ProgramParseError(
                    ProgramParseErrorKind.Other("JAM code section truncated: code extends beyond end")
                )
            }

            val bitmaskStart = codeEnd
            val bitmaskLength = data.size - bitmaskStart

            // Extract sections
            blob.jumpTableEntrySize = jumpTableEntrySize
            blob.jumpTable = ArcBytes.fromStatic(data.copyOfRange(jumpTableStart, jumpTableEnd))
            blob.code = ArcBytes.fromStatic(data.copyOfRange(codeStart, codeEnd))
            blob.bitmask = ArcBytes.fromStatic(data.copyOfRange(bitmaskStart, data.size))

            // Validate bitmask length
            var expectedBitmaskLength = codeLength / 8
            val isBitmaskPadded = codeLength % 8 != 0
            if (isBitmaskPadded) expectedBitmaskLength++

            if (bitmaskLength != expectedBitmaskLength) {
                throw ProgramParseError(
                    ProgramParseErrorKind.Other(
                        "JAM bitmask length mismatch: expected $expectedBitmaskLength, got $bitmaskLength"
                    )
                )
            }

            // Validate bitmask padding
            if (isBitmaskPadded && blob.bitmask.asRef().isNotEmpty()) {
                val lastByte = blob.bitmask.asRef().last()
                val paddingBits = bitmaskLength * 8 - codeLength
                val paddingMask = (0b10000000.toByte().toInt() shr (paddingBits - 1)).toByte()

                if ((lastByte.toInt() and paddingMask.toInt()) != 0) {
                    throw ProgramParseError(
                        ProgramParseErrorKind.Other("JAM bitmask is padded with non-zero bits")
                    )
                }
            }
        }

        /**
         * Parse standard PolkaVM format code section.
         */
        private fun parsePolkavmCodeSection(parts: ProgramParts, blob: ProgramBlob) {
            val reader = ArcBytesReader(parts.codeAndJumpTable)
            val initialPosition = reader.position

            val jumpTableEntryCount = reader.readVarintInternal().getOrThrow()
            if (jumpTableEntryCount > VM_MAXIMUM_JUMP_TABLE_ENTRIES) {
                throw ProgramParseError(ProgramParseErrorKind.Other("the jump table section is too long"))
            }

            val jumpTableEntrySize = reader.readByte().getOrThrow()
            if (jumpTableEntrySize !in 0..4) {
                throw ProgramParseError(ProgramParseErrorKind.Other("invalid jump table entry size"))
            }

            val codeLength = reader.readVarintInternal().getOrThrow()
            if (codeLength > VM_MAXIMUM_CODE_SIZE) {
                throw ProgramParseError(ProgramParseErrorKind.Other("the code section is too long"))
            }

            val jumpTableLength = jumpTableEntryCount.toInt() * jumpTableEntrySize.toInt()
            blob.jumpTableEntrySize = jumpTableEntrySize
            blob.jumpTable = reader.readSliceAsBytes(jumpTableLength).getOrThrow()
            blob.code = reader.readSliceAsBytes(codeLength.toInt()).getOrThrow()

            val bitmaskLength = parts.codeAndJumpTable.asRef().size - (reader.position - initialPosition)
            blob.bitmask = reader.readSliceAsBytes(bitmaskLength).getOrThrow()

            var expectedBitmaskLength = blob.code.asRef().size / 8
            val isBitmaskPadded = blob.code.asRef().size % 8 != 0
            if (isBitmaskPadded) expectedBitmaskLength++

            if (blob.bitmask.asRef().size != expectedBitmaskLength) {
                throw ProgramParseError(
                    ProgramParseErrorKind.Other("the bitmask length doesn't match the code length")
                )
            }

            if (isBitmaskPadded) {
                val lastByte = blob.bitmask.asRef().last()
                val paddingBits = blob.bitmask.asRef().size * 8 - blob.code.asRef().size
                val paddingMask = (0b10000000.toByte().toInt() shr (paddingBits - 1)).toByte()

                if ((lastByte.toInt() and paddingMask.toInt()) != 0) {
                    throw ProgramParseError(
                        ProgramParseErrorKind.Other("the bitmask is padded with non-zero bits")
                    )
                }
            }
        }
    }
}
