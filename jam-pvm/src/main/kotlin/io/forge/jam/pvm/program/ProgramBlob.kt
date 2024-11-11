package io.forge.jam.pvm.program

import io.forge.jam.pvm.PvmLogger
import io.forge.jam.pvm.engine.InstructionSet
import io.forge.jam.pvm.engine.RuntimeInstructionSet

/**
 * A partially deserialized PolkaVM program.
 */
data class ProgramBlob(
    var is64Bit: Boolean = false,
    var roDataSize: UInt = 0u,
    var rwDataSize: UInt = 0u,
    var stackSize: UInt = 0u,
    var roData: ArcBytes = ArcBytes.empty(),
    var rwData: ArcBytes = ArcBytes.empty(),
    var code: ArcBytes = ArcBytes.empty(),
    private var jumpTable: ArcBytes = ArcBytes.empty(),
    private var jumpTableEntrySize: Byte = 0,
    var bitmask: ArcBytes = ArcBytes.empty(),
    private var importOffsets: ArcBytes = ArcBytes.empty(),
    private var importSymbols: ArcBytes = ArcBytes.empty(),
    private var exports: ArcBytes = ArcBytes.empty(),
    private var debugStrings: ArcBytes = ArcBytes.empty(),
    private var debugLineProgramRanges: ArcBytes = ArcBytes.empty(),
    private var debugLinePrograms: ArcBytes = ArcBytes.empty()
) {

    fun <I : InstructionSet> instructionsBoundedAt(instructionSet: I, offset: ProgramCounter): Instructions<I> {
        return Instructions(
            code = code.toByteArray(),
            bitmask = bitmask.toByteArray(),
            offset = offset.value,
            instructionSet = instructionSet
        )
    }

    fun isJumpTargetValid(instructionSet: RuntimeInstructionSet, offset: ProgramCounter): Boolean {
        return Program.isJumpTargetValid(instructionSet, code.toByteArray(), bitmask.asRef(), offset.value)
    }


    companion object {
        private val logger = PvmLogger(ProgramBlob::class.java)
        private val VM_MAXIMUM_JUMP_TABLE_ENTRIES: UInt = 16u * 1024u * 1024u
        private val VM_MAXIMUM_CODE_SIZE: UInt = 32u * 1024u * 1024u
        private const val VERSION_DEBUG_LINE_PROGRAM_V1: Byte = 1

        fun fromParts(parts: ProgramParts): Result<ProgramBlob> = runCatching {
            val blob = ProgramBlob(
                is64Bit = parts.is64Bit,
                roDataSize = parts.roDataSize,
                rwDataSize = parts.rwDataSize,
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

            blob
        }
    }
}
