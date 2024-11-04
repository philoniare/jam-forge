package io.forge.jam.pvm.program

import io.forge.jam.core.startsWith
import java.nio.ByteBuffer
import java.nio.ByteOrder

data class ProgramParts(
    var is64Bit: Boolean = false,
    var roDataSize: UInt = 0u,
    var rwDataSize: UInt = 0u,
    var stackSize: UInt = 0u,
    var roData: ArcBytes = ArcBytes.empty(),
    var rwData: ArcBytes = ArcBytes.empty(),
    var codeAndJumpTable: ArcBytes = ArcBytes.empty(),
    var importOffsets: ArcBytes = ArcBytes.empty(),
    var importSymbols: ArcBytes = ArcBytes.empty(),
    var exports: ArcBytes = ArcBytes.empty(),
    var debugStrings: ArcBytes = ArcBytes.empty(),
    var debugLineProgramRanges: ArcBytes = ArcBytes.empty(),
    var debugLinePrograms: ArcBytes = ArcBytes.empty()
) {
    companion object {
        val BLOB_MAGIC: ByteArray = byteArrayOf('P'.code.toByte(), 'V'.code.toByte(), 'M'.code.toByte(), 0)
        private const val BLOB_VERSION_V1_32: Byte = 1
        private const val BLOB_VERSION_V1_64: Byte = 0

        private const val BLOB_LEN_SIZE: Int = Long.SIZE_BYTES
        private const val VM_MAXIMUM_IMPORT_COUNT: UInt = 1024u

        // Section constants
        private const val SECTION_MEMORY_CONFIG: Byte = 1
        private const val SECTION_RO_DATA: Byte = 2
        private const val SECTION_RW_DATA: Byte = 3
        private const val SECTION_IMPORTS: Byte = 4
        private const val SECTION_EXPORTS: Byte = 5
        private const val SECTION_CODE_AND_JUMP_TABLE: Byte = 6
        private const val SECTION_OPT_DEBUG_STRINGS: Byte = 128.toByte()
        private const val SECTION_OPT_DEBUG_LINE_PROGRAMS: Byte = 129.toByte()
        private const val SECTION_OPT_DEBUG_LINE_PROGRAM_RANGES: Byte = 130.toByte()
        private const val SECTION_END_OF_FILE: Byte = 0

        fun fromBytes(blob: ArcBytes): Result<ProgramParts> = runCatching {
            if (!blob.asRef().startsWith(BLOB_MAGIC)) {
                throw ProgramParseError(
                    ProgramParseErrorKind.Other("blob doesn't start with the expected magic bytes")
                )
            }

            val reader = ArcBytesReader(blob, BLOB_MAGIC.size)

            val is64Bit = when (val blobVersion = reader.readByte().getOrThrow()) {
                BLOB_VERSION_V1_32 -> false
                BLOB_VERSION_V1_64 -> true
                else -> throw ProgramParseError(ProgramParseErrorKind.UnsupportedVersion(blobVersion))
            }

            val blobLen = reader.readSlice(BLOB_LEN_SIZE).getOrThrow()
                .let { ByteBuffer.wrap(it).order(ByteOrder.LITTLE_ENDIAN).long }

            if (blobLen != blob.asRef().size.toLong()) {
                throw ProgramParseError(
                    ProgramParseErrorKind.Other("blob size doesn't match the blob length metadata")
                )
            }

            val parts = ProgramParts(is64Bit = is64Bit)
            var sectionHolder = reader.readByte().getOrThrow()

            // Read memory config section if present
            if (sectionHolder == SECTION_MEMORY_CONFIG) {
                val sectionLength = reader.readVarintInternal().getOrThrow()
                val position = reader.position

                parts.roDataSize = reader.readVarintInternal().getOrThrow()
                parts.rwDataSize = reader.readVarintInternal().getOrThrow()
                parts.stackSize = reader.readVarintInternal().getOrThrow()

                if (position + sectionLength.toInt() != reader.position) {
                    throw ProgramParseError(
                        ProgramParseErrorKind.Other("the memory config section contains more data than expected")
                    )
                }

                sectionHolder = reader.readByte().getOrThrow()
            }

            // Read data sections
            val (newSection1, roData) = reader.readSectionAsBytes(sectionHolder, SECTION_RO_DATA).getOrThrow()
            parts.roData = roData
            sectionHolder = newSection1

            val (newSection2, rwData) = reader.readSectionAsBytes(sectionHolder, SECTION_RW_DATA).getOrThrow()
            parts.rwData = rwData
            sectionHolder = newSection2

            // Handle imports section if present
            if (sectionHolder == SECTION_IMPORTS) {
                val sectionLength = reader.readVarintInternal().getOrThrow().toInt()
                val sectionStart = reader.position

                val importCount = reader.readVarintInternal().getOrThrow()
                if (importCount > VM_MAXIMUM_IMPORT_COUNT) {
                    throw ProgramParseError(ProgramParseErrorKind.Other("too many imports"))
                }

                val importOffsetsSize = importCount.toInt() * 4
                parts.importOffsets = reader.readSliceAsBytes(importOffsetsSize).getOrThrow()

                val importSymbolsSize = sectionLength - (reader.position - sectionStart)
                if (importSymbolsSize < 0) {
                    throw ProgramParseError(ProgramParseErrorKind.Other("the imports section is invalid"))
                }

                parts.importSymbols = reader.readSliceAsBytes(importSymbolsSize).getOrThrow()
                sectionHolder = reader.readByte().getOrThrow()
            }

            // Read remaining sections
            val (newSection3, exports) = reader.readSectionAsBytes(sectionHolder, SECTION_EXPORTS).getOrThrow()
            parts.exports = exports
            sectionHolder = newSection3

            val (newSection4, codeAndJumpTable) = reader.readSectionAsBytes(sectionHolder, SECTION_CODE_AND_JUMP_TABLE)
                .getOrThrow()
            parts.codeAndJumpTable = codeAndJumpTable
            sectionHolder = newSection4

            val (newSection5, debugStrings) = reader.readSectionAsBytes(sectionHolder, SECTION_OPT_DEBUG_STRINGS)
                .getOrThrow()
            parts.debugStrings = debugStrings
            sectionHolder = newSection5

            val (newSection6, debugLinePrograms) =
                reader.readSectionAsBytes(sectionHolder, SECTION_OPT_DEBUG_LINE_PROGRAMS).getOrThrow()
            parts.debugLinePrograms = debugLinePrograms
            sectionHolder = newSection6

            val (newSection7, debugLineProgramRanges) =
                reader.readSectionAsBytes(sectionHolder, SECTION_OPT_DEBUG_LINE_PROGRAM_RANGES).getOrThrow()
            parts.debugLineProgramRanges = debugLineProgramRanges
            sectionHolder = newSection7

            // Skip any unknown optional sections
            while ((sectionHolder.toInt() and 0b10000000) != 0) {
                val sectionLength = reader.readVarintInternal().getOrThrow()
                reader.skip(sectionLength.toInt()).getOrThrow()
                sectionHolder = reader.readByte().getOrThrow()
            }

            // Verify we're at the end
            if (sectionHolder != SECTION_END_OF_FILE) {
                throw ProgramParseError(
                    ProgramParseErrorKind.UnexpectedSection(
                        offset = reader.position - 1,
                        section = sectionHolder
                    )
                )
            }

            parts
        }
    }
}
