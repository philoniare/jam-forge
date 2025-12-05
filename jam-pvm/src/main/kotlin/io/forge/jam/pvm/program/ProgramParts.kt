package io.forge.jam.pvm.program

import io.forge.jam.pvm.engine.InstructionSetKind
import java.nio.ByteBuffer
import java.nio.ByteOrder

data class ProgramParts(
    var isaKind: InstructionSetKind? = null,  // Track the ISA kind for proper instruction validation
    var is64Bit: Boolean = false,
    var roDataSize: UInt = 0u,
    var rwDataSize: UInt = 0u,
    var actualRwDataLen: UInt = 0u,  // The actual rwData content length (without heap pages)
    var stackSize: UInt = 0u,
    var heapPages: UInt = 0u,  // Number of heap empty pages from program blob
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

    fun setCodeJumpTable(blob: ByteArray): Result<ProgramParts> = runCatching {
        this.codeAndJumpTable = ArcBytes.fromStatic(blob)
        this
    }

    companion object {
        // Polkavm blob magic bytes: "PVM\0"
        private val BLOB_MAGIC = byteArrayOf(0x50, 0x56, 0x4D, 0x00)
        private const val BLOB_MAGIC_SIZE = 4

        // Generic blob magic bytes: 'G' (0x47) or 'P' (0x50)
        private const val GENERIC_MAGIC_G: Byte = 0x47  // 'G'
        private const val GENERIC_MAGIC_P: Byte = 0x50  // 'P'

        private const val BLOB_LEN_SIZE: Int = Long.SIZE_BYTES
        private const val VM_MAXIMUM_IMPORT_COUNT: UInt = 1024u

        // JAM blob format constants
        private const val JAM_INIT_ZONE_SIZE: UInt = 65536u  // 2^16
        private const val JAM_PAGE_SIZE: UInt = 4096u

        // Section constants
        private const val SECTION_MEMORY_CONFIG: Byte = 1

        fun fromJamBytes(blob: ArcBytes): Result<ProgramParts> = runCatching {
            val bytes = blob.toByteArray()
            if (bytes.size < 15) {
                throw ProgramParseError(ProgramParseErrorKind.Other("JAM blob too short"))
            }

            var offset = 0

            // Read |o| (3 bytes little-endian)
            val roDataLen = readLE3(bytes, offset)
            offset += 3

            // Read |w| (3 bytes little-endian)
            val rwDataLen = readLE3(bytes, offset)
            offset += 3

            // Read z (2 bytes little-endian) - heap pages
            val heapPages = readLE2(bytes, offset)
            offset += 2

            // Read s (3 bytes little-endian) - stack size
            val stackSize = readLE3(bytes, offset)
            offset += 3

            // Read o (ro_data)
            if (offset + roDataLen.toInt() > bytes.size) {
                throw ProgramParseError(ProgramParseErrorKind.Other("JAM blob truncated: ro_data extends beyond end"))
            }
            val roData = bytes.copyOfRange(offset, offset + roDataLen.toInt())
            offset += roDataLen.toInt()

            // Read w (rw_data)
            if (offset + rwDataLen.toInt() > bytes.size) {
                throw ProgramParseError(ProgramParseErrorKind.Other("JAM blob truncated: rw_data extends beyond end"))
            }
            val rwData = bytes.copyOfRange(offset, offset + rwDataLen.toInt())
            offset += rwDataLen.toInt()

            // Read |c| (4 bytes little-endian) - code+jumptable length
            if (offset + 4 > bytes.size) {
                throw ProgramParseError(ProgramParseErrorKind.Other("JAM blob truncated: missing code length"))
            }
            val codeLen = readLE4(bytes, offset)
            offset += 4

            // Read c (code+jumptable)
            if (offset + codeLen.toInt() > bytes.size) {
                throw ProgramParseError(ProgramParseErrorKind.Other("JAM blob truncated: code extends beyond end"))
            }
            val codeAndJumpTable = bytes.copyOfRange(offset, offset + codeLen.toInt())

            // Calculate heap size in bytes from heap pages
            val heapSize = heapPages.toUInt() * JAM_PAGE_SIZE

            ProgramParts(
                isaKind = InstructionSetKind.JamV1,  // JAM format implies JamV1 ISA
                is64Bit = true,  // JamV1 uses 64-bit registers (though 32-bit addressing)
                roDataSize = roDataLen.toUInt(),
                rwDataSize = rwDataLen.toUInt() + heapSize,
                actualRwDataLen = rwDataLen.toUInt(),  // Store the actual rwData length (without heap)
                stackSize = stackSize.toUInt(),
                heapPages = heapPages.toUInt(),  // Store the heap pages count
                roData = ArcBytes.fromStatic(roData),
                rwData = ArcBytes.fromStatic(rwData),
                codeAndJumpTable = ArcBytes.fromStatic(codeAndJumpTable)
            )
        }
        
        fun fromGenericBytes(blob: ArcBytes): Result<ProgramParts> = runCatching {
            val bytes = blob.toByteArray()
            if (bytes.isEmpty() || (bytes[0] != GENERIC_MAGIC_G && bytes[0] != GENERIC_MAGIC_P)) {
                throw ProgramParseError(ProgramParseErrorKind.Other("Not a Generic blob format"))
            }

            var offset = 1  // Skip 'G' magic byte

            // Skip version byte
            if (offset >= bytes.size) {
                throw ProgramParseError(ProgramParseErrorKind.Other("Generic blob truncated: missing version"))
            }
            offset += 1  // version byte

            // Skip name (length-prefixed string)
            if (offset >= bytes.size) {
                throw ProgramParseError(ProgramParseErrorKind.Other("Generic blob truncated: missing name length"))
            }
            val nameLen = bytes[offset].toInt() and 0xFF
            offset += 1 + nameLen

            // Skip version string (length-prefixed)
            if (offset >= bytes.size) {
                throw ProgramParseError(ProgramParseErrorKind.Other("Generic blob truncated: missing version string length"))
            }
            val versionLen = bytes[offset].toInt() and 0xFF
            offset += 1 + versionLen

            // Skip license (length-prefixed)
            if (offset >= bytes.size) {
                throw ProgramParseError(ProgramParseErrorKind.Other("Generic blob truncated: missing license length"))
            }
            val licenseLen = bytes[offset].toInt() and 0xFF
            offset += 1 + licenseLen

            // Skip authors (count + each length-prefixed)
            if (offset >= bytes.size) {
                throw ProgramParseError(ProgramParseErrorKind.Other("Generic blob truncated: missing author count"))
            }
            val authorCount = bytes[offset].toInt() and 0xFF
            offset += 1

            for (i in 0 until authorCount) {
                if (offset >= bytes.size) {
                    throw ProgramParseError(ProgramParseErrorKind.Other("Generic blob truncated: missing author $i length"))
                }
                val authorLen = bytes[offset].toInt() and 0xFF
                offset += 1 + authorLen
            }

            // Remainder is the JAM blob
            if (offset >= bytes.size) {
                throw ProgramParseError(ProgramParseErrorKind.Other("Generic blob truncated: missing JAM blob"))
            }

            val jamBlob = bytes.copyOfRange(offset, bytes.size)
            fromJamBytes(ArcBytes.fromStatic(jamBlob)).getOrThrow()
        }

        /**
         * Read 2 bytes little-endian.
         */
        private fun readLE2(bytes: ByteArray, offset: Int): Int {
            return (bytes[offset].toInt() and 0xFF) or
                ((bytes[offset + 1].toInt() and 0xFF) shl 8)
        }

        /**
         * Read 3 bytes little-endian.
         */
        private fun readLE3(bytes: ByteArray, offset: Int): Int {
            return (bytes[offset].toInt() and 0xFF) or
                ((bytes[offset + 1].toInt() and 0xFF) shl 8) or
                ((bytes[offset + 2].toInt() and 0xFF) shl 16)
        }

        /**
         * Read 4 bytes little-endian.
         */
        private fun readLE4(bytes: ByteArray, offset: Int): Int {
            return (bytes[offset].toInt() and 0xFF) or
                ((bytes[offset + 1].toInt() and 0xFF) shl 8) or
                ((bytes[offset + 2].toInt() and 0xFF) shl 16) or
                ((bytes[offset + 3].toInt() and 0xFF) shl 24)
        }

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
            val bytes = blob.toByteArray()

            // 1. Check magic bytes first (must be "PVM\0")
            if (bytes.size < BLOB_MAGIC_SIZE) {
                throw ProgramParseError(ProgramParseErrorKind.Other("blob too short"))
            }
            if (!bytes.sliceArray(0 until BLOB_MAGIC_SIZE).contentEquals(BLOB_MAGIC)) {
                throw ProgramParseError(
                    ProgramParseErrorKind.Other("blob doesn't start with expected magic bytes")
                )
            }

            // 2. Create reader starting AFTER magic bytes (offset 4)
            val reader = ArcBytesReader(blob, BLOB_MAGIC_SIZE)

            // 3. Read blob version (now at correct offset 4)
            val blobVersion = reader.readByte().getOrThrow()

            // 4. Determine ISA kind and is64Bit from blob version
            val isaKind = InstructionSetKind.fromBlobVersion(blobVersion)
                ?: throw ProgramParseError(ProgramParseErrorKind.UnsupportedVersion(blobVersion))
            val is64Bit = isaKind.is64Bit()

            // 5. Read blob length and verify
            val blobLen = reader.readSlice(BLOB_LEN_SIZE).getOrThrow()
                .let { ByteBuffer.wrap(it).order(ByteOrder.LITTLE_ENDIAN).long }

            if (blobLen != blob.asRef().size.toLong()) {
                throw ProgramParseError(
                    ProgramParseErrorKind.Other("blob size doesn't match the blob length metadata")
                )
            }

            val parts = ProgramParts(isaKind = isaKind, is64Bit = is64Bit)
            var sectionHolder = reader.readByte().getOrThrow()

            // Read memory config section if present
            if (sectionHolder == SECTION_MEMORY_CONFIG) {
                val sectionLength = reader.readVarintInternal().getOrThrow()
                val position = reader.position

                parts.roDataSize = reader.readVarintInternal().getOrThrow()
                parts.rwDataSize = reader.readVarintInternal().getOrThrow()
                parts.actualRwDataLen = parts.rwDataSize  // For PolkaVM format, no separate heap pages
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
