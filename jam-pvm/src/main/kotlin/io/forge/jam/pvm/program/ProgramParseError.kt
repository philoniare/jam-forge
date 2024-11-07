package io.forge.jam.pvm.program

class ProgramParseError(private val kind: ProgramParseErrorKind) : Exception() {
    companion object {
        @JvmStatic
        fun failedToReadVarint(offset: Int): ProgramParseError =
            ProgramParseError(ProgramParseErrorKind.FailedToReadVarint(offset))

        @JvmStatic
        fun unexpectedEndOfFile(
            offset: Int,
            expectedCount: Int,
            actualCount: Int
        ): ProgramParseError =
            ProgramParseError(
                ProgramParseErrorKind.UnexpectedEnd(
                    offset = offset,
                    expectedCount = expectedCount,
                    actualCount = actualCount
                )
            )


    }

    override fun toString(): String = "ProgramParseError($kind)"

    override val message: String
        get() = when (kind) {
            is ProgramParseErrorKind.FailedToReadVarint ->
                "failed to parse program blob: failed to parse a varint at offset 0x${kind.offset.toString(16)}"

            is ProgramParseErrorKind.FailedToReadStringNonUtf ->
                "failed to parse program blob: failed to parse a string at offset 0x${kind.offset.toString(16)} (not valid UTF-8)"

            is ProgramParseErrorKind.UnexpectedSection ->
                "failed to parse program blob: found unexpected section as offset 0x${kind.offset.toString(16)}: 0x${
                    kind.section.toString(
                        16
                    )
                }"

            is ProgramParseErrorKind.UnexpectedEnd ->
                "failed to parse program blob: unexpected end of file at offset 0x${kind.offset.toString(16)}: " +
                    "expected to be able to read at least ${kind.expectedCount} bytes, found ${kind.actualCount} bytes"

            is ProgramParseErrorKind.UnsupportedVersion ->
                "failed to parse program blob: unsupported version: ${kind.version}"

            is ProgramParseErrorKind.Other ->
                "failed to parse program blob: ${kind.error}"
        }
}


// Sealed class to represent the different error kinds
sealed class ProgramParseErrorKind {
    data class FailedToReadVarint(val offset: Int) : ProgramParseErrorKind()
    data class FailedToReadStringNonUtf(val offset: Int) : ProgramParseErrorKind()
    data class UnexpectedSection(val offset: Int, val section: Byte) : ProgramParseErrorKind()
    data class UnexpectedEnd(
        val offset: Int,
        val expectedCount: Int,
        val actualCount: Int
    ) : ProgramParseErrorKind()

    data class UnsupportedVersion(val version: Byte) : ProgramParseErrorKind()
    data class Other(val error: String) : ProgramParseErrorKind()

    override fun toString(): String = when (this) {
        is FailedToReadVarint -> "FailedToReadVarint(offset=$offset)"
        is FailedToReadStringNonUtf -> "FailedToReadStringNonUtf(offset=$offset)"
        is UnexpectedSection -> "UnexpectedSection(offset=$offset, section=$section)"
        is UnexpectedEnd -> "UnexpectedEnd(offset=$offset, expected_count=$expectedCount, actual_count=$actualCount)"
        is UnsupportedVersion -> "UnsupportedVersion(version=$version)"
        is Other -> "Other($error)"
    }
}
