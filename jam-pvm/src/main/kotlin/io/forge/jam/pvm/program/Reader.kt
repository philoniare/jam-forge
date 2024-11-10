package io.forge.jam.pvm.program

import io.forge.jam.pvm.PvmLogger
import io.forge.jam.pvm.readVarint

open class Reader<T>(
    val blob: T,
    var position: Int = 0
) : Cloneable where T : Any {

    public override fun clone(): Reader<T> = Reader(blob, position)

    companion object {
        private val logger = PvmLogger(Reader::class.java)
        fun <T> from(blob: T): Reader<T> where T : Any = Reader(blob)
    }

    private fun T.asBytes(): ByteArray = when (this) {
        is ByteArray -> this
        is String -> this.encodeToByteArray()
        is ArcBytes -> this.asRef()
        else -> throw IllegalArgumentException("Type must be convertible to ByteArray")
    }

    fun skip(count: Int): Result<Unit> = runCatching {
        readSliceAsRange(count).map { }
    }

    fun readByte(): Result<Byte> = runCatching {
        readSlice(1).getOrThrow().first()
    }

    open fun readSlice(length: Int): Result<ByteArray> = runCatching {
        val blobBytes = blob.asBytes()

        val remainingBlob = blobBytes.copyOfRange(position, blobBytes.size)

        if (length > remainingBlob.size) {
            throw ProgramParseError.unexpectedEndOfFile(
                position,
                length,
                remainingBlob.size
            )
        }
        val slice = remainingBlob.copyOfRange(0, length)
        position += length
        slice
    }

    fun readVarintInternal(): Result<UInt> = runCatching {
        val firstByte = readByte().getOrThrow()
        val blobBytes = blob.asBytes()
        val remainingBytes = blobBytes.copyOfRange(position, blobBytes.size)
        val (length, value) = readVarint(
            remainingBytes.toUByteArray(),
            firstByte.toUByte()
        ) ?: throw ProgramParseError.failedToReadVarint(position - 1)
        position += length.toInt()
        value
    }

    fun readBytesWithLength(): Result<ByteArray> = runCatching {
        val length = readVarintInternal().getOrThrow().toInt()
        readSlice(length).getOrThrow()
    }

    fun readStringWithLength(): Result<String> = runCatching {
        val offset = position
        val slice = readBytesWithLength().getOrThrow()
        String(slice).also {
            if (!it.isValidUtf8()) {
                throw ProgramParseError(ProgramParseErrorKind.FailedToReadStringNonUtf(offset))
            }
        }
    }

    fun readSliceAsRange(count: Int): Result<IntRange> = runCatching {
        val blobBytes = blob.asBytes()
        val remainingBytes = blobBytes.copyOfRange(position, blobBytes.size)
        if (count > blobBytes.size) {
            throw ProgramParseError.unexpectedEndOfFile(
                position,
                count,
                remainingBytes.size
            )
        }
        val range = position..(position + count)
        position += count
        range
    }
}

private fun String.isValidUtf8(): Boolean = try {
    this.encodeToByteArray().decodeToString()
    true
} catch (e: Exception) {
    false
}
