package io.forge.jam.pvm.program

import io.forge.jam.pvm.PvmLogger

class ArcBytesReader(
    blob: ArcBytes,
    position: Int = 0
) : Reader<ArcBytes>(blob, position) {
    override fun clone(): ArcBytesReader = ArcBytesReader(blob, position)

    companion object {
        private val logger = PvmLogger(ArcBytesReader::class.java)
        fun from(blob: ArcBytes): ArcBytesReader = ArcBytesReader(blob)
    }

    fun readSliceAsBytes(length: Int): Result<ArcBytes> = runCatching {
        val range = readSliceAsRange(length).getOrThrow()
        blob.subslice(range)
    }

    fun readSectionAsBytes(
        outSection: Byte,
        expectedSection: Byte
    ): Result<Pair<Byte, ArcBytes>> = runCatching {
        if (outSection != expectedSection) {
            return@runCatching Pair(outSection, ArcBytes.empty())
        }

        val sectionLength = readVarintInternal().getOrThrow().toInt()
        val range = readSliceAsRange(sectionLength).getOrThrow()
        val result = blob.subslice(range)
        position += sectionLength
        val newSection = readByte().getOrThrow()
        Pair(newSection, result)
    }

    fun readBytesWithLengthAsArcBytes(): Result<ArcBytes> = runCatching {
        val length = readVarintInternal().getOrThrow().toInt()
        readSliceAsBytes(length).getOrThrow()
    }
}
