package io.forge.jam.pvm.engine

import io.forge.jam.pvm.PvmConstants

/**
 * A memory zone containing multiple non-overlapping chunks.
 * Used for sparse memory allocation in dynamic paging mode.
 */
class MemoryZone(
    val startAddress: UInt,
    var endAddress: UInt,
    private val chunks: MutableList<MemoryChunk> = mutableListOf()
) {
    private val pageSize: UInt = PvmConstants.ZP

    companion object {
        fun create(
            startAddress: UInt,
            endAddress: UInt,
            chunks: List<MemoryChunk> = emptyList()
        ): MemoryZone {
            if (startAddress > endAddress) {
                throw MemoryError.ZoneNotFound(startAddress)
            }

            // Verify chunks are sorted and non-overlapping
            val sortedChunks = chunks.sortedBy { it.startAddress }
            for (i in 0 until sortedChunks.size - 1) {
                if (sortedChunks[i].endAddress > sortedChunks[i + 1].startAddress) {
                    throw MemoryError.ZoneNotFound(startAddress)
                }
            }

            // Verify last chunk doesn't exceed zone boundary
            if (sortedChunks.isNotEmpty() && sortedChunks.last().endAddress > endAddress) {
                throw MemoryError.ZoneNotFound(startAddress)
            }

            return MemoryZone(startAddress, endAddress, sortedChunks.toMutableList())
        }
    }

    private fun searchChunk(address: UInt): Pair<Int, Boolean> {
        var low = 0
        var high = chunks.size

        while (low < high) {
            val mid = low + (high - low) / 2
            val chunk = chunks[mid]

            when {
                chunk.startAddress <= address && address < chunk.endAddress -> return Pair(mid, true)
                chunk.startAddress > address -> high = mid
                else -> low = mid + 1
            }
        }

        return Pair(low, false)
    }

    fun read(address: UInt, length: Int): ByteArray {
        if (length == 0) return ByteArray(0)

        val readEnd = address + length.toUInt()
        if (readEnd < address) {
            throw MemoryError.OutOfMemory(readEnd)
        }
        if (readEnd > endAddress) {
            throw MemoryError.ExceedBoundary(endAddress)
        }

        val result = ByteArray(length)
        var resultOffset = 0
        var currentAddress = address
        val (startIndex, _) = searchChunk(address)
        var chunkIndex = startIndex

        while (currentAddress < readEnd && chunkIndex < chunks.size) {
            val chunk = chunks[chunkIndex]

            // Handle gap before chunk
            if (currentAddress < chunk.startAddress) {
                val gapSize = minOf(
                    (chunk.startAddress - currentAddress).toInt(),
                    (readEnd - currentAddress).toInt()
                )
                // Zero-fill gap (ByteArray is already zero-initialized)
                resultOffset += gapSize
                currentAddress += gapSize.toUInt()
                continue
            }

            // Handle chunk content
            if (currentAddress >= chunk.endAddress) {
                chunkIndex++
                continue
            }

            val chunkOffset = (currentAddress - chunk.startAddress).toInt()
            val bytesToRead = minOf(
                chunk.size - chunkOffset,
                (readEnd - currentAddress).toInt()
            )

            chunk.getData().copyInto(result, resultOffset, chunkOffset, chunkOffset + bytesToRead)
            resultOffset += bytesToRead
            currentAddress += bytesToRead.toUInt()
            chunkIndex++
        }

        // Remaining space after last chunk is already zero (ByteArray default)
        return result
    }

    fun write(address: UInt, values: ByteArray) {
        if (values.isEmpty()) return
        insertOrUpdate(address, values)
    }

    private fun insertOrUpdate(chunkStart: UInt, data: ByteArray) {
        val chunkEnd = chunkStart + data.size.toUInt()
        if (chunkEnd < chunkStart) {
            throw MemoryError.OutOfMemory(chunkEnd)
        }

        // Fast search for exact chunk match
        val (chunkIndex, found) = searchChunk(chunkStart)
        if (found && chunkIndex < chunks.size) {
            val chunk = chunks[chunkIndex]
            if (chunkStart >= chunk.startAddress && chunkEnd <= chunk.endAddress) {
                // Data fits entirely within existing chunk
                chunk.write(chunkStart, data)
                return
            }
        }

        // Check if we're appending at the end
        if (chunks.isNotEmpty() && chunkStart >= chunks.last().endAddress) {
            val newChunk = MemoryChunk.create(chunkStart, data)
            if (chunks.last().endAddress == chunkStart) {
                // Adjacent - append to last chunk
                chunks.last().append(newChunk)
            } else {
                // Gap - add new chunk
                chunks.add(newChunk)
            }
            return
        }

        // Find all overlapping chunks
        var firstIndex = searchChunk(chunkStart).first
        if (firstIndex > 0 && chunks[firstIndex - 1].endAddress > chunkStart) {
            firstIndex--
        }

        var lastIndex = firstIndex
        while (lastIndex < chunks.size && chunks[lastIndex].startAddress < chunkEnd) {
            lastIndex++
        }

        // No overlaps - insert new chunk
        if (firstIndex == lastIndex) {
            chunks.add(firstIndex, MemoryChunk.create(chunkStart, data))
            return
        }

        // Have overlaps - merge into a single chunk
        val startAddr = minOf(chunks[firstIndex].startAddress, chunkStart)
        val endAddr = maxOf(chunks[lastIndex - 1].endAddress, chunkEnd)

        val newChunk = MemoryChunk.create(startAddr, ByteArray((endAddr - startAddr).toInt()))

        // Copy existing chunk data
        for (i in firstIndex until lastIndex) {
            val existingChunk = chunks[i]
            newChunk.write(existingChunk.startAddress, existingChunk.getData())
        }

        // Overwrite with new data
        newChunk.write(chunkStart, data)

        // Replace overlapping chunks with merged chunk
        if (lastIndex - firstIndex == 1) {
            chunks[firstIndex] = newChunk
        } else {
            chunks.subList(firstIndex, lastIndex).clear()
            chunks.add(firstIndex, newChunk)
        }
    }

    fun incrementEnd(increment: UInt) {
        if (endAddress > UInt.MAX_VALUE - increment) {
            throw MemoryError.OutOfMemory(endAddress)
        }
        endAddress += increment
    }

    fun zero(pageIndex: UInt, pages: Int) {
        if (pages == 0) return

        val address = pageIndex * pageSize
        val length = pages * pageSize.toInt()
        insertOrUpdate(address, ByteArray(length))
    }

    fun chunkCount(): Int = chunks.size

    fun clear() {
        chunks.clear()
    }
}
