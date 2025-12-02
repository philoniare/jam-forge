package io.forge.jam.pvm.engine

/**
 * Represents a contiguous block of memory data.
 * Used by MemoryZone for sparse memory allocation.
 */
class MemoryChunk(
    var startAddress: UInt,
    private var data: ByteArray
) {
    val endAddress: UInt get() = startAddress + data.size.toUInt()
    val size: Int get() = data.size

    companion object {
        fun create(startAddress: UInt, data: ByteArray): MemoryChunk {
            val endAddress = startAddress + data.size.toUInt()
            if (endAddress < startAddress && data.isNotEmpty()) {
                throw MemoryError.OutOfMemory(startAddress)
            }
            return MemoryChunk(startAddress, data)
        }
    }

    fun read(address: UInt, length: Int): ByteArray {
        if (address < startAddress || address + length.toUInt() > endAddress) {
            throw MemoryError.ExceedBoundary(address)
        }

        val offset = (address - startAddress).toInt()
        return data.sliceArray(offset until offset + length)
    }

    fun write(address: UInt, values: ByteArray) {
        if (values.isEmpty()) return

        if (address < startAddress || address + values.size.toUInt() > endAddress) {
            throw MemoryError.ExceedBoundary(address)
        }

        val offset = (address - startAddress).toInt()
        values.copyInto(data, offset)
    }

    fun append(chunk: MemoryChunk) {
        if (endAddress != chunk.startAddress) {
            throw MemoryError.NotAdjacent(chunk.startAddress)
        }

        // Check for overflow
        if (chunk.endAddress < chunk.startAddress) {
            throw MemoryError.OutOfMemory(endAddress)
        }

        data = data + chunk.data
    }

    fun zeroExtend(until: UInt) {
        if (until <= endAddress) return

        val extensionSize = (until - endAddress).toInt()
        data = data + ByteArray(extensionSize)
    }

    fun getData(): ByteArray = data

    fun copy(): MemoryChunk = MemoryChunk(startAddress, data.copyOf())
}
