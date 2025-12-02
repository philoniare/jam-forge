package io.forge.jam.pvm.engine

/**
 * Memory errors
 */
sealed class MemoryError(val address: UInt) : Exception() {
    class ZoneNotFound(address: UInt) : MemoryError(alignToPage(address)) {
        override val message: String get() = "zone not found at 0x${address.toString(16)}"
    }

    class ChunkNotFound(address: UInt) : MemoryError(alignToPage(address)) {
        override val message: String get() = "chunk not found at 0x${address.toString(16)}"
    }

    class NotReadable(address: UInt) : MemoryError(alignToPage(address)) {
        override val message: String get() = "memory not readable at 0x${address.toString(16)}"
    }

    class NotWritable(address: UInt) : MemoryError(alignToPage(address)) {
        override val message: String get() = "memory not writable at 0x${address.toString(16)}"
    }

    class OutOfMemory(address: UInt) : MemoryError(alignToPage(address)) {
        override val message: String get() = "out of memory at 0x${address.toString(16)}"
    }

    class ExceedBoundary(address: UInt) : MemoryError(alignToPage(address)) {
        override val message: String get() = "memory access exceeds boundary at 0x${address.toString(16)}"
    }

    class NotAdjacent(address: UInt) : MemoryError(alignToPage(address)) {
        override val message: String get() = "chunks not adjacent at 0x${address.toString(16)}"
    }

    companion object {
        private const val PAGE_SIZE = 4096u

        fun alignToPage(address: UInt): UInt =
            (address / PAGE_SIZE) * PAGE_SIZE
    }
}

interface Memory {
    /**
     * The PageMap for per-page access control.
     */
    val pageMap: PageMap

    /**
     * Reads a single byte from memory.
     * @throws MemoryError.NotReadable if the address is not readable
     */
    fun read(address: UInt): UByte

    /**
     * Reads multiple bytes from memory.
     * @throws MemoryError.NotReadable if any address in the range is not readable
     */
    fun read(address: UInt, length: Int): ByteArray

    /**
     * Writes a single byte to memory.
     * @throws MemoryError.NotWritable if the address is not writable
     */
    fun write(address: UInt, value: UByte)

    /**
     * Writes multiple bytes to memory.
     * @throws MemoryError.NotWritable if any address in the range is not writable
     */
    fun write(address: UInt, values: ByteArray)

    /**
     * Allocates additional heap memory (sbrk syscall).
     * @param size Number of bytes to allocate
     * @return The previous heap end address (base of newly allocated memory)
     * @throws MemoryError.OutOfMemory if allocation fails
     */
    fun sbrk(size: UInt): UInt

    /**
     * Returns the current heap size.
     */
    fun heapSize(): UInt
}


/**
 * Checks if an address range is readable.
 */
fun Memory.isReadable(address: UInt, length: Int): Boolean {
    if (length == 0) return true
    return pageMap.isReadable(address, length).first
}

/**
 * Checks if an address range is writable.
 */
fun Memory.isWritable(address: UInt, length: Int): Boolean {
    if (length == 0) return true
    return pageMap.isWritable(address, length).first
}

/**
 * Ensures an address range is readable, throws if not.
 * @throws MemoryError.NotReadable with page-aligned address if not readable
 */
fun Memory.ensureReadable(address: UInt, length: Int) {
    val (result, failedAddress) = pageMap.isReadable(address, length)
    if (!result) {
        throw MemoryError.NotReadable(failedAddress)
    }
}

/**
 * Ensures an address range is writable, throws if not.
 * @throws MemoryError.NotWritable with page-aligned address if not writable
 */
fun Memory.ensureWritable(address: UInt, length: Int) {
    val (result, failedAddress) = pageMap.isWritable(address, length)
    if (!result) {
        throw MemoryError.NotWritable(failedAddress)
    }
}

/**
 * Read-only wrapper around a Memory instance.
 * Only exposes read operations.
 */
class ReadonlyMemory(private val memory: Memory) {
    fun read(address: UInt): UByte = memory.read(address)
    fun read(address: UInt, length: Int): ByteArray = memory.read(address, length)
}
