package io.forge.jam.pvm.engine

import io.forge.jam.pvm.PvmConstants

/**
 * General memory implementation for dynamic paging mode.
 * Uses sparse chunk-based storage with per-page access control.
 */
class GeneralMemory private constructor(
    override val pageMap: PageMap,
    private val zone: MemoryZone
) : Memory {

    companion object {
        /**
         * Creates a new GeneralMemory with initial page mappings and chunks.
         *
         * @param pageMapEntries List of (address, length, writable) tuples
         * @param chunks List of (address, data) tuples for initial memory content
         */
        fun create(
            pageMapEntries: List<Triple<UInt, UInt, Boolean>> = emptyList(),
            chunks: List<Pair<UInt, ByteArray>> = emptyList()
        ): GeneralMemory {
            val pageSize = PvmConstants.ZP

            // Convert to PageMap format
            val pageMap = PageMap.create(
                pageMapEntries.map { (address, length, writable) ->
                    Triple(address, length, if (writable) PageAccess.READ_WRITE else PageAccess.READ_ONLY)
                },
                pageSize
            )

            // Create memory chunks
            val memoryChunks = chunks.map { (address, data) ->
                MemoryChunk.create(address, data)
            }

            // Create zone spanning entire address space
            val zone = MemoryZone.create(0u, UInt.MAX_VALUE, memoryChunks)

            return GeneralMemory(pageMap, zone)
        }

        /**
         * Creates a new empty GeneralMemory.
         */
        fun new(): GeneralMemory = create()
    }

    override fun read(address: UInt): UByte {
        ensureReadable(address, 1)
        val data = zone.read(address, 1)
        return data[0].toUByte()
    }

    override fun read(address: UInt, length: Int): ByteArray {
        if (length == 0) return ByteArray(0)
        ensureReadable(address, length)
        return zone.read(address, length)
    }

    override fun write(address: UInt, value: UByte) {
        ensureWritable(address, 1)
        zone.write(address, byteArrayOf(value.toByte()))
    }

    override fun write(address: UInt, values: ByteArray) {
        if (values.isEmpty()) return
        ensureWritable(address, values.size)
        zone.write(address, values)
    }

    override fun heapSize(): UInt = 0u

    /**
     * Allocates additional memory using sbrk.
     * Finds a gap in the PageMap and marks it as read-write.
     *
     * @param size Number of bytes to allocate
     * @return The starting address of the allocated region (page-aligned)
     */
    override fun sbrk(size: UInt): UInt {
        // It just finds a gap and allocates pages
        val pageSize = PvmConstants.ZP.toInt()
        val pagesNeeded = (size.toInt() + pageSize - 1) / pageSize

        // Find a gap in the PageMap
        val pageIndex = pageMap.findGapOrThrow(pagesNeeded)

        // Mark pages as read-write
        pageMap.updatePages(pageIndex, pagesNeeded, PageAccess.READ_WRITE)

        return pageIndex * pageSize.toUInt()
    }

    /**
     * Manages page access dynamically.
     *
     * @param pageIndex Starting page index
     * @param pages Number of pages
     * @param variant Operation variant:
     *   - 0: Remove all access
     *   - 1: Read-only access, zero pages
     *   - 2: Read-write access, zero pages
     *   - 3: Read-only access, no zeroing
     *   - 4: Read-write access, no zeroing
     */
    fun pages(pageIndex: UInt, pages: Int, variant: ULong) {
        when (variant.toInt()) {
            0 -> pageMap.removeAccessPages(pageIndex, pages)
            1, 3 -> pageMap.updatePages(pageIndex, pages, PageAccess.READ_ONLY)
            2, 4 -> pageMap.updatePages(pageIndex, pages, PageAccess.READ_WRITE)
        }

        // Zero pages for variants < 3
        if (variant < 3u) {
            zone.zero(pageIndex, pages)
        }
    }

    /**
     * Clears all memory.
     */
    fun clear() {
        zone.clear()
    }

    // ========== Private Helpers ==========

    private fun ensureReadable(address: UInt, length: Int) {
        val (result, failedAddress) = pageMap.isReadable(address, length)
        if (!result) {
            throw MemoryError.NotReadable(failedAddress)
        }
    }

    private fun ensureWritable(address: UInt, length: Int) {
        val (result, failedAddress) = pageMap.isWritable(address, length)
        if (!result) {
            throw MemoryError.NotWritable(failedAddress)
        }
    }
}
