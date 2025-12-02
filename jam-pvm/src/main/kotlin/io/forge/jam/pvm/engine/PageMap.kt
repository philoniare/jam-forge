package io.forge.jam.pvm.engine

/**
 * - READ_ONLY: Page can be read but not written (R)
 * - READ_WRITE: Page can be read and written (W)
 * - Inaccessible pages are represented by absence in the bitset
 */
enum class PageAccess {
    READ_ONLY,
    READ_WRITE;

    fun isReadable(): Boolean = true
    fun isWritable(): Boolean = this == READ_WRITE
}

/**
 * Per-page access control using efficient bitset operations.
 * Uses two parallel bit arrays (readableBits and writableBits) where each bit
 * represents one memory page. This allows O(1) access checking with minimal
 * memory overhead.
 *
 * @param pageSize The size of each memory page in bytes (typically 4096)
 */
class PageMap(
    val pageSize: UInt
) {
    private val pageSizeShift: Int = pageSize.countTrailingZeroBits()

    // Bitset storage: each Long represents 64 pages
    private var readableBits: LongArray = LongArray(0)
    private var writableBits: LongArray = LongArray(0)
    private var maxPageIndex: UInt = 0u

    companion object {
        private const val BITS_PER_WORD = 64

        /**
         * Creates a PageMap with initial page mappings.
         *
         * @param pageMap List of (address, length, access) tuples defining initial mappings
         * @param pageSize The size of each memory page in bytes
         */
        fun create(
            pageMap: List<Triple<UInt, UInt, PageAccess>>,
            pageSize: UInt
        ): PageMap {
            val map = PageMap(pageSize)

            // First pass: calculate maximum page index needed
            for ((address, length, _) in pageMap) {
                if (length == 0u) continue
                val startIndex = address shr map.pageSizeShift
                val pages = map.numberOfPagesToAccess(address, length.toInt())
                val endIndex = startIndex + pages
                if (endIndex > map.maxPageIndex) {
                    map.maxPageIndex = endIndex
                }
            }

            // Allocate bit arrays
            val wordsNeeded = ((map.maxPageIndex + 63u) / 64u).toInt()
            map.readableBits = LongArray(wordsNeeded)
            map.writableBits = LongArray(wordsNeeded)

            // Second pass: set access bits
            for ((address, length, access) in pageMap) {
                if (length == 0u) continue
                val startIndex = address shr map.pageSizeShift
                val pages = map.numberOfPagesToAccess(address, length.toInt())
                map.setPageAccessRange(startIndex, pages.toInt(), access)
            }

            return map
        }
    }

    /**
     * Calculates the number of pages needed to access a memory range.
     */
    fun numberOfPagesToAccess(address: UInt, length: Int): UInt {
        if (length == 0) return 0u
        val startPage = address shr pageSizeShift
        val endAddress = address + length.toUInt() - 1u
        val endPage = endAddress shr pageSizeShift
        return endPage - startPage + 1u
    }

    /**
     * Ensures the bitset arrays have capacity for the given page index.
     */
    private fun ensureCapacity(pageIndex: UInt) {
        if (pageIndex >= maxPageIndex) {
            maxPageIndex = pageIndex + 1u
            val wordsNeeded = ((maxPageIndex + 63u) / 64u).toInt()
            val currentSize = readableBits.size

            if (wordsNeeded > currentSize) {
                readableBits = readableBits.copyOf(wordsNeeded)
                writableBits = writableBits.copyOf(wordsNeeded)
            }
        }
    }

    /**
     * Checks if a single page is readable.
     */
    private fun isPageReadable(pageIndex: UInt): Boolean {
        val wordIndex = (pageIndex / 64u).toInt()
        if (wordIndex >= readableBits.size) return false
        val bitIndex = (pageIndex % 64u).toInt()
        return (readableBits[wordIndex] and (1L shl bitIndex)) != 0L
    }

    /**
     * Checks if a single page is writable.
     */
    private fun isPageWritable(pageIndex: UInt): Boolean {
        val wordIndex = (pageIndex / 64u).toInt()
        if (wordIndex >= writableBits.size) return false
        val bitIndex = (pageIndex % 64u).toInt()
        return (writableBits[wordIndex] and (1L shl bitIndex)) != 0L
    }

    /**
     * Core function to check if all pages in a range have a certain property.
     * Returns (result, firstFailingPage) where result is true if all pages pass.
     */
    private inline fun checkPagesInRange(
        pageStart: UInt,
        pages: Int,
        bits: LongArray,
        singlePageChecker: (UInt) -> Boolean
    ): Pair<Boolean, UInt> {
        if (pages == 0) return Pair(true, pageStart)

        var currentPage = pageStart
        val pageEnd = pageStart + pages.toUInt()

        while (currentPage < pageEnd) {
            val wordIndex = (currentPage / 64u).toInt()
            val bitIndex = (currentPage % 64u).toInt()
            val bitsInThisWord = minOf(BITS_PER_WORD - bitIndex, (pageEnd - currentPage).toInt())

            // Create mask for the bits in this word
            val mask: Long = if (bitsInThisWord == BITS_PER_WORD) {
                -1L // All bits set
            } else {
                (1L shl bitsInThisWord) - 1L
            }
            val shiftedMask = mask shl bitIndex

            // Check if all required bits are set
            val wordValue = if (wordIndex < bits.size) bits[wordIndex] else 0L
            if ((wordValue and shiftedMask) != shiftedMask) {
                // Some bits aren't set - find which page failed
                for (bit in 0 until bitsInThisWord) {
                    val pageIndex = currentPage + bit.toUInt()
                    if (!singlePageChecker(pageIndex)) {
                        return Pair(false, pageIndex)
                    }
                }
            }

            currentPage += bitsInThisWord.toUInt()
        }

        return Pair(true, pageStart)
    }

    /**
     * Core function to modify bits in a range.
     */
    private inline fun modifyBitsInRange(
        startIndex: UInt,
        pages: UInt,
        modifier: (wordIndex: Int, mask: Long) -> Unit
    ) {
        if (pages == 0u) return

        var currentPage = startIndex
        val endIndex = startIndex + pages

        while (currentPage < endIndex) {
            val wordIndex = (currentPage / 64u).toInt()
            val bitIndex = (currentPage % 64u).toInt()
            val bitsInThisWord = minOf(BITS_PER_WORD - bitIndex, (endIndex - currentPage).toInt())

            val mask: Long = if (bitsInThisWord == BITS_PER_WORD) {
                -1L
            } else {
                (1L shl bitsInThisWord) - 1L
            }
            val shiftedMask = mask shl bitIndex

            modifier(wordIndex, shiftedMask)
            currentPage += bitsInThisWord.toUInt()
        }
    }

    /**
     * Sets access for a range of pages.
     */
    private fun setPageAccessRange(startIndex: UInt, pages: Int, access: PageAccess) {
        if (pages == 0) return

        val endPage = startIndex + pages.toUInt() - 1u
        ensureCapacity(endPage)

        modifyBitsInRange(startIndex, pages.toUInt()) { wordIndex, shiftedMask ->
            when (access) {
                PageAccess.READ_ONLY -> {
                    readableBits[wordIndex] = readableBits[wordIndex] or shiftedMask
                    writableBits[wordIndex] = writableBits[wordIndex] and shiftedMask.inv()
                }

                PageAccess.READ_WRITE -> {
                    readableBits[wordIndex] = readableBits[wordIndex] or shiftedMask
                    writableBits[wordIndex] = writableBits[wordIndex] or shiftedMask
                }
            }
        }
    }

    fun isReadablePages(pageStart: UInt, pages: Int): Pair<Boolean, UInt> {
        return checkPagesInRange(pageStart, pages, readableBits, ::isPageReadable)
    }

    fun isReadable(address: UInt, length: Int): Pair<Boolean, UInt> {
        if (length == 0) return Pair(true, address)
        val startPageIndex = address shr pageSizeShift
        val pages = numberOfPagesToAccess(address, length)
        val (result, page) = isReadablePages(startPageIndex, pages.toInt())
        return Pair(result, page shl pageSizeShift)
    }

    fun isWritablePages(pageStart: UInt, pages: Int): Pair<Boolean, UInt> {
        return checkPagesInRange(pageStart, pages, writableBits, ::isPageWritable)
    }

    fun isWritable(address: UInt, length: Int): Pair<Boolean, UInt> {
        if (length == 0) return Pair(true, address)
        val startPageIndex = address shr pageSizeShift
        val pages = numberOfPagesToAccess(address, length)
        val (result, page) = isWritablePages(startPageIndex, pages.toInt())
        return Pair(result, page shl pageSizeShift)
    }

    fun updatePages(pageIndex: UInt, pages: Int, access: PageAccess) {
        setPageAccessRange(pageIndex, pages, access)
    }

    fun update(address: UInt, length: Int, access: PageAccess) {
        if (length == 0) return
        val startPageIndex = address shr pageSizeShift
        val pages = numberOfPagesToAccess(address, length)
        setPageAccessRange(startPageIndex, pages.toInt(), access)
    }

    fun removeAccessPages(pageIndex: UInt, pages: Int) {
        if (pages == 0) return

        modifyBitsInRange(pageIndex, pages.toUInt()) { wordIndex, shiftedMask ->
            if (wordIndex < readableBits.size) {
                readableBits[wordIndex] = readableBits[wordIndex] and shiftedMask.inv()
                writableBits[wordIndex] = writableBits[wordIndex] and shiftedMask.inv()
            }
        }
    }

    fun removeAccess(address: UInt, length: Int) {
        if (length == 0) return
        val startPageIndex = address shr pageSizeShift
        val pages = numberOfPagesToAccess(address, length)
        removeAccessPages(startPageIndex, pages.toInt())
    }

    fun findGapOrThrow(pages: Int): UInt {
        if (pages == 0) return 0u

        var currentGapStart: UInt? = null
        var gapSize = 0

        // Search a bit beyond maxPageIndex to find gaps after all allocated pages
        val searchLimit = maxOf(maxPageIndex, pages.toUInt() + maxPageIndex + 64u)
        val totalWords = ((searchLimit + 63u) / 64u).toInt()

        for (wordIdx in 0 until totalWords) {
            val readableWord = if (wordIdx < readableBits.size) readableBits[wordIdx] else 0L
            val writableWord = if (wordIdx < writableBits.size) writableBits[wordIdx] else 0L
            val occupiedWord = readableWord or writableWord

            if (occupiedWord == 0L) {
                // Entire word is free
                val pagesInWord = minOf(BITS_PER_WORD, (searchLimit.toInt() - wordIdx * BITS_PER_WORD))
                if (currentGapStart == null) {
                    currentGapStart = (wordIdx * BITS_PER_WORD).toUInt()
                    gapSize = pagesInWord
                } else {
                    gapSize += pagesInWord
                }

                if (gapSize >= pages) {
                    return currentGapStart
                }
                continue
            }

            // Word has some allocated pages - check bit by bit
            val pagesInWord = minOf(BITS_PER_WORD, (searchLimit.toInt() - wordIdx * BITS_PER_WORD))
            for (bitIdx in 0 until pagesInWord) {
                val pageIndex = (wordIdx * BITS_PER_WORD + bitIdx).toUInt()
                val hasAccess = (occupiedWord and (1L shl bitIdx)) != 0L

                if (!hasAccess) {
                    if (currentGapStart == null) {
                        currentGapStart = pageIndex
                        gapSize = 1
                    } else {
                        gapSize++
                    }

                    if (gapSize >= pages) {
                        return currentGapStart
                    }
                } else {
                    currentGapStart = null
                    gapSize = 0
                }
            }
        }

        throw MemoryError.OutOfMemory(0u)
    }

    fun alignToPageStart(address: UInt): UInt {
        return (address shr pageSizeShift) shl pageSizeShift
    }
}
