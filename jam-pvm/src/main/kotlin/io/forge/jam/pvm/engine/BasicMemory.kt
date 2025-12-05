package io.forge.jam.pvm.engine

import io.forge.jam.pvm.PvmConstants

/**
 * Basic memory implementation for non-dynamic paging mode.
 * Implements the Memory interface with per-page access control via PageMap.
 */
class BasicMemory private constructor(
    private var _pageMap: PageMap,
    private val rwData: MutableList<UByte>,
    private val stack: MutableList<UByte>,
    private val aux: MutableList<UByte>,
    private var isMemoryDirty: Boolean,
    private var _heapSize: UInt
) : Memory {

    override val pageMap: PageMap get() = _pageMap

    companion object {
        fun new(): BasicMemory = BasicMemory(
            _pageMap = PageMap(PvmConstants.ZP),
            rwData = mutableListOf(),
            stack = mutableListOf(),
            aux = mutableListOf(),
            isMemoryDirty = false,
            _heapSize = 0u
        )

        fun MutableList<UByte>.resize(newSize: Int, padValue: UByte = 0u) {
            when {
                size > newSize -> subList(newSize, size).clear()
                size < newSize -> addAll(List(newSize - size) { padValue })
            }
        }

        fun alignToNextPageSize(pageSize: Int, size: Int): Int {
            return ((size + pageSize - 1) / pageSize) * pageSize
        }
    }

    /**
     * Returns the current heap size.
     */
    override fun heapSize(): UInt = _heapSize

    /**
     * Checks if a page is mapped in the current memory layout.
     */
    fun isPageMapped(module: Module, address: UInt): Boolean {
        val memoryMap = module.memoryMap()
        return when {
            address >= memoryMap.auxDataAddress ->
                address < memoryMap.auxDataAddress + memoryMap.auxDataSize

            address >= memoryMap.stackAddressLow() ->
                address < memoryMap.stackAddressLow() + memoryMap.stackSize

            address >= memoryMap.rwDataAddress ->
                address < memoryMap.rwDataAddress + memoryMap.rwDataSize

            address >= memoryMap.roDataAddress() ->
                module.interpretedModule() != null &&
                    address < memoryMap.roDataAddress() + module.interpretedModule()!!.roData.size.toUInt()

            else -> false
        }
    }

    /**
     * Marks the memory as dirty, requiring a reset before next use.
     */
    fun markDirty() {
        isMemoryDirty = true
    }

    /**
     * Resets the memory if it's marked as dirty.
     */
    fun reset(module: Module) {
        if (isMemoryDirty) {
            forceReset(module)
        }
    }

    /**
     * Forces a reset of all memory regions and reinitializes PageMap.
     */
    fun forceReset(module: Module) {
        rwData.clear()
        stack.clear()
        aux.clear()
        isMemoryDirty = false

        module.interpretedModule()?.let { interpretedModule ->
            val memoryMap = module.memoryMap()

            // Extend RW data from interpreted module
            rwData.addAll(interpretedModule.rwData.map { it.toUByte() })

            // Resize memory regions to match memory map
            rwData.resize(memoryMap.rwDataSize.toInt())
            stack.resize(memoryMap.stackSize.toInt())
            aux.resize(memoryMap.auxDataSize.toInt())

            // The interpretedModule.heapEmptyPages contains the original heapPages from blob.
            val actualRwDataLen = (interpretedModule.rwData.size).toUInt()
            val pageAlignedRwDataLen = alignToNextPageSize(memoryMap.pageSize.toInt(), actualRwDataLen.toInt()).toUInt()
            val heapEmptyPagesSize = interpretedModule.heapEmptyPages.toUInt() * memoryMap.pageSize
            _heapSize = memoryMap.rwDataAddress + pageAlignedRwDataLen + heapEmptyPagesSize - memoryMap.heapBase
            println("[HEAP-INIT] actualRwDataLen=$actualRwDataLen, pageAlignedRwDataLen=$pageAlignedRwDataLen, heapEmptyPagesSize=$heapEmptyPagesSize, heapPages=${interpretedModule.heapEmptyPages}, _heapSize=$_heapSize")

            initializePageMap(module, memoryMap)
        }
    }

    /**
     * Initializes the PageMap with correct page access permissions.
     */
    private fun initializePageMap(module: Module, memoryMap: io.forge.jam.pvm.Abi.MemoryMap) {
        val pageMapEntries = mutableListOf<Triple<UInt, UInt, PageAccess>>()

        // RO data region - READ_ONLY
        module.interpretedModule()?.let { interpretedModule ->
            if (interpretedModule.roData.isNotEmpty()) {
                pageMapEntries.add(
                    Triple(
                        memoryMap.roDataAddress(),
                        memoryMap.roDataSize,
                        PageAccess.READ_ONLY
                    )
                )
            }
        }

        // RW data region (including heap) - READ_WRITE
        if (memoryMap.rwDataSize > 0u) {
            pageMapEntries.add(
                Triple(
                    memoryMap.rwDataAddress,
                    memoryMap.rwDataSize,
                    PageAccess.READ_WRITE
                )
            )
        }

        // Stack region - READ_WRITE
        if (memoryMap.stackSize > 0u) {
            pageMapEntries.add(
                Triple(
                    memoryMap.stackAddressLow(),
                    memoryMap.stackSize,
                    PageAccess.READ_WRITE
                )
            )
        }

        // Aux data region - typically READ_ONLY, but GP stack region is READ_WRITE
        if (memoryMap.auxDataSize > 0u) {
            // Check if GP stack region overlaps with aux data
            val gpStackLow = PvmConstants.GP_STACK_LOW
            val gpStackBase = PvmConstants.GP_STACK_BASE

            if (gpStackLow >= memoryMap.auxDataAddress &&
                gpStackBase <= memoryMap.auxDataAddress + memoryMap.auxDataSize
            ) {
                // GP stack is within aux data - mark it as READ_WRITE
                // Mark everything before GP stack as READ_ONLY
                if (gpStackLow > memoryMap.auxDataAddress) {
                    pageMapEntries.add(
                        Triple(
                            memoryMap.auxDataAddress,
                            gpStackLow - memoryMap.auxDataAddress,
                            PageAccess.READ_ONLY
                        )
                    )
                }

                // GP stack region - READ_WRITE
                pageMapEntries.add(
                    Triple(
                        gpStackLow,
                        PvmConstants.GP_STACK_SIZE,
                        PageAccess.READ_WRITE
                    )
                )

                // Mark everything after GP stack as READ_ONLY
                val afterGpStack = gpStackBase
                val auxEnd = memoryMap.auxDataAddress + memoryMap.auxDataSize
                if (afterGpStack < auxEnd) {
                    pageMapEntries.add(
                        Triple(
                            afterGpStack,
                            auxEnd - afterGpStack,
                            PageAccess.READ_ONLY
                        )
                    )
                }
            } else {
                // No GP stack overlap - mark entire aux as READ_ONLY
                pageMapEntries.add(
                    Triple(
                        memoryMap.auxDataAddress,
                        memoryMap.auxDataSize,
                        PageAccess.READ_ONLY
                    )
                )
            }
        }

        _pageMap = PageMap.create(pageMapEntries, memoryMap.pageSize)
    }

    // ========== Memory Interface Implementation ==========

    override fun read(address: UInt): UByte {
        ensureReadable(address, 1)
        val bytes = readInternal(address, 1u)
            ?: throw MemoryError.NotReadable(address)
        return bytes[0].toUByte()
    }

    override fun read(address: UInt, length: Int): ByteArray {
        if (length == 0) return ByteArray(0)
        ensureReadable(address, length)
        return readInternal(address, length.toUInt())
            ?: throw MemoryError.NotReadable(address)
    }

    override fun write(address: UInt, value: UByte) {
        ensureWritable(address, 1)
        writeInternal(address, byteArrayOf(value.toByte()))
    }

    override fun write(address: UInt, values: ByteArray) {
        if (values.isEmpty()) return
        ensureWritable(address, values.size)
        writeInternal(address, values)
    }

    override fun sbrk(size: UInt): UInt {
        throw UnsupportedOperationException("sbrk requires Module context, use sbrk(module, size)")
    }

    /**
     * Implements the sbrk system call to grow heap memory.
     * Returns the PREVIOUS heap end (base of newly allocated memory).
     */
    fun sbrk(module: Module, size: UInt): UInt? {
        val memoryMap = module.memoryMap()

        // Calculate current heap end (heapBase + heapSize)
        val prevHeapEnd = memoryMap.heapBase + _heapSize

        println("[SBRK-KOTLIN] size=$size, heapBase=${memoryMap.heapBase}, _heapSize=$_heapSize, prevHeapEnd=$prevHeapEnd")
        println("[SBRK-KOTLIN] rwDataAddress=${memoryMap.rwDataAddress}, rwDataSize=${memoryMap.rwDataSize}")

        // If size is 0, just return current heap end
        if (size == 0u) {
            return prevHeapEnd
        }

        // Check for overflow
        val newHeapSize = _heapSize.plus(size).takeIf { it >= _heapSize } ?: return null

        if (newHeapSize > memoryMap.maxHeapSize) {
            return null
        }

        _heapSize = newHeapSize
        val newHeapEnd = memoryMap.heapBase + newHeapSize

        // Expand rwData if needed
        if (newHeapEnd.toInt() > memoryMap.rwDataAddress.toInt() + rwData.size) {
            val newSize = alignToNextPageSize(
                pageSize = memoryMap.pageSize.toInt(),
                size = newHeapEnd.toInt()
            ) - memoryMap.rwDataAddress.toInt()

            rwData.resize(newSize)
        }

        // Update PageMap for newly allocated pages
        val prevPageBoundary = alignToNextPageSize(memoryMap.pageSize.toInt(), prevHeapEnd.toInt())
        if (newHeapEnd.toInt() > prevPageBoundary) {
            val startPage = prevPageBoundary.toUInt() / memoryMap.pageSize
            val endPage =
                alignToNextPageSize(memoryMap.pageSize.toInt(), newHeapEnd.toInt()).toUInt() / memoryMap.pageSize
            val pageCount = (endPage - startPage).toInt()
            if (pageCount > 0) {
                _pageMap.updatePages(startPage, pageCount, PageAccess.READ_WRITE)
            }
        }

        // Return the PREVIOUS heap end (base of newly allocated memory)
        return prevHeapEnd
    }

    // ========== Internal Methods ==========

    /**
     * Gets a read-only slice of memory (internal implementation).
     */
    private fun readInternal(address: UInt, length: UInt): ByteArray? {
        val memoryMap = getMemoryMapFromContext() ?: return null

        val (start, memorySlice) = when {
            address >= memoryMap.auxDataAddress ->
                memoryMap.auxDataAddress to aux

            // GP standard stack region (readable from aux data space)
            address >= PvmConstants.GP_STACK_LOW && address < PvmConstants.GP_STACK_BASE ->
                memoryMap.auxDataAddress to aux

            address >= memoryMap.stackAddressLow() ->
                memoryMap.stackAddressLow() to stack

            address >= memoryMap.rwDataAddress ->
                memoryMap.rwDataAddress to rwData

            address >= memoryMap.roDataAddress() -> {
                val interpretedModule = currentModule?.interpretedModule() ?: return null
                memoryMap.roDataAddress() to interpretedModule.roData.map { it.toUByte() }
            }

            else -> return null
        }

        val offset = (address - start).toInt()
        return memorySlice
            .subList(offset, (offset + length.toInt()).coerceAtMost(memorySlice.size))
            .map { it.toByte() }
            .toByteArray()
    }

    /**
     * Writes to memory (internal implementation).
     */
    private fun writeInternal(address: UInt, data: ByteArray) {
        val memoryMap = getMemoryMapFromContext() ?: return

        val (start, memorySlice) = when {
            // GP standard stack region is writable (falls within aux data space)
            address >= PvmConstants.GP_STACK_LOW && address < PvmConstants.GP_STACK_BASE ->
                memoryMap.auxDataAddress to aux

            address >= memoryMap.stackAddressLow() ->
                memoryMap.stackAddressLow() to stack

            address >= memoryMap.rwDataAddress ->
                memoryMap.rwDataAddress to rwData

            else -> return
        }

        isMemoryDirty = true
        val offset = (address - start).toInt()
        data.forEachIndexed { index, byte ->
            if (offset + index < memorySlice.size) {
                memorySlice[offset + index] = byte.toUByte()
            }
        }
    }

    // Thread-local storage for current module context
    private var currentModule: Module? = null

    /**
     * Sets the current module context for memory operations.
     */
    fun setModuleContext(module: Module) {
        currentModule = module
    }

    private fun getMemoryMapFromContext(): io.forge.jam.pvm.Abi.MemoryMap? {
        return currentModule?.memoryMap()
    }

    // ========== Legacy Methods for Backward Compatibility ==========

    /**
     * Gets a read-only slice of memory (legacy method for Visitor compatibility).
     */
    fun getMemorySlice(module: Module, address: UInt, length: UInt): ByteArray? {
        currentModule = module
        return readInternal(address, length)
    }

    /**
     * Gets a mutable slice of memory (legacy method for Visitor compatibility).
     */
    fun getMemorySliceMut(
        module: Module,
        address: UInt,
        length: UInt,
        isExternal: Boolean = false
    ): MutableList<UByte>? {
        currentModule = module
        val memoryMap = module.memoryMap()

        val (start, memorySlice) = when {
            isExternal && address >= memoryMap.auxDataAddress ->
                memoryMap.auxDataAddress to aux

            // GP standard stack region is writable (falls within aux data space)
            address >= PvmConstants.GP_STACK_LOW && address < PvmConstants.GP_STACK_BASE ->
                memoryMap.auxDataAddress to aux

            address >= memoryMap.stackAddressLow() ->
                memoryMap.stackAddressLow() to stack

            address >= memoryMap.rwDataAddress ->
                memoryMap.rwDataAddress to rwData

            else -> return null
        }

        isMemoryDirty = true
        val offset = (address - start).toInt()
        return memorySlice.subList(offset, (offset + length.toInt()).coerceAtMost(memorySlice.size))
    }

    /**
     * Checks if a memory range is writable (legacy method for Visitor compatibility).
     */
    fun isWritable(module: Module, address: UInt, length: UInt): Boolean {
        // Use PageMap for access control
        return pageMap.isWritable(address, length.toInt()).first
    }
}

/**
 * Extension function for BasicMemory to check readability.
 */
private fun BasicMemory.ensureReadable(address: UInt, length: Int) {
    val readResult = pageMap.isReadable(address, length)
    if (!readResult.first) {
        throw MemoryError.NotReadable(readResult.second)
    }
}

/**
 * Extension function for BasicMemory to check writability.
 */
private fun BasicMemory.ensureWritable(address: UInt, length: Int) {
    val writeResult = pageMap.isWritable(address, length)
    if (!writeResult.first) {
        throw MemoryError.NotWritable(writeResult.second)
    }
}
