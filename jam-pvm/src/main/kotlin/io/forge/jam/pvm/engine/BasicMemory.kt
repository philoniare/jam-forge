package io.forge.jam.pvm.engine

/**
 * Internal implementation of basic memory management for the VM
 */
class BasicMemory private constructor(
    private val rwData: MutableList<UByte>,
    private val stack: MutableList<UByte>,
    private val aux: MutableList<UByte>,
    private var isMemoryDirty: Boolean,
    private var heapSize: UInt
) {
    companion object {
        fun new(): BasicMemory = BasicMemory(
            rwData = mutableListOf(),
            stack = mutableListOf(),
            aux = mutableListOf(),
            isMemoryDirty = false,
            heapSize = 0u
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

    fun isPageMapped(module: Module, address: UInt): Boolean {
        val memoryMap = module.memoryMap()
        return when {
            // Check if address falls within any valid memory region
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
     * Returns the current heap size
     */
    fun heapSize(): UInt = heapSize

    /**
     * Checks if a memory range is writable
     */
    fun isWritable(module: Module, address: UInt, length: UInt): Boolean {
        val memoryMap = module.memoryMap()

        // Get end address, checking for overflow
        val endAddress = address.plus(length).takeIf { it >= address } ?: return false

        return when {
            // RO data region is never writable
            address < memoryMap.rwDataAddress -> false
            // Aux data is only writable for external access
            address >= memoryMap.auxDataAddress -> false
            // Stack region is always writable
            address >= memoryMap.stackAddressLow() ->
                endAddress <= (memoryMap.stackAddressLow() + memoryMap.stackSize)
            // RW data region (including heap)
            address >= memoryMap.rwDataAddress -> {
                val rwRegionEnd = memoryMap.rwDataAddress + rwData.size.toUInt()
                endAddress <= rwRegionEnd
            }

            else -> false
        }
    }

    /**
     * Marks the memory as dirty, requiring a reset before next use
     */
    fun markDirty() {
        isMemoryDirty = true
    }

    /**
     * Resets the memory if it's marked as dirty
     */
    fun reset(module: Module) {
        if (isMemoryDirty) {
            forceReset(module)
        }
    }

    /**
     * Forces a reset of all memory regions
     */
    fun forceReset(module: Module) {
        rwData.clear()
        stack.clear()
        aux.clear()
        heapSize = 0u
        isMemoryDirty = false

        module.interpretedModule()?.let { interpretedModule ->
            // Extend RW data from interpreted module
            rwData.addAll(interpretedModule.rwData.map { it.toUByte() })

            // Resize memory regions to match memory map
            rwData.resize(module.memoryMap().rwDataSize.toInt())
            stack.resize(module.memoryMap().stackSize.toInt())
            aux.resize(module.memoryMap().auxDataSize.toInt())
        }
    }

    /**
     * Gets a read-only slice of memory
     */
    fun getMemorySlice(module: Module, address: UInt, length: UInt): ByteArray? {
        println("[DEBUG-MEM] Reading memory at $address, length $length")
        val memoryMap = module.memoryMap()
        val (start, memorySlice) = when {
            address >= memoryMap.auxDataAddress ->
                memoryMap.auxDataAddress to aux

            address >= memoryMap.stackAddressLow() ->
                memoryMap.stackAddressLow() to stack

            address >= memoryMap.rwDataAddress ->
                memoryMap.rwDataAddress to rwData

            address >= memoryMap.roDataAddress() -> {
                val interpretedModule = module.interpretedModule() ?: return null
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
     * Gets a mutable slice of memory
     */
    fun getMemorySliceMut(
        module: Module,
        address: UInt,
        length: UInt,
        isExternal: Boolean = false
    ): MutableList<UByte>? {
        val memoryMap = module.memoryMap()
        val (start, memorySlice) = when {
            isExternal && address >= memoryMap.auxDataAddress ->
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
     * Implements the sbrk system call to grow heap memory
     */
    fun sbrk(module: Module, size: UInt): UInt? {
        // Check for overflow
        val newHeapSize = heapSize.plus(size).takeIf { it >= heapSize } ?: run {
            return null
        }

        val memoryMap = module.memoryMap()
        if (newHeapSize > memoryMap.maxHeapSize) {
            return null
        }

        heapSize = newHeapSize
        val heapTop = memoryMap.heapBase + newHeapSize

        if (heapTop.toInt() > memoryMap.rwDataAddress.toInt() + rwData.size) {
            val newSize = alignToNextPageSize(
                pageSize = memoryMap.pageSize.toInt(),
                size = heapTop.toInt()
            ) - memoryMap.rwDataAddress.toInt()

            rwData.resize(newSize)
        }

        return heapTop
    }
}
