package io.forge.jam.pvm

object Abi {
    // Constants for the VM memory layout.
    const val ADDRESS_SPACE_SIZE: ULong = 0x100000000UL

    // The minimum page size of the VM.
    const val VM_MIN_PAGE_SIZE: UInt = 0x1000U

    // The maximum page size of the VM.
    const val VM_MAX_PAGE_SIZE: UInt = 0x10000U

    init {
        require(VM_MIN_PAGE_SIZE <= VM_MAX_PAGE_SIZE) {
            "VM_MIN_PAGE_SIZE must be less than or equal to VM_MAX_PAGE_SIZE"
        }
    }

    // Memory layout constants
    private const val VM_ADDRESS_SPACE_BOTTOM: UInt = VM_MAX_PAGE_SIZE
    val VM_ADDRESS_SPACE_TOP: UInt = (ADDRESS_SPACE_SIZE - VM_MAX_PAGE_SIZE.toULong()).toUInt()


    // Special addresses and limits
    const val VM_ADDR_RETURN_TO_HOST: UInt = 0xffff0000U
    val VM_MAXIMUM_CODE_SIZE: UInt = 32U * 1024U * 1024U
    val VM_MAXIMUM_JUMP_TABLE_ENTRIES: UInt = 16U * 1024U * 1024U
    const val VM_MAXIMUM_IMPORT_COUNT: UInt = 1024U
    const val VM_CODE_ADDRESS_ALIGNMENT: UInt = 2U

    init {
        require(VM_ADDR_RETURN_TO_HOST and 0b11U == 0U) {
            "VM_ADDR_RETURN_TO_HOST must be aligned to 4 bytes"
        }
    }

    data class MemoryMap(
        val pageSize: UInt,
        val roDataSize: UInt,
        val rwDataAddress: UInt,
        val rwDataSize: UInt,
        val stackAddressHigh: UInt,
        val stackSize: UInt,
        val auxDataAddress: UInt,
        val auxDataSize: UInt,
        val heapBase: UInt,
        val maxHeapSize: UInt
    ) {
        // Memory region getters
        fun roDataAddress(): UInt = VM_ADDRESS_SPACE_BOTTOM

        fun roDataRange(): UIntRange = roDataAddress()..(roDataAddress() + roDataSize)
        fun rwDataRange(): UIntRange = rwDataAddress..(rwDataAddress + rwDataSize)
        fun stackAddressLow(): UInt = stackAddressHigh - stackSize
        fun stackRange(): UIntRange = stackAddressLow()..stackAddressHigh
        fun auxDataRange(): UIntRange = auxDataAddress..(auxDataAddress + auxDataSize)
    }

    class MemoryMapBuilder {
        private var pageSize: UInt = 0U
        private var roDataSize: UInt = 0U
        private var rwDataSize: UInt = 0U
        private var actualRwDataLen: UInt = 0U  // The actual rwData content length (without heap pages)
        private var stackSize: UInt = 0U
        private var auxDataSize: UInt = 0U

        companion object {
            fun new(pageSize: UInt) = MemoryMapBuilder().apply {
                this.pageSize = pageSize
            }
        }

        fun roDataSize(value: UInt) = apply { roDataSize = value }
        fun rwDataSize(value: UInt) = apply { rwDataSize = value }
        fun actualRwDataLen(value: UInt) = apply { actualRwDataLen = value }
        fun stackSize(value: UInt) = apply { stackSize = value }
        fun auxDataSize(value: UInt) = apply { auxDataSize = value }

        @Suppress("ComplexMethod")
        fun build(): Result<MemoryMap> = runCatching {
            // Validate page size
            when {
                pageSize < VM_MIN_PAGE_SIZE -> throw IllegalStateException("invalid page size: page size is too small")
                pageSize > VM_MAX_PAGE_SIZE -> throw IllegalStateException("invalid page size: page size is too big")
                !pageSize.isPowerOfTwo() -> throw IllegalStateException("invalid page size: page size is not a power of two")
            }

            // Align and validate memory regions
            val roDataAddressSpace = Utils.alignToNextPageLong(
                VM_MAX_PAGE_SIZE.toLong(),
                roDataSize.toLong()
            )?.toULong() ?: throw IllegalStateException("the size of read-only data is too big")

            val alignedRoDataSize = Utils.alignToNextPageInt(
                pageSize.toInt(),
                roDataSize.toInt()
            )?.toUInt() ?: throw IllegalStateException("the size of read-only data is too big")

            val rwDataAddressSpace = Utils.alignToNextPageLong(
                VM_MAX_PAGE_SIZE.toLong(),
                rwDataSize.toLong()
            )?.toULong() ?: throw IllegalStateException("the size of read-write data is too big")

            val originalRwDataSize = rwDataSize
            val alignedRwDataSize = Utils.alignToNextPageInt(
                pageSize.toInt(),
                rwDataSize.toInt()
            )?.toUInt() ?: throw IllegalStateException("the size of read-write data is too big")

            val stackAddressSpace = Utils.alignToNextPageLong(
                VM_MAX_PAGE_SIZE.toLong(),
                stackSize.toLong()
            )?.toULong() ?: throw IllegalStateException("the size of the stack is too big")

            val alignedStackSize = Utils.alignToNextPageInt(
                pageSize.toInt(),
                stackSize.toInt()
            )?.toUInt() ?: throw IllegalStateException("the size of the stack is too big")

            val auxDataAddressSpace = Utils.alignToNextPageLong(
                VM_MAX_PAGE_SIZE.toLong(),
                auxDataSize.toLong()
            )?.toULong() ?: throw IllegalStateException("the size of the aux data is too big")

            val alignedAuxDataSize = Utils.alignToNextPageInt(
                pageSize.toInt(),
                auxDataSize.toInt()
            )?.toUInt() ?: throw IllegalStateException("the size of the aux data is too big")

            // Calculate memory layout
            var addressLow: ULong = 0U
            addressLow += VM_ADDRESS_SPACE_BOTTOM.toULong()
            addressLow += roDataAddressSpace
            addressLow += VM_MAX_PAGE_SIZE.toULong()

            val rwDataAddress = addressLow.toUInt()
            // heapBase should point to the start of the heap (right after actual rwData content)
            // Use actualRwDataLen if set, otherwise fall back to originalRwDataSize for PolkaVM format
            val effectiveRwDataLen = if (actualRwDataLen > 0u) actualRwDataLen else originalRwDataSize
            val heapBase = addressLow + effectiveRwDataLen.toULong()
            addressLow += rwDataAddressSpace
            val heapSlack = addressLow - heapBase
            addressLow += VM_MAX_PAGE_SIZE.toULong()

            var addressHigh: Long = VM_ADDRESS_SPACE_TOP.toLong()
            addressHigh -= auxDataAddressSpace.toLong()
            val auxDataAddress = addressHigh.toUInt()
            addressHigh -= VM_MAX_PAGE_SIZE.toLong()
            val stackAddressHigh = addressHigh.toUInt()
            addressHigh -= stackAddressSpace.toLong()

            if (addressLow.toLong() > addressHigh) {
                throw IllegalStateException("maximum memory size exceeded")
            }

            val maxHeapSize = (addressHigh.toULong() - addressLow + heapSlack).toUInt()

            MemoryMap(
                pageSize = pageSize,
                roDataSize = alignedRoDataSize,
                rwDataAddress = rwDataAddress,
                rwDataSize = alignedRwDataSize,
                stackAddressHigh = stackAddressHigh,
                stackSize = alignedStackSize,
                auxDataAddress = auxDataAddress,
                auxDataSize = alignedAuxDataSize,
                heapBase = heapBase.toUInt(),
                maxHeapSize = maxHeapSize
            )
        }
    }

    private fun UInt.isPowerOfTwo(): Boolean = this != 0U && (this and (this - 1U)) == 0U
}
