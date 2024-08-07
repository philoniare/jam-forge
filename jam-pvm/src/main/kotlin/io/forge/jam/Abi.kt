package io.forge.jam

const val ADDRESS_SPACE_SIZE: ULong = 0x100000000UL

// The minimum page size of the VM.
const val VM_MIN_PAGE_SIZE: UInt = 0x1000U

// The maximum page size of the VM.
const val VM_MAX_PAGE_SIZE: UInt = 0x10000U

// The address at which the program's stack starts inside the VM.
val VM_ADDR_USER_STACK_HIGH: UInt = (ADDRESS_SPACE_SIZE - VM_MAX_PAGE_SIZE.toULong()).toUInt()

// The address which, when jumped to, will return to the host.
const val VM_ADDR_RETURN_TO_HOST: UInt = 0xffff0000U

// The maximum byte size of the code blob.
val VM_MAXIMUM_CODE_SIZE: UInt = 32U * 1024U * 1024U

// The maximum number of entries in the jump table.
val VM_MAXIMUM_JUMP_TABLE_ENTRIES: UInt = 16U * 1024U * 1024U

// The maximum number of functions the program can import.
const val VM_MAXIMUM_IMPORT_COUNT: UInt = 1024U

// The minimum required alignment of runtime code pointers.
const val VM_CODE_ADDRESS_ALIGNMENT: UInt = 2U

// Helper functions
fun alignToNextPageU32(alignment: UInt, value: UInt): UInt {
    val mask = alignment - 1U
    return (value + mask) and mask.inv()
}

fun alignToNextPageU64(alignment: ULong, value: ULong): ULong {
    val mask = alignment - 1UL
    return (value + mask) and mask.inv()
}

fun isPowerOfTwo(n: UInt): Boolean {
    return n > 0u && (n and (n - 1u)) == 0u
}

// The memory map of a given guest program.
data class MemoryMap(
    val pageSize: UInt,
    val roDataSize: UInt,
    val rwDataSize: UInt,
    val stackSize: UInt,
    val heapBase: UInt,
    val maxHeapSize: UInt
) {
    companion object {
        // Creates an empty memory map.
        fun empty() = MemoryMap(0U, 0U, 0U, 0U, 0U, 0U)

        // Calculates the memory map from the given parameters.
        fun new(pageSize: UInt, roDataSize: UInt, rwDataSize: UInt, stackSize: UInt): Result<MemoryMap> {
            if (pageSize < VM_MIN_PAGE_SIZE) {
                return Result.failure(IllegalArgumentException("invalid page size: page size is too small"))
            }

            if (pageSize > VM_MAX_PAGE_SIZE) {
                return Result.failure(IllegalArgumentException("invalid page size: page size is too big"))
            }

            if (!isPowerOfTwo(pageSize)) {
                return Result.failure(IllegalArgumentException("invalid page size: page size is not a power of two"))
            }

            val roDataAddressSpace = alignToNextPageU64(VM_MAX_PAGE_SIZE.toULong(), roDataSize.toULong())

            val alignedRoDataSize = alignToNextPageU32(pageSize, roDataSize)

            val rwDataAddressSpace = alignToNextPageU64(VM_MAX_PAGE_SIZE.toULong(), rwDataSize.toULong())

            val originalRwDataSize = rwDataSize
            val alignedRwDataSize = alignToNextPageU32(pageSize, rwDataSize)

            val stackAddressSpace = alignToNextPageU64(VM_MAX_PAGE_SIZE.toULong(), stackSize.toULong())

            val alignedStackSize = alignToNextPageU32(pageSize, stackSize)

            var addressLow: ULong = 0UL

            addressLow += VM_MAX_PAGE_SIZE.toULong()
            addressLow += roDataAddressSpace
            addressLow += VM_MAX_PAGE_SIZE.toULong()

            val heapBase = addressLow + originalRwDataSize.toULong()
            addressLow += rwDataAddressSpace
            val heapSlack = addressLow - heapBase
            addressLow += VM_MAX_PAGE_SIZE.toULong()

            var addressHigh: ULong = VM_ADDR_USER_STACK_HIGH.toULong()
            addressHigh -= stackAddressSpace

            if (addressLow > addressHigh) {
                return Result.failure(IllegalArgumentException("maximum memory size exceeded"))
            }

            val maxHeapSize = addressHigh - addressLow + heapSlack

            return Result.success(
                MemoryMap(
                    pageSize,
                    alignedRoDataSize,
                    alignedRwDataSize,
                    alignedStackSize,
                    heapBase.toUInt(),
                    maxHeapSize.toUInt()
                )
            )
        }
    }

    // The address at where the program's read-only data starts inside the VM.
    fun roDataAddress(): UInt = VM_MAX_PAGE_SIZE

    // The range of addresses where the program's read-only data is inside the VM.
    fun roDataRange(): UIntRange = roDataAddress() until (roDataAddress() + roDataSize)

    // The address at where the program's read-write data starts inside the VM.
    fun rwDataAddress(): UInt {
        val offset = alignToNextPageU32(VM_MAX_PAGE_SIZE, roDataAddress() + roDataSize)
        return offset + VM_MAX_PAGE_SIZE
    }

    // The range of addresses where the program's read-write data is inside the VM.
    fun rwDataRange(): UIntRange = rwDataAddress() until (rwDataAddress() + rwDataSize)

    // The address at where the program's stack starts inside the VM.
    fun stackAddressLow(): UInt = stackAddressHigh() - stackSize

    // The address at where the program's stack ends inside the VM.
    fun stackAddressHigh(): UInt = VM_ADDR_USER_STACK_HIGH

    // The range of addresses where the program's stack is inside the VM.
    fun stackRange(): UIntRange = stackAddressLow() until stackAddressHigh()
}
