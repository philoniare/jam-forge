package io.forge.jam.pvm.engine

import io.forge.jam.pvm.Abi

/**
 * Represents the initialization parameters for a guest VM instance.
 * All sizes are represented as unsigned 32-bit integers.
 */
data class GuestInit(
    val pageSize: UInt,
    val roData: ByteArray,
    val rwData: ByteArray,
    val roDataSize: UInt,
    val rwDataSize: UInt,
    val actualRwDataLen: UInt,  // The actual rwData content length (without heap pages)
    val stackSize: UInt,
    val auxDataSize: UInt,
    val heapPages: UInt = 0u  // Number of heap empty pages from program blob
) {
    /**
     * Creates a memory map based on the initialization parameters.
     * @return Result containing the MemoryMap if successful, or an error message if failed
     */
    fun memoryMap(): Result<Abi.MemoryMap> = runCatching {
        Abi.MemoryMapBuilder.new(pageSize)
            .roDataSize(roDataSize)
            .rwDataSize(rwDataSize)
            .actualRwDataLen(actualRwDataLen)
            .stackSize(stackSize)
            .auxDataSize(auxDataSize)
            .build()
            .getOrThrow()
    }

    companion object {
        fun default() = GuestInit(
            pageSize = 0u,
            roData = ByteArray(0),
            rwData = ByteArray(0),
            roDataSize = 0u,
            rwDataSize = 0u,
            actualRwDataLen = 0u,
            stackSize = 0u,
            auxDataSize = 0u,
            heapPages = 0u
        )
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is GuestInit) return false

        return pageSize == other.pageSize &&
            roData.contentEquals(other.roData) &&
            rwData.contentEquals(other.rwData) &&
            roDataSize == other.roDataSize &&
            rwDataSize == other.rwDataSize &&
            actualRwDataLen == other.actualRwDataLen &&
            stackSize == other.stackSize &&
            auxDataSize == other.auxDataSize &&
            heapPages == other.heapPages
    }

    override fun hashCode(): Int {
        var result = pageSize.hashCode()
        result = 31 * result + roData.contentHashCode()
        result = 31 * result + rwData.contentHashCode()
        result = 31 * result + roDataSize.hashCode()
        result = 31 * result + rwDataSize.hashCode()
        result = 31 * result + actualRwDataLen.hashCode()
        result = 31 * result + stackSize.hashCode()
        result = 31 * result + auxDataSize.hashCode()
        result = 31 * result + heapPages.hashCode()
        return result
    }

    override fun toString(): String = buildString {
        append("GuestInit(")
        append("pageSize=").append(pageSize).append(", ")
        append("roData.size=").append(roData.size).append(", ")
        append("rwData.size=").append(rwData.size).append(", ")
        append("roDataSize=").append(roDataSize).append(", ")
        append("rwDataSize=").append(rwDataSize).append(", ")
        append("actualRwDataLen=").append(actualRwDataLen).append(", ")
        append("stackSize=").append(stackSize).append(", ")
        append("auxDataSize=").append(auxDataSize).append(", ")
        append("heapPages=").append(heapPages)
        append(")")
    }
}
