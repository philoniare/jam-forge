package io.forge.jam.pvm.engine

class InterpretedModule private constructor(
    val roData: ByteArray,
    val rwData: ByteArray,
    val heapEmptyPages: UInt  // Number of heap empty pages from program blob
) {
    companion object {
        fun new(init: GuestInit): Result<InterpretedModule> = runCatching {
            // Get memory map or throw if failed
            val memoryMap = init.memoryMap().getOrThrow()

            // Create and resize ro_data to match memory map size
            val roData = init.roData.copyOf(memoryMap.roDataSize.toInt())

            InterpretedModule(
                roData = roData,
                rwData = init.rwData.copyOf(),
                heapEmptyPages = init.heapPages
            )
        }
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is InterpretedModule) return false

        return roData.contentEquals(other.roData) &&
            rwData.contentEquals(other.rwData) &&
            heapEmptyPages == other.heapEmptyPages
    }

    override fun hashCode(): Int {
        var result = roData.contentHashCode()
        result = 31 * result + rwData.contentHashCode()
        result = 31 * result + heapEmptyPages.hashCode()
        return result
    }

    override fun toString(): String = buildString {
        append("InterpretedModule(")
        append("roData.size=${roData.size}, ")
        append("rwData.size=${rwData.size}, ")
        append("heapEmptyPages=$heapEmptyPages")
        append(")")
    }
}
