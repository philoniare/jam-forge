package io.forge.jam.pvm.engine

sealed class MemoryAccessError : Exception() {
    data class OutOfRangeAccess(
        val address: UInt,
        val length: ULong
    ) : MemoryAccessError() {
        override val message: String
            get() = buildString {
                append("out of range memory access in 0x")
                append(address.toString(16))
                append("-0x")
                append((address.toULong() + length.toULong()).toString(16))
                append(" (")
                append(length)
                append(" bytes)")
            }
    }

    data class Error(val error: PvmError) : MemoryAccessError() {
        override val message: String = "memory access failed: $error"
    }

    companion object {
        fun outOfRangeAccess(address: UInt, length: ULong): MemoryAccessError =
            OutOfRangeAccess(address, length)

        fun error(error: PvmError): MemoryAccessError =
            Error(error)
    }

}
