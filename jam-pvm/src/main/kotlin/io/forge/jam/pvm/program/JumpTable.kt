package io.forge.jam.pvm.program

import io.forge.jam.pvm.Abi

/**
 * Represents a table of jump addresses with fixed-size entries.
 *
 * @property blob The underlying byte array containing jump addresses
 * @property entrySize Size of each jump table entry in bytes
 */
@JvmInline
value class JumpTable(
    private val state: Pair<ByteArray, UInt>
) : Iterable<ProgramCounter> {
    private val blob: ByteArray get() = state.first
    private val entrySize: UInt get() = state.second

    companion object {
        /**
         * Creates a new JumpTable instance
         */
        fun create(blob: ByteArray, entrySize: UInt): JumpTable =
            JumpTable(Pair(blob, entrySize))
    }

    /**
     * Checks if the jump table is empty
     */
    fun isEmpty(): Boolean = len() == 0u

    /**
     * Returns the number of entries in the jump table
     */
    fun len(): UInt = if (entrySize == 0u) {
        0u
    } else {
        (blob.size.toUInt() / entrySize)
    }

    /**
     * Gets a program counter by memory address
     */
    fun getByAddress(address: UInt): ProgramCounter? {
        if (address and (Abi.VM_CODE_ADDRESS_ALIGNMENT - 1u) != 0u || address == 0u) {
            return null
        }
        return getByIndex(
            (address - Abi.VM_CODE_ADDRESS_ALIGNMENT) / Abi.VM_CODE_ADDRESS_ALIGNMENT
        )
    }

    /**
     * Gets a program counter by index
     */
    fun getByIndex(index: UInt): ProgramCounter? {
        if (entrySize == 0u) {
            return null
        }

        val start = index.toULong() * entrySize.toULong()
        if (start > Int.MAX_VALUE.toULong()) return null

        val end = start + entrySize.toULong()
        if (end > Int.MAX_VALUE.toULong()) return null

        val startInt = start.toInt()
        val endInt = end.toInt()

        if (startInt >= blob.size || endInt > blob.size) {
            return null
        }

        val slice = blob.slice(startInt until endInt)

        val value = when (slice.size) {
            1 -> slice[0].toUByte().toUInt()
            2 -> {
                val bytes = slice.toByteArray()
                ((bytes[0].toUByte().toUInt()) or
                    (bytes[1].toUByte().toUInt() shl 8))
            }

            3 -> {
                val bytes = slice.toByteArray()
                ((bytes[0].toUByte().toUInt()) or
                    (bytes[1].toUByte().toUInt() shl 8) or
                    (bytes[2].toUByte().toUInt() shl 16))
            }

            4 -> {
                val bytes = slice.toByteArray()
                ((bytes[0].toUByte().toUInt()) or
                    (bytes[1].toUByte().toUInt() shl 8) or
                    (bytes[2].toUByte().toUInt() shl 16) or
                    (bytes[3].toUByte().toUInt() shl 24))
            }

            else -> throw IllegalStateException("Invalid entry size")
        }

        return ProgramCounter(value)
    }

    /**
     * Returns an iterator over the jump table entries
     */
    override fun iterator(): Iterator<ProgramCounter> = object : Iterator<ProgramCounter> {
        private var index: UInt = 0u

        override fun hasNext(): Boolean =
            index < len()

        override fun next(): ProgramCounter {
            val value = getByIndex(index) ?: throw NoSuchElementException("No more elements")
            index++
            return value
        }
    }
}
