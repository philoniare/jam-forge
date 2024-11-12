package io.forge.jam.pvm.engine

import io.forge.jam.pvm.Abi
import io.forge.jam.pvm.program.ArcBytes
import io.forge.jam.pvm.program.ProgramCounter


/**
 * Represents a jump table in the VM architecture.
 */
class JumpTable(
    private val blob: ArcBytes,
    private val entrySize: UInt
) : Cloneable {

    fun isEmpty(): Boolean = len() == 0u

    fun len(): UInt = if (entrySize == 0u) {
        0u
    } else {
        (blob.toByteArray().size.toUInt()) / entrySize
    }

    fun getByAddress(address: UInt): ProgramCounter? {
        if ((address and (Abi.VM_CODE_ADDRESS_ALIGNMENT - 1u)) != 0u || address == 0u) {
            return null
        }

        return getByIndex(
            (address - Abi.VM_CODE_ADDRESS_ALIGNMENT) / Abi.VM_CODE_ADDRESS_ALIGNMENT
        )
    }

    fun getByIndex(index: UInt): ProgramCounter? {
        if (entrySize == 0u) {
            return null
        }

        // Safe multiplication and addition checks
        val start = index.times(entrySize).takeIf { it >= index } ?: return null
        val end = start.plus(entrySize).takeIf { it >= start } ?: return null

        // Convert to ByteArray for reading
        val bytes = blob.toByteArray()

        // Range check
        if (end.toInt() > bytes.size) {
            return null
        }

        val slice = bytes.slice(start.toInt() until end.toInt())

        val value = when (slice.size) {
            1 -> slice[0].toUByte().toUInt()
            2 -> slice.toUShort(0).toUInt()
            3 -> slice.toUInt24(0)
            4 -> slice.toUInt(0)
            else -> throw IllegalStateException("Invalid entry size")
        }

        return ProgramCounter(value)
    }

    fun iterator(): Iterator<ProgramCounter> = object : Iterator<ProgramCounter> {
        private var index: UInt = 0u

        override fun hasNext(): Boolean = index < len()

        override fun next(): ProgramCounter {
            if (!hasNext()) throw NoSuchElementException()
            return getByIndex(index++)
                ?: throw IllegalStateException("Failed to read entry at index $index")
        }
    }

    public override fun clone(): JumpTable = JumpTable(blob.clone(), entrySize)
}

// Extension functions for byte array conversions
private fun List<Byte>.toUShort(offset: Int): UShort =
    ((this[offset].toUByte().toUInt() or
        (this[offset + 1].toUByte().toUInt() shl 8))).toUShort()

private fun List<Byte>.toUInt24(offset: Int): UInt =
    (this[offset].toUByte().toUInt() or
        (this[offset + 1].toUByte().toUInt() shl 8) or
        (this[offset + 2].toUByte().toUInt() shl 16))

private fun List<Byte>.toUInt(offset: Int): UInt =
    (this[offset].toUByte().toUInt() or
        (this[offset + 1].toUByte().toUInt() shl 8) or
        (this[offset + 2].toUByte().toUInt() shl 16) or
        (this[offset + 3].toUByte().toUInt() shl 24))
