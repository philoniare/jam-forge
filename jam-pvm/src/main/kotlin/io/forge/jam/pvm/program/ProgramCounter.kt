package io.forge.jam.pvm.program

/**
 * Represents a program counter in the virtual machine.
 * This is a transparent wrapper around an unsigned 32-bit integer.
 *
 * @property value The underlying program counter value
 */
@JvmInline
value class ProgramCounter(val value: UInt) : Comparable<ProgramCounter> {
    /**
     * Returns string representation of the program counter value.
     */
    override fun toString(): String = value.toString()

    /**
     * Compares this program counter with another for ordering.
     */
    override fun compareTo(other: ProgramCounter): Int =
        value.compareTo(other.value)

    /**
     * Returns a string representation for debugging.
     */
    fun toDebugString(): String = "ProgramCounter($value)"

    companion object {
        /**
         * Creates a ProgramCounter from a raw unsigned 32-bit value.
         */
        fun fromRaw(value: UInt): ProgramCounter = ProgramCounter(value)
    }
}

// Extension function to provide a consistent hash code
fun ProgramCounter.hashValue(): Int = value.hashCode()

/**
 * Extension function to format the program counter for display.
 * Matches Rust's Debug trait implementation.
 */
fun ProgramCounter.formatDebug(): String = toDebugString()
