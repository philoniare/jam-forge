package io.forge.jam.pvm.program

/**
 * Represents a raw register value with safety checks and formatting capabilities.
 * Wraps a 32-bit unsigned integer and provides safe conversion to/from [Reg].
 *
 * @property value The underlying raw 32-bit value
 */
@JvmInline
value class RawReg(private val value: UInt) {
    /**
     * Retrieves the parsed register value with safety bounds checking.
     * Values greater than 12 are clamped to 12.
     *
     * @return The parsed [Reg] value
     * @throws IllegalStateException if the value cannot be converted to a valid [Reg]
     */
    fun get(): Reg {
        var parsed = (value and 0xFu).toInt()
        if (parsed > 12) {
            parsed = 12
        }
        return Reg.fromRaw(parsed) ?: throw IllegalStateException("Internal error: entered unreachable code")
    }

    /**
     * Returns the raw unparsed value.
     */
    fun rawUnparsed(): UInt = value

    override fun toString(): String = get().toString()

    companion object {
        /**
         * Creates a [RawReg] from a [Reg] value.
         */
        fun from(reg: Reg): RawReg = RawReg(reg.ordinal.toUInt())
    }
}

/**
 * Extension function to convert [Reg] to [RawReg].
 */
fun Reg.toRawReg(): RawReg = RawReg.from(this)

fun RawReg.toU32(): UInt = get().toIndex().toUInt()

/**
 * Extension function to convert [RawReg] to [Reg].
 */
fun RawReg.toReg(): Reg = get()

/**
 * Custom debug string formatter for [RawReg].
 */
fun RawReg.toDebugString(): String = "${get()} (0x${rawUnparsed().toString(16)})"
