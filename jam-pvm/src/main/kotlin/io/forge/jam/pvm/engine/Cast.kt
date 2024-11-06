package io.forge.jam.pvm.engine

/**
 * Explicit casting facility to replace Kotlin's built-in conversions.
 * This ensures compile-time safety for numeric casts.
 */
@JvmInline
value class Cast<T>(val value: T) {
    companion object {
        // Helper to create Cast instances
        @Suppress("NOTHING_TO_INLINE")
        inline operator fun <T> invoke(value: T): Cast<T> = Cast(value)
    }
}

/**
 * Byte (i8) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Byte>.toI32SignExtend(): Int = value.toInt()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Byte>.toI64SignExtend(): Long = value.toLong()

/**
 * Short (i16) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Short>.toI32SignExtend(): Int = value.toInt()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Short>.toI64SignExtend(): Long = value.toLong()

/**
 * Int (i32) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Int>.toUnsigned(): UInt = value.toUInt()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Int>.toI64SignExtend(): Long = value.toLong()

/**
 * Long (i64) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Long>.toUnsigned(): ULong = value.toULong()

/**
 * UByte (u8) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UByte>.toSigned(): Byte = value.toByte()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UByte>.toU64(): ULong = value.toULong()

/**
 * UShort (u16) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UShort>.toU64(): ULong = value.toULong()

/**
 * UInt (u32) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.assertAlwaysFitsInU32(): UInt {
    assert(value <= UInt.MAX_VALUE) { "Value doesn't fit in UInt" }
    return value.toUInt()
}

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.toSigned(): Int = value.toInt()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.toU64(): ULong = value.toULong()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.toUSize(): ULong = value.toULong()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.truncateToU8(): UByte = value.toUByte()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.truncateToU16(): UShort = value.toUShort()

/**
 * ULong (u64) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.assertAlwaysFitsInU32(): UInt {
    assert(value <= UInt.MAX_VALUE.toULong()) { "Value doesn't fit in UInt" }
    return value.toUInt()
}

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.toSigned(): Long = value.toLong()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.truncateToU8(): UByte = value.toUByte()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.truncateToU16(): UShort = value.toUShort()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.truncateToU32(): UInt = value.toUInt()

/**
 * ULong (usize) extensions - in Kotlin we use ULong for usize
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.assertAlwaysFitsInU32ForSize(): UInt {
    assert(value <= UInt.MAX_VALUE.toULong()) { "Size doesn't fit in UInt" }
    return value.toUInt()
}

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.toU64ForSize(): ULong = value
