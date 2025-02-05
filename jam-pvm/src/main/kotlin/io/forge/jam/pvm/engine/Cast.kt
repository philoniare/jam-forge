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
inline fun Cast<Byte>.byteToI32SignExtend(): Int = value.toInt()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Byte>.byteToI64SignExtend(): Long = value.toLong()

/**
 * Short (i16) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Short>.shortToI32SignExtend(): Int = value.toInt()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Short>.shortToI64SignExtend(): Long = value.toLong()

/**
 * Int (i32) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Int>.intToUnsigned(): UInt = value.toUInt()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Int>.intToI64SignExtend(): Long = value.toLong()

/**
 * Long (i64) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<Long>.longToUnsigned(): ULong = value.toULong()

/**
 * UByte (u8) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UByte>.ubyteToSigned(): Byte = value.toByte()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UByte>.ubyteToU64(): ULong = value.toULong()

/**
 * UShort (u16) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UShort>.ushortToSigned(): Short = value.toShort()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UShort>.ushortToU64(): ULong = value.toULong()

/**
 * UInt (u32) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.uintAssertAlwaysFitsInU32(): UInt {
    assert(value <= UInt.MAX_VALUE) { "Value doesn't fit in UInt" }
    return value.toUInt()
}

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.uintToSigned(): Int = value.toInt()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.uintToU64(): ULong = value.toULong()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.uintToUSize(): ULong = value.toULong()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.uintTruncateToU8(): UByte = value.toUByte()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<UInt>.uintTruncateToU16(): UShort = value.toUShort()

/**
 * ULong (u64) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.ulongAssertAlwaysFitsInU32(): UInt {
    assert(value <= UInt.MAX_VALUE.toULong()) { "Value doesn't fit in UInt" }
    return value.toUInt()
}

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.ulongToSigned(): Long = value.toLong()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.ulongTruncateToU8(): UByte = value.toUByte()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.ulongTruncateToU16(): UShort = value.toUShort()

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.ulongTruncateToU32(): UInt = value.toUInt()

/**
 * ULong (usize) extensions
 */
@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.usizeAssertAlwaysFitsInU32(): UInt {
    assert(value <= UInt.MAX_VALUE.toULong()) { "Size doesn't fit in UInt" }
    return value.toUInt()
}

@Suppress("NOTHING_TO_INLINE")
inline fun Cast<ULong>.usizeToU64(): ULong = value
