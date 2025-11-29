package io.forge.jam.core

import java.nio.ByteBuffer
import java.nio.ByteOrder

// Convert ByteArray to hex string
fun ByteArray.toHex(): String = joinToString("") { "%02x".format(it) }

// Convert hex string to JamByteArray
fun String.hexToJamBytes(): JamByteArray {
    val hex = this.removePrefix("0x")
    val len = hex.length
    require(len % 2 == 0) { "Hex string must have even length" }
    return JamByteArray(ByteArray(len / 2) { i ->
        hex.substring(2 * i, 2 * i + 2).toInt(16).toByte()
    })
}

// Convert Integer to Little Endian Bytes
fun Int.toLEBytes(size: Int = 4): ByteArray {
    val bytes = ByteArray(size)
    for (i in 0 until size) {
        bytes[i] = ((this shr (8 * i)) and 0xFF).toByte()
    }
    return bytes
}

// Convert Boolean to Byte
fun Boolean.toByte(): Byte = if (this) 1.toByte() else 0.toByte()

fun <T : Encodable> encodeNullable(item: T?): ByteArray {
    return if (item == null) {
        byteArrayOf(0)
    } else {
        byteArrayOf(1) + item.encode()
    }
}

/**
 * Encode a list of non-optional Encodable items.
 * When includeLength is true, uses 1-byte fixed length prefix (for small fixed-max lists).
 * For variable-size lists that may exceed 255 elements, use encodeListWithCompactLength instead.
 */
fun <T : Encodable> encodeList(list: List<T>, includeLength: Boolean = true): ByteArray {
    val itemsBytes = list.fold(ByteArray(0)) { acc, item ->
        acc + item.encode()
    }
    if (includeLength) {
        val lengthBytes = encodeFixedWidthInteger(list.size, 1, false)
        return lengthBytes + itemsBytes
    } else {
        return itemsBytes
    }
}

/**
 * Encode a list of non-optional Encodable items with compact integer length prefix.
 * Use this for variable-size SEQUENCE OF types.
 */
fun <T : Encodable> encodeListWithCompactLength(list: List<T>): ByteArray {
    val itemsBytes = list.fold(ByteArray(0)) { acc, item ->
        acc + item.encode()
    }
    val lengthBytes = encodeCompactInteger(list.size.toLong())
    return lengthBytes + itemsBytes
}

/**
 * Encodes nested lists according to JAM protocol specification.
 * The outer list length is included if includeLength is true (1-byte fixed).
 * Inner lists use compact integer length prefix.
 */
fun <T : Encodable> encodeNestedList(list: List<List<T>>, includeLength: Boolean = true): ByteArray {
    val outerLengthBytes = if (includeLength) {
        encodeFixedWidthInteger(list.size, 1, false)
    } else {
        ByteArray(0)
    }

    val innerListsBytes = list.fold(ByteArray(0)) { acc, innerList ->
        acc + encodeList(innerList, true)
    }

    return outerLengthBytes + innerListsBytes
}

/**
 * Encode a list of optional Encodable items
 */
fun <T : Encodable> encodeOptionalList(list: List<T?>, includeLength: Boolean = true): ByteArray {
    val itemsBytes = list.fold(ByteArray(0)) { acc, item ->
        acc + if (item == null) {
            byteArrayOf(0)
        } else {
            byteArrayOf(1) + item.encode()
        }
    }

    return if (includeLength) {
        encodeFixedWidthInteger(list.size, 1, false) + itemsBytes
    } else {
        itemsBytes
    }
}

/**
 * Helper to convert Long to little-endian byte array of specified size
 */
fun Long.toByteArray(size: Int): ByteArray {
    val result = ByteArray(size)
    var value = this
    for (i in 0 until size) {
        result[i] = (value and 0xFF).toByte()
        value = value shr 8
    }
    return result
}

fun encodeFixedWidthInteger(value: Number, byteSize: Int = 4, hasDiscriminator: Boolean = true): ByteArray {
    val buffer = ByteBuffer.allocate(byteSize)
        .order(ByteOrder.LITTLE_ENDIAN)

    when (byteSize) {
        1 -> buffer.put(value.toByte())
        2 -> buffer.putShort(value.toShort())
        4 -> buffer.putInt(value.toInt())
        8 -> buffer.putLong(value.toLong())
        else -> throw IllegalArgumentException("Unsupported byte size")
    }

    if (hasDiscriminator) {
        val discriminator = byteArrayOf(16.toByte())
        return buffer.array() + discriminator
    }
    return buffer.array()
}

/**
 * Encodes a nonnegative Long (up to < 2^64) according to
 * Section C.6 of the Gray Paper ("General Natural Number Serialization").
 *
 * Returns the full spec-compliant encoding: prefix byte + E_l(remainder).
 * - If x = 0, returns [0].
 * - If 2^(7l) <= x < 2^(7(l+1)) for some l in [0..8], then
 *     [ 256 - 2^(8-l) + floor(x / 2^(8*l)) ] ++ E_l(x mod 2^(8*l))
 * - Otherwise (x < 2^64), returns [255] ++ E_8(x).
 *
 * Here E_l( r ) means "r in little-endian form over l bytes."
 */
fun encodeCompactInteger(x: Long): ByteArray {
    require(x >= 0) { "No negative values allowed." }
    // 1) Special case x = 0
    if (x == 0L) {
        return byteArrayOf(0)
    }

    // 2) If x < 2^(64), figure out which l fits 2^(7l) <= x < 2^(7(l+1))
    //    for l in [0..8].
    //    (We only go up to l=8 because 2^(7*8) = 2^56 < 2^64.)
    for (l in 0..8) {
        val lowerBound = 1L shl (7 * l)          // 2^(7l)
        val upperBound = 1L shl (7 * (l + 1))    // 2^(7(l+1))
        if (x in lowerBound until upperBound) {
            // prefix = 256 - 2^(8-l) + floor(x / 2^(8*l))
            val prefixVal = (256 - (1 shl (8 - l))) + (x ushr (8 * l))
            val prefixByte = prefixVal.toByte()

            // remainder = x mod 2^(8*l)
            val remainder = x and ((1L shl (8 * l)) - 1)
            // E_l(remainder) -> little-endian representation in l bytes
            val remainderBytes = ByteArray(l)
            for (i in 0 until l) {
                remainderBytes[i] = ((remainder shr (8 * i)) and 0xFF).toByte()
            }
            return byteArrayOf(prefixByte) + remainderBytes
        }
    }

    // 3) Otherwise (still x < 2^64 from the require() above):
    //    [255] ++ E_8(x)
    val prefix = 0xFF.toByte()
    val fullBytes = ByteArray(8)
    for (i in 0 until 8) {
        fullBytes[i] = ((x shr (8 * i)) and 0xFF).toByte()
    }
    return byteArrayOf(prefix) + fullBytes
}


fun encodeOptionalByteArray(value: ByteArray?): ByteArray {
    return if (value == null) {
        // Existence flag for 'absent' value
        byteArrayOf(0)
    } else {
        // Existence flag for 'present' value
        val existenceFlag = byteArrayOf(1.toByte())
        // Combine existence flag and data
        existenceFlag + value
    }
}
