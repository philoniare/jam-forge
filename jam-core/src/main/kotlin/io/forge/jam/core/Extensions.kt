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
    return item?.encode() ?: byteArrayOf(0)
}

// Encode a list of Encodable items with length prefix
fun <T : Encodable> encodeList(list: List<T?>, includeLength: Boolean = true): ByteArray {
    val itemsBytes = list.fold(ByteArray(0)) { acc, item ->
        acc + encodeNullable(item)
    }
    if (includeLength) {
        val lengthBytes = encodeFixedWidthInteger(list.size, 1, false)
        return lengthBytes + itemsBytes
    } else {
        return itemsBytes
    }
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

fun encodeCompactInteger(value: Long): ByteArray {
    if (value < 0) {
        throw IllegalArgumentException("Value must be non-negative")
    }

    return when {
        value < (1L shl 6) -> {
            // Single-byte mode (mode 0, discriminator '00')
            val encoded = (value shl 2).toByte()
            byteArrayOf(encoded)
        }

        value < (1L shl 14) -> {
            // Two-byte mode (mode 1, discriminator '01')
            val v = (value shl 2) or 0x01L
            byteArrayOf(
                (v and 0xFF).toByte(),
                ((v shr 8) and 0xFF).toByte()
            )
        }

        value < (1L shl 30) -> {
            // Four-byte mode (mode 2, discriminator '10')
            val v = (value shl 2) or 0x02L
            byteArrayOf(
                (v and 0xFF).toByte(),
                ((v shr 8) and 0xFF).toByte(),
                ((v shr 16) and 0xFF).toByte(),
                ((v shr 24) and 0xFF).toByte()
            )
        }

        else -> {
            // Big-integer mode (mode 3, discriminator '11')
            val valueBytes = ByteBuffer.allocate(8)
                .order(ByteOrder.LITTLE_ENDIAN)
                .putLong(value)
                .array()
            val length = valueBytes.size // 8 bytes
            val lengthByte = (((length - 4) shl 2) or 0x03).toByte()
            val bytes = ByteArray(1 + length)
            bytes[0] = lengthByte
            System.arraycopy(valueBytes, 0, bytes, 1, length)
            bytes
        }
    }
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

fun List<Culprit>.validateCulprits(): OptionalResult<Unit, SafroleErrorCode> {
    // Check if empty
    if (isEmpty()) {
        return OptionalResult.Ok(Unit)
    }

    // Check that keys are strictly increasing (implies both sorted and unique)
    for (i in 0 until size - 1) {
        // Compare ByteArrays lexicographically
        val comparison = this[i].key.compareUnsigned(this[i + 1].key)
        if (comparison >= 0) { // If current >= next or equal (not strictly increasing)
            return OptionalResult.Err(SafroleErrorCode.CULPRITS_NOT_SORTED_UNIQUE)
        }
    }

    return OptionalResult.Ok(Unit)
}
