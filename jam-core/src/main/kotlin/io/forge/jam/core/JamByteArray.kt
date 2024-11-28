package io.forge.jam.core

import kotlinx.serialization.Serializable

@Serializable
data class JamByteArray(val bytes: ByteArray) : Encodable {

    override fun toString(): String {
        return "JamByteArray(size=${bytes.size}, hex=${toHex()})"
    }

    override fun encode(): ByteArray = bytes

    fun clone(): JamByteArray = JamByteArray(bytes.clone())

    fun contentEquals(other: ByteArray): Boolean = bytes.contentEquals(other)
    fun contentEquals(other: JamByteArray): Boolean = bytes.contentEquals(other.bytes)

    operator fun plus(other: ByteArray): JamByteArray =
        JamByteArray(bytes + other)

    operator fun plus(other: JamByteArray): JamByteArray =
        JamByteArray(bytes + other.bytes)

    val size: Int get() = bytes.size
    val indices: IntRange get() = bytes.indices
    val lastIndex: Int get() = bytes.lastIndex

    operator fun get(index: Int): Byte = bytes[index]
    operator fun iterator(): ByteIterator = bytes.iterator()

    fun copyInto(
        destination: ByteArray,
        destinationOffset: Int = 0,
        startIndex: Int = 0,
        endIndex: Int = size
    ): ByteArray =
        bytes.copyInto(destination, destinationOffset, startIndex, endIndex)

    fun copyOfRange(fromIndex: Int, toIndex: Int): ByteArray = bytes.copyOfRange(fromIndex, toIndex)
    fun fill(element: Byte, fromIndex: Int = 0, toIndex: Int = size): Unit = bytes.fill(element, fromIndex, toIndex)

    override fun equals(other: Any?): Boolean = when {
        this === other -> true
        other !is JamByteArray -> false
        else -> bytes.contentEquals(other.bytes)
    }

    override fun hashCode(): Int = bytes.contentHashCode()
    fun contentHashCode(): Int = bytes.contentHashCode()

    fun toHex(): String = bytes.joinToString("") { "%02x".format(it) }

    fun compareUnsigned(other: ByteArray): Int {
        val minLength = minOf(bytes.size, other.size)

        for (i in 0 until minLength) {
            val b1 = bytes[i].toInt() and 0xFF
            val b2 = other[i].toInt() and 0xFF

            if (b1 != b2) {
                return b1 - b2
            }
        }

        return bytes.size - other.size
    }

    fun compareUnsigned(other: JamByteArray): Int {
        return compareUnsigned(other.bytes)
    }

    fun compareTo(other: JamByteArray): Int {
        val minLength = minOf(size, other.size)
        for (i in 0 until minLength) {
            val diff = (bytes[i].toInt() and 0xFF) - (other[i].toInt() and 0xFF)
            if (diff != 0) return diff
        }
        return size - other.size
    }

    fun startsWith(prefix: ByteArray): Boolean {
        if (this.size < prefix.size) return false
        return prefix.withIndex().all { (i, byte) -> bytes[i] == byte }
    }

    fun contentToString(): String = bytes.contentToString()

    fun isEmpty(): Boolean = bytes.isEmpty()

    companion object {
        // Extension function to allow ByteArray + JamByteArray
        operator fun ByteArray.plus(other: JamByteArray): JamByteArray =
            JamByteArray(this + other.bytes)
    }
}
