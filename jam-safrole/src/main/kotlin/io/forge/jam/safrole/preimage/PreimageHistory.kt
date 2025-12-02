package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.core.encodeFixedWidthInteger
import kotlinx.serialization.Serializable

@Serializable
data class PreimageHistory(val key: PreimageHistoryKey, val value: List<Long>) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<PreimageHistory, Int> {
            var currentOffset = offset
            val (key, keyBytes) = PreimageHistoryKey.fromBytes(data, currentOffset)
            currentOffset += keyBytes
            val length = decodeFixedWidthInteger(data, currentOffset, 1, false).toInt()
            currentOffset += 1
            val value = mutableListOf<Long>()
            for (i in 0 until length) {
                value.add(decodeFixedWidthInteger(data, currentOffset, 4, false))
                currentOffset += 4
            }
            return Pair(PreimageHistory(key, value), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val lengthBytes = encodeFixedWidthInteger(value.size, 1, false)
        val valueBytes = value.fold(byteArrayOf()) { acc, v ->
            acc + encodeFixedWidthInteger(v, 4, false)
        }
        return key.encode() + lengthBytes + valueBytes
    }
}
