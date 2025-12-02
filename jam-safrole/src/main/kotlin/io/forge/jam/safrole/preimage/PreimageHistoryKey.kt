package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class PreimageHistoryKey(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val hash: JamByteArray,
    val length: Long
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<PreimageHistoryKey, Int> {
            var currentOffset = offset
            val hash = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32
            val length = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4
            return Pair(PreimageHistoryKey(hash, length), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val lengthBytes = encodeFixedWidthInteger(length, 4, false)
        return hash.bytes + lengthBytes
    }
}
