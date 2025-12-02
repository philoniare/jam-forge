package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class PreimageHash(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val hash: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val blob: JamByteArray
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<PreimageHash, Int> {
            var currentOffset = offset

            // hash - 32 bytes
            val hash = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32

            // blob - compact length + bytes
            val (blobLength, blobLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += blobLengthBytes
            val blob = JamByteArray(data.copyOfRange(currentOffset, currentOffset + blobLength.toInt()))
            currentOffset += blobLength.toInt()

            return Pair(PreimageHash(hash, blob), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val lengthBytes = encodeCompactInteger(blob.bytes.size.toLong())
        return hash.bytes + lengthBytes + blob.bytes
    }
}
