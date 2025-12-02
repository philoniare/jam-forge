package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class PreimageExtrinsic(
    val requester: Long,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val blob: JamByteArray,
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<PreimageExtrinsic, Int> {
            var currentOffset = offset
            val requester = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4
            val (blobLength, blobLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += blobLengthBytes
            val blob = JamByteArray(data.copyOfRange(currentOffset, currentOffset + blobLength.toInt()))
            currentOffset += blobLength.toInt()
            return Pair(PreimageExtrinsic(requester, blob), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val requesterBytes = encodeFixedWidthInteger(requester, 4, false)
        val blobLengthBytes = encodeCompactInteger(blob.size.toLong())
        return requesterBytes + blobLengthBytes + blob.bytes
    }
}

