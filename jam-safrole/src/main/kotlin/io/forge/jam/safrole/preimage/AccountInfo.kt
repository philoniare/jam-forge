package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.encodeList
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class AccountInfo(
    @SerialName("preimage_blobs")
    var preimages: List<PreimageHash>,
    @SerialName("preimage_requests")
    var lookupMeta: List<PreimageHistory>,
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<AccountInfo, Int> {
            var currentOffset = offset
            // preimages - variable size list
            val (preimagesLength, preimagesLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += preimagesLengthBytes
            val preimages = mutableListOf<PreimageHash>()
            for (i in 0 until preimagesLength.toInt()) {
                val (preimageHash, preimageHashBytes) = PreimageHash.fromBytes(data, currentOffset)
                preimages.add(preimageHash)
                currentOffset += preimageHashBytes
            }
            // lookupMeta - variable size list
            val (lookupMetaLength, lookupMetaLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += lookupMetaLengthBytes
            val lookupMeta = mutableListOf<PreimageHistory>()
            for (i in 0 until lookupMetaLength.toInt()) {
                val (history, historyBytes) = PreimageHistory.fromBytes(data, currentOffset)
                lookupMeta.add(history)
                currentOffset += historyBytes
            }
            return Pair(AccountInfo(preimages, lookupMeta), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        return encodeList(preimages) + encodeList(lookupMeta)
    }
}
