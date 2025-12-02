package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.encodeList
import kotlinx.serialization.Serializable

@Serializable
data class PreimageInput(
    val preimages: List<PreimageExtrinsic>,
    val slot: Long,
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<PreimageInput, Int> {
            var currentOffset = offset
            // preimages - variable size list
            val (preimagesLength, preimagesLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += preimagesLengthBytes
            val preimages = mutableListOf<PreimageExtrinsic>()
            for (i in 0 until preimagesLength.toInt()) {
                val (preimage, preimageBytes) = PreimageExtrinsic.fromBytes(data, currentOffset)
                preimages.add(preimage)
                currentOffset += preimageBytes
            }
            val slot = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4
            return Pair(PreimageInput(preimages, slot), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val guaranteesBytes = encodeList(preimages)
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        return guaranteesBytes + slotBytes
    }
}
