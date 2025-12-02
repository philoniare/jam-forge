package io.forge.jam.safrole.stats

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.core.encodeFixedWidthInteger
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class StatInput(
    val slot: Long,
    @SerialName("author_index")
    val authorIndex: Long,
    val extrinsic: StatExtrinsic
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, coresCount: Int, validatorsCount: Int): Pair<StatInput, Int> {
            var currentOffset = offset

            val slot = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4

            val authorIndex = decodeFixedWidthInteger(data, currentOffset, 2, false)
            currentOffset += 2

            val (extrinsic, extrinsicBytes) = StatExtrinsic.fromBytes(data, currentOffset, coresCount, validatorsCount)
            currentOffset += extrinsicBytes

            return Pair(StatInput(slot, authorIndex, extrinsic), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        val authorIndexBytes = encodeFixedWidthInteger(authorIndex, 2, false)
        return slotBytes + authorIndexBytes + extrinsic.encode()
    }
}
