package io.forge.jam.safrole.stats

import io.forge.jam.core.Encodable
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
    override fun encode(): ByteArray {
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        val authorIndexBytes = encodeFixedWidthInteger(authorIndex, 2, false)
        return slotBytes + authorIndexBytes + extrinsic.encode()
    }
}
