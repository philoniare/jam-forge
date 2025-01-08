package io.forge.jam.safrole.safrole

import io.forge.jam.core.*
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class SafroleInput(
    val slot: Long? = null,

    @Serializable(with = JamByteArrayHexSerializer::class)
    val entropy: JamByteArray = JamByteArray(byteArrayOf(0)),
    val extrinsic: List<TicketEnvelope> = emptyList(),

    val disputes: Dispute? = null
) : Encodable {
    override fun encode(): ByteArray {
        val slotBytes = slot?.let { encodeFixedWidthInteger(it, 4, true) } ?: byteArrayOf(0)
        val entropyBytes = entropy.bytes
        val extrinsicBytes = encodeList(extrinsic)
        val disputesBytes = disputes?.encode() ?: byteArrayOf(0)
        return slotBytes + entropyBytes + extrinsicBytes + disputesBytes
    }
}
