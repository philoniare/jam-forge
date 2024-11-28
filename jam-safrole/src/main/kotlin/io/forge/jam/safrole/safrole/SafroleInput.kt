package io.forge.jam.safrole.safrole

import io.forge.jam.core.*
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class SafroleInput(
    val slot: Long? = null,

    @Serializable(with = JamByteArrayHexSerializer::class)
    val entropy: JamByteArray = JamByteArray(byteArrayOf(0)),
    val extrinsic: List<TicketEnvelope> = emptyList(),

    @SerialName("post_offenders")
    @Serializable(with = JamByteArrayListHexSerializer::class)
    val postOffenders: List<JamByteArray>? = null,

    val disputes: Dispute? = null
) : Encodable {
    override fun encode(): ByteArray {
        val slotBytes = slot?.let { encodeFixedWidthInteger(it, 4, true) } ?: byteArrayOf(0)
        val entropyBytes = entropy.bytes
        val extrinsicBytes = encodeList(extrinsic)
        val postOffendersBytes = postOffenders?.let { encodeList(it) } ?: byteArrayOf(0)
        val disputesBytes = disputes?.encode() ?: byteArrayOf(0)
        return slotBytes + entropyBytes + extrinsicBytes + postOffendersBytes + disputesBytes
    }
}
