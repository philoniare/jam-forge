package io.forge.jam.safrole.safrole

import io.forge.jam.core.Dispute
import io.forge.jam.core.TicketEnvelope
import io.forge.jam.core.serializers.ByteArrayHexSerializer
import io.forge.jam.core.serializers.ByteArrayListHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class SafroleInput(
    val slot: Long? = null,

    @Serializable(with = ByteArrayHexSerializer::class)
    val entropy: ByteArray = byteArrayOf(0),
    val extrinsic: List<TicketEnvelope> = emptyList(),

    @SerialName("post_offenders")
    @Serializable(with = ByteArrayListHexSerializer::class)
    val postOffenders: List<ByteArray>? = null,

    val disputes: Dispute? = null
)
