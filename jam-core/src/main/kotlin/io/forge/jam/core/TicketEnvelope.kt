package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class TicketEnvelope(
    val attempt: Long,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val signature: JamByteArray
) : Encodable {
    override fun encode(): ByteArray {
        val attemptBytes = encodeFixedWidthInteger(attempt, 1, false)
        val signatureBytes = signature.bytes
        return attemptBytes + signatureBytes
    }
}
