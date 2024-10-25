package io.forge.jam.core

import kotlinx.serialization.Serializable

@Serializable
data class TicketEnvelope(
    val attempt: Long,
    @Serializable(with = ByteArrayHexSerializer::class)
    val signature: ByteArray
) : Encodable {
    override fun encode(): ByteArray {
        val attemptBytes = encodeFixedWidthInteger(attempt, 1, false)
        val signatureBytes = signature
        return attemptBytes + signatureBytes
    }
}
