package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class TicketMark(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val id: JamByteArray,
    val attempt: Long,
) : Encodable {
    override fun encode(): ByteArray {
        val attemptBytes = encodeFixedWidthInteger(attempt, 1, false)
        return id.bytes + attemptBytes
    }
}

