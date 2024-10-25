package io.forge.jam.safrole

import io.forge.jam.core.ByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class TicketBody(
    @Serializable(with = ByteArrayHexSerializer::class)
    val id: ByteArray,
    val attempt: Long
) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is TicketBody) return false
        return id.contentEquals(other.id) && attempt == other.attempt
    }

    override fun hashCode(): Int {
        var result = id.contentHashCode()
        result = 31 * result + attempt.hashCode()
        return result
    }
}
