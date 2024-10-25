package io.forge.jam.safrole

import io.forge.jam.core.ByteArrayListHexSerializer
import io.forge.jam.core.toHex
import kotlinx.serialization.Serializable

@Serializable
data class TicketsOrKeys(
    @Serializable(with = ByteArrayListHexSerializer::class)
    val keys: List<ByteArray>? = null,
    val tickets: List<TicketBody>? = null
) {
    companion object {
        fun fromKeys(keys: List<ByteArray>) = TicketsOrKeys(keys = keys)
        fun fromTickets(tickets: List<TicketBody>) = TicketsOrKeys(tickets = tickets)
    }

    override fun toString(): String {
        val keyHex = keys?.joinToString(",") { it.toHex() } ?: "null"
        val ticketsString = tickets?.joinToString(",") ?: "null"
        return "TicketsOrKeys(keys=$keyHex, tickets=$ticketsString)"
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is TicketsOrKeys) return false

        // Deep comparison for `keys`
        if (keys != null && other.keys != null) {
            if (keys.size != other.keys.size) return false
            for (i in keys.indices) {
                if (!keys[i].contentEquals(other.keys[i])) return false
            }
        } else if (keys != other.keys) {
            return false
        }

        // Use default equality for `tickets`
        return tickets == other.tickets
    }

    override fun hashCode(): Int {
        var result = keys?.fold(0) { acc, byteArray -> acc * 31 + byteArray.contentHashCode() } ?: 0
        result = 31 * result + (tickets?.hashCode() ?: 0)
        return result
    }
}
