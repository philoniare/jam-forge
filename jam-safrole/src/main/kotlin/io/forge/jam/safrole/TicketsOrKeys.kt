package io.forge.jam.safrole

import io.forge.jam.core.ByteArrayListHexSerializer
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
}
