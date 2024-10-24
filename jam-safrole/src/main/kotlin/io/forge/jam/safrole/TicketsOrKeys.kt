package io.forge.jam.safrole

import io.forge.jam.core.ByteArrayListHexSerializer
import io.forge.jam.core.Ticket
import kotlinx.serialization.Serializable

@Serializable
data class TicketsOrKeys(
    @Serializable(with = ByteArrayListHexSerializer::class)
    val keys: List<ByteArray>? = null,
    val tickets: List<Ticket>? = null
)
