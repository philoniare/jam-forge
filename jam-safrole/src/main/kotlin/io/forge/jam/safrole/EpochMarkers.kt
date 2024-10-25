package io.forge.jam.safrole

import io.forge.jam.core.TicketEnvelope

data class EpochMarkers(
    val epochMarker: Pair<ByteArray, List<ByteArray>>?,
    val winningTickets: List<TicketEnvelope>?
)

