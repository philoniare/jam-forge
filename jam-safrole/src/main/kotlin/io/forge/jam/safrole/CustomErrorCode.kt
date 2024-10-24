package io.forge.jam.safrole

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
enum class CustomErrorCode {
    @SerialName("bad-slot")
    BAD_SLOT,

    @SerialName("unexpected-ticket")
    UNEXPECTED_TICKET,

    @SerialName("bad-ticket-order")
    BAD_TICKET_ORDER,

    @SerialName("bad-ticket-proof")
    BAD_TICKET_PROOF,

    @SerialName("bad-ticket-attempt")
    BAD_TICKET_ATTEMPT,

    @SerialName("reserved")
    RESERVED,

    @SerialName("duplicate-ticket")
    DUPLICATE_TICKET
}
