package io.forge.jam.core

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
enum class JamErrorCode {
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
    DUPLICATE_TICKET,

    @SerialName("bad-signature")
    BAD_SIGNATURE,

    @SerialName("not-enough-culprits")
    NOT_ENOUGH_CULPRITS,

    @SerialName("culprits-not-sorted-unique")
    CULPRITS_NOT_SORTED_UNIQUE,

    @SerialName("already-judged")
    ALREADY_JUDGED,
}
