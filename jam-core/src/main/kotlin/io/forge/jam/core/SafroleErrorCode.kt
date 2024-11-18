package io.forge.jam.core

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
enum class SafroleErrorCode {
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

    @SerialName("judgements-not-sorted-unique")
    JUDGEMENTS_NOT_SORTED_UNIQUE,

    @SerialName("already-judged")
    ALREADY_JUDGED,

    @SerialName("offender-already-reported")
    OFFENDER_ALREADY_REPORTED,

    @SerialName("culprits-verdict-not-bad")
    CULPRITS_VERDICT_NOT_BAD,

    @SerialName("bad-judgement-age")
    BAD_JUDGEMENT_AGE,

    @SerialName("verdicts-not-sorted-unique")
    VERDICTS_NOT_SORTED_UNIQUE,

    @SerialName("bad-vote-split")
    BAD_VOTE_SPLIT,

    @SerialName("fault-verdict-wrong")
    FAULT_VERDICT_WRONG,

    @SerialName("not-enough-faults")
    NOT_ENOUGH_FAULTS,

    @SerialName("faults-not-sorted-unique")
    FAULTS_NOT_SORTED_UNIQUE,
}
