package io.forge.jam.safrole

import io.forge.jam.core.EpochMark
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class OutputMarks(
    @SerialName("epoch_mark")
    val epochMark: EpochMark? = null,
    @SerialName("tickets_mark")
    val ticketsMark: List<TicketBody>? = null,
) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is OutputMarks) return false
        return epochMark == other.epochMark && ticketsMark == other.ticketsMark
    }

    override fun hashCode(): Int {
        var result = epochMark?.hashCode() ?: 0
        result = 31 * result + (ticketsMark?.hashCode() ?: 0)
        return result
    }
}
