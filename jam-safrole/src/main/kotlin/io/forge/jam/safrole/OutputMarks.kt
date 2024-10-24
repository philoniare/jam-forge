package io.forge.jam.safrole

import io.forge.jam.core.EpochMark
import io.forge.jam.core.Ticket
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class OutputMarks(
    @SerialName("epoch_mark")
    val epochMark: EpochMark? = null,
    @SerialName("tickets_mark")
    val ticketsMark: List<Ticket>? = null,
)
