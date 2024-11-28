package io.forge.jam.safrole

import io.forge.jam.core.Encodable
import io.forge.jam.core.EpochMark
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.encodeList
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class OutputMarks(
    @SerialName("epoch_mark")
    val epochMark: EpochMark? = null,
    @SerialName("tickets_mark")
    val ticketsMark: List<TicketBody>? = null,
    @SerialName("offenders_mark")
    @Serializable(with = JamByteArrayListHexSerializer::class)
    val offendersMark: List<JamByteArray>? = null,
) : Encodable {
    override fun encode(): ByteArray {
        val epochMarkBytes = epochMark?.encode() ?: ByteArray(0)
        val ticketsMarkBytes = if (ticketsMark != null) encodeList(ticketsMark) else ByteArray(0)
        return epochMarkBytes
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is OutputMarks) return false
        return epochMark == other.epochMark && ticketsMark == other.ticketsMark && offendersMark == other.offendersMark
    }

    override fun hashCode(): Int {
        var result = epochMark?.hashCode() ?: 0
        result = 31 * result + (ticketsMark?.hashCode() ?: 0)
        result = 31 * result + (offendersMark?.hashCode() ?: 0)
        return result
    }

    override fun toString(): String {
        val offendersMarkHex = offendersMark?.joinToString(", ") { it.toHex() } ?: "null"
        return "OutputMarks(epochMark=$epochMark, ticketsMark=$ticketsMark, offendersMark=[$offendersMarkHex])"
    }
}
