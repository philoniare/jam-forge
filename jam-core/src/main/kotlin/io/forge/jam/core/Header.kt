package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class Header(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val parent: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    @SerialName("parent_state_root")
    val parentStateRoot: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    @SerialName("extrinsic_hash")
    val extrinsicHash: JamByteArray,
    val slot: Long,
    @SerialName("epoch_mark")
    val epochMark: EpochMark?,
    @SerialName("tickets_mark")
    val ticketsMark: List<TicketMark>?,
    @SerialName("offenders_mark")
    @Serializable(with = JamByteArrayListHexSerializer::class)
    val offendersMark: List<JamByteArray>,
    @SerialName("author_index")
    val authorIndex: Long,
    @SerialName("entropy_source")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val entropySource: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val seal: JamByteArray
) : Encodable {
    override fun encode(): ByteArray {
        val parentBytes = parent.bytes
        val parentStateRootBytes = parentStateRoot.bytes
        val extrinsicHashBytes = extrinsicHash.bytes
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        val epochMarkBytes =
            if (epochMark != null) byteArrayOf(1) + epochMark.encode() else byteArrayOf(0)
        val ticketsMarkBytes =
            if (ticketsMark != null) byteArrayOf(1) + encodeList(ticketsMark, false) else byteArrayOf(0)
        val offendersMarkBytes =
            offendersMark.fold(encodeFixedWidthInteger(offendersMark.size, 1, false)) { acc, offender ->
                acc + offender.bytes
            }
        val authorIndexBytes = encodeFixedWidthInteger(authorIndex, 2, false)
        val entropySourceBytes = entropySource.bytes
        val sealBytes = seal.bytes
        return parentBytes +
            parentStateRootBytes +
            extrinsicHashBytes +
            slotBytes +
            epochMarkBytes +
            ticketsMarkBytes +
            offendersMarkBytes +
            authorIndexBytes +
            entropySourceBytes +
            sealBytes
    }
}

