package io.forge.jam.safrole.historical

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.ReportedWorkPackage
import io.forge.jam.core.encodeList
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class HistoricalBeta(
    @SerialName("header_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val headerHash: JamByteArray,

    val mmr: HistoricalMmr,

    @SerialName("state_root")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val stateRoot: JamByteArray,

    @SerialName("reported")
    val reported: List<ReportedWorkPackage>
) : Encodable {
    override fun encode(): ByteArray {
        val mmrBytes = mmr.encode()
        val reportedBytes = encodeList(reported)
        return headerHash.bytes + mmrBytes + stateRoot.bytes + reportedBytes
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || this::class != other::class) return false

        other as HistoricalBeta

        if (!headerHash.contentEquals(other.headerHash)) return false
        if (!stateRoot.contentEquals(other.stateRoot)) return false

        if (mmr != other.mmr) return false
        if (reported != other.reported) return false

        return true
    }

    override fun hashCode(): Int {
        var result = headerHash.contentHashCode()
        result = 31 * result + mmr.hashCode()
        result = 31 * result + stateRoot.contentHashCode()
        result = 31 * result + reported.hashCode()
        return result
    }

    override fun toString(): String {
        return "\nHistoricalBeta(headerHash=${headerHash.toHex()}, mmr=$mmr, stateRoot=${stateRoot.toHex()}, reported=[$reported])"
    }
}
