package io.forge.jam.safrole.historical

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.ReportedWorkPackage
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.encodeList
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class HistoricalBeta(
    @SerialName("header_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val headerHash: JamByteArray,

    @SerialName("beefy_root")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val beefyRoot: JamByteArray,

    @SerialName("state_root")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val stateRoot: JamByteArray,

    @SerialName("reported")
    val reported: List<ReportedWorkPackage>
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<HistoricalBeta, Int> {
            var currentOffset = offset
            val headerHash = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32
            val beefyRoot = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32
            val stateRoot = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32
            // reported - variable-size list with compact integer length
            val (reportedLength, reportedLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += reportedLengthBytes
            val reported = mutableListOf<ReportedWorkPackage>()
            for (i in 0 until reportedLength.toInt()) {
                reported.add(ReportedWorkPackage.fromBytes(data, currentOffset))
                currentOffset += ReportedWorkPackage.SIZE
            }
            return Pair(HistoricalBeta(headerHash, beefyRoot, stateRoot, reported), currentOffset - offset)
        }
    }
    override fun encode(): ByteArray {
        val reportedBytes = encodeList(reported)
        return headerHash.bytes + beefyRoot.bytes + stateRoot.bytes + reportedBytes
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || this::class != other::class) return false

        other as HistoricalBeta

        if (!headerHash.contentEquals(other.headerHash)) return false
        if (!beefyRoot.contentEquals(other.beefyRoot)) return false
        if (!stateRoot.contentEquals(other.stateRoot)) return false
        if (reported != other.reported) return false

        return true
    }

    override fun hashCode(): Int {
        var result = headerHash.contentHashCode()
        result = 31 * result + beefyRoot.contentHashCode()
        result = 31 * result + stateRoot.contentHashCode()
        result = 31 * result + reported.hashCode()
        return result
    }

    override fun toString(): String {
        return "\nHistoricalBeta(headerHash=${headerHash.toHex()}, beefyRoot=${beefyRoot.toHex()}, stateRoot=${stateRoot.toHex()}, reported=[$reported])"
    }
}
