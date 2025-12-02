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
data class HistoricalInput(
    @SerialName("header_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val headerHash: JamByteArray,

    @SerialName("parent_state_root")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val parentStateRoot: JamByteArray,

    @SerialName("accumulate_root")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val accumulateRoot: JamByteArray,

    @SerialName("work_packages")
    val workPackages: List<ReportedWorkPackage>
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<HistoricalInput, Int> {
            var currentOffset = offset
            val headerHash = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32
            val parentStateRoot = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32
            val accumulateRoot = JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32))
            currentOffset += 32
            // workPackages - variable-size list with compact integer length
            val (length, lengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += lengthBytes
            val workPackages = mutableListOf<ReportedWorkPackage>()
            for (i in 0 until length.toInt()) {
                workPackages.add(ReportedWorkPackage.fromBytes(data, currentOffset))
                currentOffset += ReportedWorkPackage.SIZE
            }
            return Pair(HistoricalInput(headerHash, parentStateRoot, accumulateRoot, workPackages), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        return headerHash.bytes + parentStateRoot.bytes + accumulateRoot.bytes + encodeList(workPackages)
    }
}
