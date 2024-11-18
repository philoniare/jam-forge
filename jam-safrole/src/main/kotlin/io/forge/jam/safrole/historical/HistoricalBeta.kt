package io.forge.jam.safrole.historical

import io.forge.jam.core.ReportedWorkPackage
import io.forge.jam.core.serializers.ByteArrayHexSerializer
import io.forge.jam.core.toHex
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class HistoricalBeta(
    @SerialName("header_hash")
    @Serializable(with = ByteArrayHexSerializer::class)
    val hash: ByteArray,

    val mmr: HistoricalMmr,

    @SerialName("state_root")
    @Serializable(with = ByteArrayHexSerializer::class)
    val stateRoot: ByteArray,

    @SerialName("reported")
    val reported: List<ReportedWorkPackage>
) {
    override fun toString(): String {
        return "\nHistoricalBeta(headerHash=${hash.toHex()}, mmr=$mmr, stateRoot=${stateRoot.toHex()}, reported=[$reported])"
    }
}
