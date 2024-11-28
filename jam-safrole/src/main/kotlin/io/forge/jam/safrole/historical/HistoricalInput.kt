package io.forge.jam.safrole.historical

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.ReportedWorkPackage
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
)
