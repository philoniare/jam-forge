package io.forge.jam.safrole.report

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class ReportPackage(
    @Serializable(with = JamByteArrayListHexSerializer::class)
    @SerialName("work_package_hash")
    val workPackageHash: List<JamByteArray>,
    @Serializable(with = JamByteArrayListHexSerializer::class)
    @SerialName("segment_tree_root")
    val segment_tree_root: List<JamByteArray>,
)
