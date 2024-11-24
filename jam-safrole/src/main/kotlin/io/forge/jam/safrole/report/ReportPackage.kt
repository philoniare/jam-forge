package io.forge.jam.safrole.report

import io.forge.jam.core.serializers.ByteArrayListHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class ReportPackage(
    @Serializable(with = ByteArrayListHexSerializer::class)
    @SerialName("work_package_hash")
    val workPackageHash: List<ByteArray>,
    @Serializable(with = ByteArrayListHexSerializer::class)
    @SerialName("segment_tree_root")
    val segment_tree_root: List<ByteArray>,
)
