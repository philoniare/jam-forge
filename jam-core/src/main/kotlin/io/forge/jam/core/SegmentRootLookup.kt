package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class SegmentRootLookup(
    @SerialName("work_package_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val workPackageHash: JamByteArray,
    @SerialName("segment_tree_root")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val segmentTreeRoot: JamByteArray
) : Encodable {
    override fun encode(): ByteArray {
        return workPackageHash.bytes + segmentTreeRoot.bytes
    }
}
