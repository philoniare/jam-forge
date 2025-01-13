package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class ReportPackage(
    @Serializable(with = JamByteArrayHexSerializer::class)
    @SerialName("work_package_hash")
    val workPackageHash: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    @SerialName("segment_tree_root")
    val segmentTreeRoot: JamByteArray,
) : Encodable {
    override fun encode(): ByteArray {
        return workPackageHash.bytes + segmentTreeRoot.bytes
    }
}
