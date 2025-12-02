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
    companion object {
        const val SIZE = 64 // 32 bytes hash + 32 bytes segmentTreeRoot

        fun fromBytes(data: ByteArray, offset: Int = 0): ReportPackage {
            val workPackageHash = JamByteArray(data.copyOfRange(offset, offset + 32))
            val segmentTreeRoot = JamByteArray(data.copyOfRange(offset + 32, offset + 64))
            return ReportPackage(workPackageHash, segmentTreeRoot)
        }
    }

    override fun encode(): ByteArray {
        return workPackageHash.bytes + segmentTreeRoot.bytes
    }
}
