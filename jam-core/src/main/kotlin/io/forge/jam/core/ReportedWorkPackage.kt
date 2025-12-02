package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class ReportedWorkPackage(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val hash: JamByteArray,
    @SerialName("exports_root")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val exportsRoot: JamByteArray
) : Encodable {
    companion object {
        const val SIZE = 64 // 32 bytes hash + 32 bytes exportsRoot

        fun fromBytes(data: ByteArray, offset: Int = 0): ReportedWorkPackage {
            val hash = JamByteArray(data.copyOfRange(offset, offset + 32))
            val exportsRoot = JamByteArray(data.copyOfRange(offset + 32, offset + 64))
            return ReportedWorkPackage(hash, exportsRoot)
        }
    }
    override fun encode(): ByteArray {
        return hash.bytes + exportsRoot.bytes
    }
}
