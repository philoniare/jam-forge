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
    override fun encode(): ByteArray {
        return hash.bytes + exportsRoot.bytes
    }
}
