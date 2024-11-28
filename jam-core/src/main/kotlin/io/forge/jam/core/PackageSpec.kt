package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class PackageSpec(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val hash: JamByteArray,
    val length: Long,
    @Serializable(with = JamByteArrayHexSerializer::class)
    @SerialName("erasure_root")
    val erasureRoot: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    @SerialName("exports_root")
    val exportsRoot: JamByteArray
) : Encodable {
    override fun encode(): ByteArray {
        val hashBytes = hash.bytes
        val lenBytes = encodeFixedWidthInteger(length, 4, false)
        val erasureRootBytes = erasureRoot.bytes
        val exportsRootBytes = exportsRoot.bytes
        return hashBytes + lenBytes + erasureRootBytes + exportsRootBytes
    }
}
