package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class WorkItemExtrinsic(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val hash: JamByteArray,
    val len: Long
) : Encodable {
    override fun encode(): ByteArray {
        val lenBytes = encodeFixedWidthInteger(len, 4, false)
        return hash.bytes + lenBytes
    }
}
