package io.forge.jam.core

import kotlinx.serialization.Serializable

@Serializable
data class WorkItemExtrinsic(
    @Serializable(with = ByteArrayHexSerializer::class)
    val hash: ByteArray,
    val len: Long
) : Encodable {
    override fun encode(): ByteArray {
        val lenBytes = encodeFixedWidthInteger(len, 4, false)
        return hash + lenBytes
    }
}
