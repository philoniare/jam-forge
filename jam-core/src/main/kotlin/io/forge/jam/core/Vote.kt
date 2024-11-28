package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class Vote(
    val vote: Boolean,
    val index: Long,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val signature: JamByteArray
) : Encodable {
    override fun encode(): ByteArray {
        val voteByte = byteArrayOf(if (vote) 1.toByte() else 0.toByte())
        val indexBytes = encodeFixedWidthInteger(index, 2, false)
        val signatureBytes = signature.bytes
        return voteByte + indexBytes + signatureBytes
    }
}
