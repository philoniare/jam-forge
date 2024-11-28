package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable


@Serializable
data class WorkAuthorizer(
    @SerialName("code_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val codeHash: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val params: JamByteArray,
) : Encodable {
    override fun encode(): ByteArray {
        val paramsLengthBytes = encodeFixedWidthInteger(params.size, 1, false)
        return codeHash.bytes + paramsLengthBytes + params.bytes
    }
}

