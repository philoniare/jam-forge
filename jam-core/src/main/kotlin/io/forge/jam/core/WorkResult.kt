package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class WorkResult(
    @SerialName("service_id")
    val service: Long,
    @SerialName("code_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val codeHash: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    @SerialName("payload_hash")
    val payloadHash: JamByteArray,
    @SerialName("gas")
    val gas: Long,
    val result: ExecutionResult
) : Encodable {
    override fun encode(): ByteArray {
        val serviceBytes = encodeFixedWidthInteger(service, 4, false)
        val gasBytes = encodeFixedWidthInteger(gas, 8, false)
        return serviceBytes + codeHash.bytes + payloadHash.bytes + gasBytes + result.encode()
    }
}

