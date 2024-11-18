package io.forge.jam.core

import io.forge.jam.core.serializers.ByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class WorkResult(
    @SerialName("service_id")
    val service: Long,
    @SerialName("code_hash")
    @Serializable(with = ByteArrayHexSerializer::class)
    val codeHash: ByteArray,
    @Serializable(with = ByteArrayHexSerializer::class)
    @SerialName("payload_hash")
    val payloadHash: ByteArray,
    @SerialName("gas")
    val gas: Long,
    val result: ExecutionResult
) : Encodable {
    override fun encode(): ByteArray {
        val serviceBytes = encodeFixedWidthInteger(service, 4, false)
        val gasBytes = encodeFixedWidthInteger(gas, 8, false)
        return serviceBytes + codeHash + payloadHash + gasBytes + result.encode()
    }
}

