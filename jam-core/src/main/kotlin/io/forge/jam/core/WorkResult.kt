package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class RefineLoad(
    @SerialName("gas_used")
    val gasUsed: Long,
    val imports: Long,
    @SerialName("extrinsic_count")
    val extrinsicCount: Long,
    @SerialName("extrinsic_size")
    val extrinsicSize: Long,
    val exports: Long
) : Encodable {
    override fun encode(): ByteArray {
        return encodeCompactInteger(gasUsed) +
            encodeCompactInteger(imports) +
            encodeCompactInteger(extrinsicCount) +
            encodeCompactInteger(extrinsicSize) +
            encodeCompactInteger(exports)
    }
}

@Serializable
data class WorkResult(
    @SerialName("service_id")
    val serviceId: Long,
    @SerialName("code_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val codeHash: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    @SerialName("payload_hash")
    val payloadHash: JamByteArray,
    @SerialName("accumulate_gas")
    val accumulateGas: Long,
    val result: ExecutionResult,
    @SerialName("refine_load")
    val refineLoad: RefineLoad
) : Encodable {
    override fun encode(): ByteArray {
        val serviceBytes = encodeFixedWidthInteger(serviceId, 4, false)
        val gasBytes = encodeFixedWidthInteger(accumulateGas, 8, false)
        return serviceBytes + codeHash.bytes + payloadHash.bytes + gasBytes + result.encode() + refineLoad.encode()
    }
}

