package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class WorkItem(
    val service: Long,
    @SerialName("code_hash")
    @Serializable(with = JamByteArrayHexSerializer::class)
    val codeHash: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val payload: JamByteArray,
    @SerialName("refine_gas_limit")
    val refineGasLimit: Long,
    @SerialName("accumulate_gas_limit")
    val accumulateGasLimit: Long,
    @SerialName("import_segments")
    val importSegments: List<WorkItemImportSegment>,
    val extrinsic: List<WorkItemExtrinsic>,
    @SerialName("export_count")
    val exportCount: Long
) : Encodable {
    override fun encode(): ByteArray {
        val serviceBytes = encodeFixedWidthInteger(service, 4, false)
        val refineGasLimitBytes = encodeFixedWidthInteger(refineGasLimit, 8, false)
        val accumulateGasLimitBytes = encodeFixedWidthInteger(accumulateGasLimit, 8, false)
        val exportCountBytes = encodeFixedWidthInteger(exportCount, 2, false)
        val payloadLengthBytes = encodeCompactInteger(payload.size.toLong())
        val importSegmentsBytes = encodeList(importSegments)
        val extrinsicBytes = encodeList(extrinsic)
        return serviceBytes + codeHash.bytes + refineGasLimitBytes + accumulateGasLimitBytes + exportCountBytes + payloadLengthBytes + payload.bytes + importSegmentsBytes + extrinsicBytes
    }
}
