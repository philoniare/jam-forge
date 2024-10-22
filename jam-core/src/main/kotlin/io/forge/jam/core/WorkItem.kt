package io.forge.jam.core

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class WorkItem(
    val service: Long,
    @SerialName("code_hash")
    @Serializable(with = ByteArrayHexSerializer::class)
    val codeHash: ByteArray,
    @Serializable(with = ByteArrayHexSerializer::class)
    val payload: ByteArray,
    @SerialName("gas_limit")
    val gasLimit: Long,
    @SerialName("import_segments")
    val importSegments: List<WorkItemImportSegment>,
    val extrinsic: List<WorkItemExtrinsic>,
    @SerialName("export_count")
    val exportCount: Long
) : Encodable {
    override fun encode(): ByteArray {
        val serviceBytes = encodeFixedWidthInteger(service, 4, false)
        val gasLimitBytes = encodeFixedWidthInteger(gasLimit, 8, false)
        val importSegmentsBytes = encodeList(importSegments)
        val extrinsicBytes = encodeList(extrinsic)
        val exportCountBytes = encodeFixedWidthInteger(exportCount, 2, false)
        val payloadLengthBytes = encodeFixedWidthInteger(payload.size, 1, false)
        return serviceBytes + codeHash + payloadLengthBytes + payload + gasLimitBytes + importSegmentsBytes + extrinsicBytes + exportCountBytes
    }
}
