package io.forge.jam.core

import kotlinx.serialization.Serializable

@Serializable
data class GuaranteeExtrinsic(
    val report: WorkReport,
    val slot: Long,
    val signatures: List<GuaranteeSignature>
) : Encodable {
    override fun encode(): ByteArray {
        val reportBytes = report.encode()
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        val signaturesBytes = encodeList(signatures)
        return reportBytes + slotBytes + signaturesBytes
    }
}
