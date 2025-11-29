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
        // signatures is SEQUENCE OF ValidatorSignature - variable size, compact integer length
        val signaturesLengthBytes = encodeCompactInteger(signatures.size.toLong())
        val signaturesBytes = encodeList(signatures, includeLength = false)
        return reportBytes + slotBytes + signaturesLengthBytes + signaturesBytes
    }
}
