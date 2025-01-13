package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.GuaranteeExtrinsic
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.encodeList
import kotlinx.serialization.Serializable

@Serializable
data class ReportInput(
    val guarantees: List<GuaranteeExtrinsic>,
    val slot: Long,
) : Encodable {
    override fun encode(): ByteArray {
        val guaranteesBytes = encodeList(guarantees)
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        return guaranteesBytes + slotBytes
    }
}
