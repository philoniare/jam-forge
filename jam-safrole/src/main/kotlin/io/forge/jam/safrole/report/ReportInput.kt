package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.GuaranteeExtrinsic
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.encodeList
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class ReportInput(
    val guarantees: List<GuaranteeExtrinsic>,
    val slot: Long,
    @SerialName("known_packages")
    @Serializable(with = JamByteArrayListHexSerializer::class)
    val knownPackages: List<JamByteArray> = emptyList()
) : Encodable {
    override fun encode(): ByteArray {
        // GuaranteesExtrinsic is SEQUENCE (SIZE(0..core-count)) - variable size, compact length
        val guaranteesLengthBytes = encodeCompactInteger(guarantees.size.toLong())
        val guaranteesBytes = encodeList(guarantees, includeLength = false)
        val slotBytes = encodeFixedWidthInteger(slot, 4, false)
        // known-packages is SEQUENCE OF WorkPackageHash - variable size, compact length
        val knownPackagesLengthBytes = encodeCompactInteger(knownPackages.size.toLong())
        val knownPackagesBytes = encodeList(knownPackages, includeLength = false)
        return guaranteesLengthBytes + guaranteesBytes + slotBytes + knownPackagesLengthBytes + knownPackagesBytes
    }
}
