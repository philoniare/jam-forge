package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.GuaranteeExtrinsic
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.decodeFixedWidthInteger
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
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<ReportInput, Int> {
            var currentOffset = offset
            // guarantees - variable size list
            val (guaranteesLength, guaranteesLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += guaranteesLengthBytes
            val guarantees = mutableListOf<GuaranteeExtrinsic>()
            for (i in 0 until guaranteesLength.toInt()) {
                val (guarantee, guaranteeBytes) = GuaranteeExtrinsic.fromBytes(data, currentOffset)
                guarantees.add(guarantee)
                currentOffset += guaranteeBytes
            }
            // slot - 4 bytes
            val slot = decodeFixedWidthInteger(data, currentOffset, 4, false)
            currentOffset += 4
            // knownPackages - variable size list of 32-byte hashes
            val (knownPackagesLength, knownPackagesLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += knownPackagesLengthBytes
            val knownPackages = mutableListOf<JamByteArray>()
            for (i in 0 until knownPackagesLength.toInt()) {
                knownPackages.add(JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32)))
                currentOffset += 32
            }
            return Pair(ReportInput(guarantees, slot, knownPackages), currentOffset - offset)
        }
    }

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
