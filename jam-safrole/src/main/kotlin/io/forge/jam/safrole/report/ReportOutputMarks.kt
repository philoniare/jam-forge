package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.encodeListWithCompactLength
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class ReportOutputMarks(
    val reported: List<ReportPackage>,
    @Serializable(with = JamByteArrayListHexSerializer::class)
    val reporters: List<JamByteArray>
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<ReportOutputMarks, Int> {
            var currentOffset = offset
            // reported - variable size list of ReportPackage
            val (reportedLength, reportedLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += reportedLengthBytes
            val reported = mutableListOf<ReportPackage>()
            for (i in 0 until reportedLength.toInt()) {
                reported.add(ReportPackage.fromBytes(data, currentOffset))
                currentOffset += ReportPackage.SIZE
            }
            // reporters - variable size list of 32-byte hashes
            val (reportersLength, reportersLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += reportersLengthBytes
            val reporters = mutableListOf<JamByteArray>()
            for (i in 0 until reportersLength.toInt()) {
                reporters.add(JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32)))
                currentOffset += 32
            }
            return Pair(ReportOutputMarks(reported, reporters), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        // Both are SEQUENCE OF - variable size, need compact length
        val reportedBytes = encodeListWithCompactLength(reported)
        val reportersBytes = encodeListWithCompactLength(reporters)
        return reportedBytes + reportersBytes
    }
}
