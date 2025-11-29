package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.encodeListWithCompactLength
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class ReportOutputMarks(
    val reported: List<ReportPackage>,
    @Serializable(with = JamByteArrayListHexSerializer::class)
    val reporters: List<JamByteArray>
) : Encodable {
    override fun encode(): ByteArray {
        // Both are SEQUENCE OF - variable size, need compact length
        val reportedBytes = encodeListWithCompactLength(reported)
        val reportersBytes = encodeListWithCompactLength(reporters)
        return reportedBytes + reportersBytes
    }
}
