package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import kotlinx.serialization.Serializable

@Serializable
data class ReportOutput(
    val ok: ReportOutputMarks? = null,
    @Serializable(with = ReportErrorCodeSerializer::class)
    val err: ReportErrorCode? = null
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<ReportOutput, Int> {
            var currentOffset = offset
            val tag = data[currentOffset].toInt() and 0xFF
            currentOffset += 1
            return if (tag == 0) {
                // ok case - ReportOutput always has non-null ok when tag is 0
                val (marks, marksBytes) = ReportOutputMarks.fromBytes(data, currentOffset)
                Pair(ReportOutput(ok = marks), currentOffset - offset + marksBytes)
            } else {
                // err case
                val errCode = ReportErrorCode.entries[data[currentOffset].toInt() and 0xFF]
                Pair(ReportOutput(err = errCode), currentOffset - offset + 1)
            }
        }
    }

    override fun encode(): ByteArray {
        return if (ok != null) {
            // Prepend a 0 byte to indicate "ok" choice
            byteArrayOf(0) + ok.encode()
        } else {
            // For error case, prepend a 1 byte to indicate "err" choice
            byteArrayOf(1) + err!!.encode()
        }
    }
}
