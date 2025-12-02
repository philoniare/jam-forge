package io.forge.jam.safrole.dispute

import io.forge.jam.core.Encodable
import kotlinx.serialization.Serializable

@Serializable
data class DisputeOutput(
    val ok: DisputeOutputMarks? = null,
    @Serializable(with = DisputeErrorCodeSerializer::class)
    val err: DisputeErrorCode? = null
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<DisputeOutput, Int> {
            var currentOffset = offset
            val tag = data[currentOffset].toInt() and 0xFF
            currentOffset += 1
            return if (tag == 0) {
                // ok case
                val (marks, marksBytes) = DisputeOutputMarks.fromBytes(data, currentOffset)
                Pair(DisputeOutput(ok = marks), currentOffset - offset + marksBytes)
            } else {
                // err case
                val errCode = DisputeErrorCode.values()[data[currentOffset].toInt() and 0xFF]
                Pair(DisputeOutput(err = errCode), currentOffset - offset + 1)
            }
        }
    }
    override fun encode(): ByteArray {
        return if (ok != null) {
            // Prepend a 0 byte to indicate "ok" choice
            byteArrayOf(0) + ok.encode()
        } else {
            // Prepend a 1 byte to indicate "err" choice
            byteArrayOf(1) + err!!.encode()
        }
    }
}
