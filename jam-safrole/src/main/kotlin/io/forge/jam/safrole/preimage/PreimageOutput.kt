package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.safrole.report.ReportOutputMarks
import kotlinx.serialization.Serializable

@Serializable
data class PreimageOutput(
    val ok: ReportOutputMarks? = null,
    @Serializable(with = PreimageErrorSerializer::class)
    val err: PreimageErrorCode? = null,
    @kotlinx.serialization.Transient
    val rawServiceDataUpdates: Map<JamByteArray, JamByteArray> = emptyMap()
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<PreimageOutput, Int> {
            var currentOffset = offset
            val tag = data[currentOffset].toInt() and 0xFF
            currentOffset += 1
            return if (tag == 0) {
                Pair(PreimageOutput(ok = null), 1)
            } else {
                // err case
                val errCode = PreimageErrorCode.values()[data[currentOffset].toInt() and 0xFF]
                Pair(PreimageOutput(err = errCode), currentOffset - offset + 1)
            }
        }
    }

    override fun encode(): ByteArray {
        return when {
            err != null -> {
                // For error case, prepend a 1 byte to indicate "err" choice
                byteArrayOf(1) + err.encode()
            }

            ok != null -> {
                // ok with content, prepend a 0 byte to indicate "ok" choice
                byteArrayOf(0) + ok.encode()
            }

            else -> {
                // ok with null content (no marks)
                byteArrayOf(0)
            }
        }
    }
}
