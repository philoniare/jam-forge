package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.safrole.report.ReportOutputMarks
import kotlinx.serialization.Serializable

@Serializable
data class PreimageOutput(
    val ok: ReportOutputMarks? = null,
    @Serializable(with = PreimageErrorSerializer::class)
    val err: PreimageErrorCode? = null
) : Encodable {
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
