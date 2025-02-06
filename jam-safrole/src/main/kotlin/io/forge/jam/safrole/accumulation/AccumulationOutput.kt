package io.forge.jam.safrole.accumulation

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import io.forge.jam.safrole.report.ReportErrorCode
import io.forge.jam.safrole.report.ReportErrorCodeSerializer
import kotlinx.serialization.Serializable

@Serializable
data class AccumulationOutput(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val ok: JamByteArray,
    @Serializable(with = ReportErrorCodeSerializer::class)
    val err: ReportErrorCode? = null
) : Encodable {
    override fun encode(): ByteArray {
        return if (ok != null) {
            byteArrayOf(0) + ok.encode()
        } else {
            err!!.encode()
        }
    }
}
