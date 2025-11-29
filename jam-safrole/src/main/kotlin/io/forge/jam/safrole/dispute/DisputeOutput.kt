package io.forge.jam.safrole.dispute

import io.forge.jam.core.Encodable
import kotlinx.serialization.Serializable

@Serializable
data class DisputeOutput(
    val ok: DisputeOutputMarks? = null,
    @Serializable(with = DisputeErrorCodeSerializer::class)
    val err: DisputeErrorCode? = null
) : Encodable {
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
