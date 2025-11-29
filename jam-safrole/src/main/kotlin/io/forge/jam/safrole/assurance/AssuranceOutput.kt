package io.forge.jam.safrole.assurance

import io.forge.jam.core.Encodable
import kotlinx.serialization.Serializable

@Serializable
data class AssuranceOutput(
    val ok: AssuranceOutputMarks? = null,
    @Serializable(with = AssuranceErrorSerializer::class)
    val err: AssuranceErrorCode? = null
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
