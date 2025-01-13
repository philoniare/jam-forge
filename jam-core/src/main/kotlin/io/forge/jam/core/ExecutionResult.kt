package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class ExecutionResult(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val ok: JamByteArray? = null,
    val panic: Boolean? = null,
) : Encodable {
    override fun encode(): ByteArray {
        if (ok != null) {
            val lengthBytes = encodeCompactInteger(ok.bytes.size.toLong())
            return byteArrayOf(0) + lengthBytes + ok.bytes
        } else {
            return byteArrayOf(2)
        }
    }
}
