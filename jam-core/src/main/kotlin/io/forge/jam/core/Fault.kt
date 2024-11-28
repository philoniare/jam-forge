package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class Fault(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val target: JamByteArray,
    val vote: Boolean,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val key: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val signature: JamByteArray
) : Encodable {
    override fun encode(): ByteArray {
        val voteByte = byteArrayOf(vote.toByte())
        return target.bytes + voteByte + key.bytes + signature.bytes
    }
}
