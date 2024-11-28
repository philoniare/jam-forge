package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class Culprit(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val target: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val key: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val signature: JamByteArray
) : Encodable {
    override fun encode(): ByteArray {
        return target.bytes + key.bytes + signature.bytes
    }
}
