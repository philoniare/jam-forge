package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class PreimageHash(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val hash: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val blob: JamByteArray
) : Encodable {
    override fun encode(): ByteArray {
        val lengthBytes = encodeCompactInteger(blob.bytes.size.toLong())
        return hash.bytes + lengthBytes + blob.bytes
    }
}
