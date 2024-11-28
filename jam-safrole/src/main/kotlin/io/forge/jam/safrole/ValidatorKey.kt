package io.forge.jam.safrole

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
class ValidatorKey(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val bandersnatch: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val ed25519: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val bls: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    val metadata: JamByteArray
) : Encodable {
    override fun encode(): ByteArray {
        return bandersnatch.bytes + ed25519.bytes + bls.bytes + metadata.bytes
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ValidatorKey) return false
        return bandersnatch.contentEquals(other.bandersnatch) &&
            ed25519.contentEquals(other.ed25519) &&
            bls.contentEquals(other.bls) &&
            metadata.contentEquals(other.metadata)
    }

    override fun hashCode(): Int {
        var result = bandersnatch.contentHashCode()
        result = 31 * result + ed25519.contentHashCode()
        result = 31 * result + bls.contentHashCode()
        result = 31 * result + metadata.contentHashCode()
        return result
    }

    fun copy(): ValidatorKey {
        return ValidatorKey(
            bandersnatch = bandersnatch.clone(),
            ed25519 = ed25519.clone(),
            bls = bls.clone(),
            metadata = metadata.clone()
        )
    }
}
