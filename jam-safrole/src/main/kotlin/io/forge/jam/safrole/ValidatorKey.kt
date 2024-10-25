package io.forge.jam.safrole

import io.forge.jam.core.ByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
class ValidatorKey(
    @Serializable(with = ByteArrayHexSerializer::class)
    val bandersnatch: ByteArray,
    @Serializable(with = ByteArrayHexSerializer::class)
    val ed25519: ByteArray,
    @Serializable(with = ByteArrayHexSerializer::class)
    val bls: ByteArray,
    @Serializable(with = ByteArrayHexSerializer::class)
    val metadata: ByteArray
) {
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
}
