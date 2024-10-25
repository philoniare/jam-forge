package io.forge.jam.core

import kotlinx.serialization.Serializable

@Serializable
data class EpochMark(
    @Serializable(with = ByteArrayHexSerializer::class)
    val entropy: ByteArray,
    @Serializable(with = ByteArrayListHexSerializer::class)
    val validators: List<ByteArray>
) : Encodable {
    override fun encode(): ByteArray {
        val validatorsBytes =
            validators.fold(byteArrayOf()) { acc, validator ->
                acc + validator
            }
        return entropy + validatorsBytes
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is EpochMark) return false
        return entropy.contentEquals(other.entropy) &&
            validators.size == other.validators.size &&
            validators.zip(other.validators).all { (a, b) -> a.contentEquals(b) }
    }

    override fun hashCode(): Int {
        var result = entropy.contentHashCode()
        result = 31 * result + validators.fold(0) { acc, bytes ->
            31 * acc + bytes.contentHashCode()
        }
        return result
    }
}
