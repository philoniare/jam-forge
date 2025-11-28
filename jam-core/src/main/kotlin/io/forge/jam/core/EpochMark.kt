package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class EpochMark(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val entropy: JamByteArray,
    @Serializable(with = JamByteArrayHexSerializer::class)
    @SerialName("tickets_entropy")
    val ticketsEntropy: JamByteArray,
    val validators: List<ValidatorKey>
) : Encodable {
    override fun toString(): String {
        return "EpochMark(" +
            "entropy=${entropy.toHex()}, " +
            "ticketsEntropy=${ticketsEntropy.toHex()}, " +
            "validators=$validators" +
            ")"
    }

    override fun encode(): ByteArray {
        val validatorsBytes =
            validators.fold(byteArrayOf()) { acc, validator ->
                acc + validator.encode()
            }
        return entropy.bytes + ticketsEntropy.bytes + validatorsBytes
    }
}
