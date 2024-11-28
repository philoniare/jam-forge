package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class Verdict(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val target: JamByteArray,
    val age: Long,
    val votes: List<Vote>
) : Encodable {
    override fun encode(): ByteArray {
        val targetBytes = target
        val ageBytes = encodeFixedWidthInteger(age, 4, false)
        val votesBytes = encodeList(votes, false)
        return targetBytes.bytes + ageBytes + votesBytes
    }
}
