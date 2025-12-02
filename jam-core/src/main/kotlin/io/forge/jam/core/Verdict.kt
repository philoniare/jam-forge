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
    companion object {
        const val VOTES_COUNT = 1023 // 2/3 of 1023 validators + 1

        fun votesPerVerdict(validatorsCount: Int): Int = (2 * validatorsCount) / 3 + 1

        fun fromBytes(data: ByteArray, offset: Int = 0, votesCount: Int = VOTES_COUNT): Pair<Verdict, Int> {
            val target = JamByteArray(data.copyOfRange(offset, offset + 32))
            val age = decodeFixedWidthInteger(data, offset + 32, 4, false)
            val votes = decodeFixedList(data, offset + 36, votesCount, Vote.SIZE) { d, o ->
                Vote.fromBytes(d, o)
            }
            return Pair(Verdict(target, age, votes), 36 + votesCount * Vote.SIZE)
        }
    }

    override fun encode(): ByteArray {
        val targetBytes = target
        val ageBytes = encodeFixedWidthInteger(age, 4, false)
        val votesBytes = encodeList(votes, false)
        return targetBytes.bytes + ageBytes + votesBytes
    }
}
