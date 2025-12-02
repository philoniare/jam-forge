package io.forge.jam.safrole.historical

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.encodeOptionalList
import io.forge.jam.core.serializers.NullableJamByteArrayListSerializer
import kotlinx.serialization.Serializable

@Serializable
data class HistoricalMmr(
    @Serializable(with = NullableJamByteArrayListSerializer::class)
    val peaks: List<JamByteArray?>
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<HistoricalMmr, Int> {
            var currentOffset = offset
            val (length, lengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += lengthBytes
            val peaks = mutableListOf<JamByteArray?>()
            for (i in 0 until length.toInt()) {
                val optionByte = data[currentOffset].toInt() and 0xFF
                currentOffset += 1
                if (optionByte == 0) {
                    peaks.add(null)
                } else {
                    peaks.add(JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32)))
                    currentOffset += 32
                }
            }
            return Pair(HistoricalMmr(peaks), currentOffset - offset)
        }
    }
    override fun encode(): ByteArray {
        return encodeOptionalList(peaks)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is HistoricalMmr) return false

        if (peaks.size != other.peaks.size) return false

        return peaks.zip(other.peaks).all { (a, b) ->
            a === b || (a != null && b != null && a.contentEquals(b))
        }
    }

    override fun hashCode(): Int {
        return peaks.fold(0) { acc, bytes ->
            31 * acc + (bytes?.contentHashCode() ?: 0)
        }
    }

    override fun toString(): String {
        return "HistoricalMmr(peaks=${peaks.map { it?.toHex() ?: "null" }})"
    }
}
