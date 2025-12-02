package io.forge.jam.safrole.dispute

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.core.encodeList
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class DisputeOutputMarks(
    @SerialName("offenders_mark")
    @Serializable(with = JamByteArrayListHexSerializer::class)
    val offendersMark: List<JamByteArray>? = null,
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<DisputeOutputMarks, Int> {
            var currentOffset = offset
            val (length, lengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += lengthBytes
            val offendersMark = mutableListOf<JamByteArray>()
            for (i in 0 until length.toInt()) {
                offendersMark.add(JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32)))
                currentOffset += 32
            }
            return Pair(DisputeOutputMarks(offendersMark), currentOffset - offset)
        }
    }
    override fun encode(): ByteArray {
        // OffendersMark is SEQUENCE OF Ed25519Public - variable-size, needs compact integer length
        val offendersMarkBytes = if (offendersMark != null) {
            val lengthBytes = encodeCompactInteger(offendersMark.size.toLong())
            val itemsBytes = encodeList(offendersMark, includeLength = false)
            lengthBytes + itemsBytes
        } else {
            ByteArray(0)
        }
        return offendersMarkBytes
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is DisputeOutputMarks) return false
        return offendersMark == other.offendersMark
    }

    override fun hashCode(): Int {
        return offendersMark?.hashCode() ?: 0
    }

    override fun toString(): String {
        val offendersMarkHex = offendersMark?.joinToString(", ") { it.toHex() } ?: "null"
        return "DisputeOutputMarks(offendersMark=[$offendersMarkHex])"
    }
}

