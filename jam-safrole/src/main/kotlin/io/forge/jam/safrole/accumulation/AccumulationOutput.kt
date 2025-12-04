package io.forge.jam.safrole.accumulation

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.serializers.JamByteArrayHexSerializer
import io.forge.jam.safrole.report.ReportErrorCode
import io.forge.jam.safrole.report.ReportErrorCodeSerializer
import kotlinx.serialization.Serializable

/**
 * Accumulation statistics per service - used for computing fresh service statistics.
 * Contains gas used and work item count for each service accumulated in this block.
 */
typealias AccumulationStats = Map<Long, Pair<Long, Int>>  // serviceId -> (gasUsed, workItemCount)

@Serializable
data class AccumulationOutput(
    @Serializable(with = JamByteArrayHexSerializer::class)
    val ok: JamByteArray,
    @Serializable(with = ReportErrorCodeSerializer::class)
    val err: ReportErrorCode? = null,
    // Transient field - not serialized, used for computing final service stats
    @kotlinx.serialization.Transient
    val accumulationStats: AccumulationStats = emptyMap()
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<AccumulationOutput, Int> {
            var currentOffset = offset

            // discriminator - 0 = ok, 1 = err
            val discriminator = data[currentOffset].toInt() and 0xFF
            currentOffset += 1

            return if (discriminator == 0) {
                // ok - compact length + bytes
                val (okLength, okLengthBytes) = decodeCompactInteger(data, currentOffset)
                currentOffset += okLengthBytes
                val ok = JamByteArray(data.copyOfRange(currentOffset, currentOffset + okLength.toInt()))
                currentOffset += okLength.toInt()
                Pair(AccumulationOutput(ok = ok, err = null), currentOffset - offset)
            } else {
                val errorOrdinal = data[currentOffset].toInt() and 0xFF
                currentOffset += 1
                val error = ReportErrorCode.entries[errorOrdinal]
                Pair(AccumulationOutput(ok = JamByteArray(ByteArray(0)), err = error), currentOffset - offset)
            }
        }
    }

    override fun encode(): ByteArray {
        return if (ok != null) {
            byteArrayOf(0) + ok.encode()
        } else {
            err!!.encode()
        }
    }
}
