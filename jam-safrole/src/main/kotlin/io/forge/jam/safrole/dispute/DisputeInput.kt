package io.forge.jam.safrole.safrole

import io.forge.jam.core.Dispute
import io.forge.jam.core.Encodable
import kotlinx.serialization.Serializable

@Serializable
data class DisputeInput(
    val disputes: Dispute? = null
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, votesPerVerdict: Int): Pair<DisputeInput, Int> {
            var currentOffset = offset
            // Always decode as Dispute (test vectors have disputes as object, never null in JSON)
            val (dispute, disputeBytes) = Dispute.fromBytes(data, currentOffset, votesPerVerdict)
            currentOffset += disputeBytes
            return Pair(DisputeInput(dispute), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val disputesBytes = disputes?.encode() ?: byteArrayOf(0)
        return disputesBytes
    }
}
