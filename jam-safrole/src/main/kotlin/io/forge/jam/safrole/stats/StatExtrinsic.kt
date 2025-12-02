package io.forge.jam.safrole.stats

import io.forge.jam.core.*
import kotlinx.serialization.Serializable

@Serializable
data class StatExtrinsic(
    val tickets: List<TicketEnvelope>,
    val preimages: List<Preimage>,
    val guarantees: List<GuaranteeExtrinsic>,
    val assurances: List<AssuranceExtrinsic>,
    val disputes: Dispute
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, coresCount: Int, validatorsCount: Int): Pair<StatExtrinsic, Int> {
            var currentOffset = offset

            // tickets - compact length + fixed-size items
            val (ticketsLength, ticketsLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += ticketsLengthBytes
            val tickets = mutableListOf<TicketEnvelope>()
            for (i in 0 until ticketsLength.toInt()) {
                tickets.add(TicketEnvelope.fromBytes(data, currentOffset))
                currentOffset += TicketEnvelope.SIZE
            }

            // preimages - compact length + variable-size items
            val (preimagesLength, preimagesLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += preimagesLengthBytes
            val preimages = mutableListOf<Preimage>()
            for (i in 0 until preimagesLength.toInt()) {
                val (preimage, preimageBytes) = Preimage.fromBytes(data, currentOffset)
                preimages.add(preimage)
                currentOffset += preimageBytes
            }

            // guarantees - compact length + variable-size items
            val (guaranteesLength, guaranteesLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += guaranteesLengthBytes
            val guarantees = mutableListOf<GuaranteeExtrinsic>()
            for (i in 0 until guaranteesLength.toInt()) {
                val (guarantee, guaranteeBytes) = GuaranteeExtrinsic.fromBytes(data, currentOffset)
                guarantees.add(guarantee)
                currentOffset += guaranteeBytes
            }

            // assurances - compact length + fixed-size items
            val (assurancesLength, assurancesLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += assurancesLengthBytes
            val assurances = mutableListOf<AssuranceExtrinsic>()
            val assuranceSize = AssuranceExtrinsic.size(coresCount)
            for (i in 0 until assurancesLength.toInt()) {
                assurances.add(AssuranceExtrinsic.fromBytes(data, currentOffset, coresCount))
                currentOffset += assuranceSize
            }

            // disputes - variable size
            val (disputes, disputesBytes) = Dispute.fromBytes(data, currentOffset, validatorsCount)
            currentOffset += disputesBytes

            return Pair(StatExtrinsic(tickets, preimages, guarantees, assurances, disputes), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        return encodeList(tickets) + encodeList(preimages) + encodeList(guarantees) + encodeList(assurances) + disputes.encode()
    }
}
