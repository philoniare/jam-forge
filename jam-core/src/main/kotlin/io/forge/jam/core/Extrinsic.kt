package io.forge.jam.core

import kotlinx.serialization.Serializable

@Serializable
data class Extrinsic(
    val tickets: List<TicketEnvelope>,
    val preimages: List<Preimage>,
    val guarantees: List<GuaranteeExtrinsic>,
    val assurances: List<AssuranceExtrinsic>,
    val disputes: Dispute,
) : Encodable {
    override fun encode(): ByteArray {
        val ticketsBytes = encodeList(tickets)
        val disputesBytes = disputes.encode()
        val preimagesBytes = encodeList(preimages)
        val assurancesBytes = encodeList(assurances)
        val guaranteesBytes = encodeList(guarantees)
        return ticketsBytes + preimagesBytes + guaranteesBytes + assurancesBytes + disputesBytes
    }
}

