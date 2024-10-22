package io.forge.jam.core

import kotlinx.serialization.Serializable

@Serializable
data class Extrinsic(
    val tickets: List<Ticket>,
    val disputes: Dispute,
    val preimages: List<Preimage>,
    val assurances: List<AssuranceExtrinsic>,
    val guarantees: List<GuaranteeExtrinsic>
) : Encodable {
    override fun encode(): ByteArray {
        val ticketsBytes = encodeList(tickets)
        val disputesBytes = disputes.encode()
        val preimagesBytes = encodeList(preimages)
        val assurancesBytes = encodeList(assurances)
        val guaranteesBytes = encodeList(guarantees)
        return ticketsBytes + disputesBytes + preimagesBytes + assurancesBytes + guaranteesBytes
    }
}

