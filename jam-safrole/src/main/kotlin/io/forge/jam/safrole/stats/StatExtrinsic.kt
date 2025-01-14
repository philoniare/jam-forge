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
    override fun encode(): ByteArray {
        return encodeList(tickets) + encodeList(preimages) + encodeList(guarantees) + encodeList(assurances) + disputes.encode()
    }
}
