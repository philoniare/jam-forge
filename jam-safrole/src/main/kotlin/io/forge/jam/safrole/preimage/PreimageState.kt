package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeList
import io.forge.jam.safrole.accumulation.ServiceStatisticsEntry
import kotlinx.serialization.Serializable

@Serializable
data class PreimageState(
    var accounts: List<PreimageAccount>,
    val statistics: List<ServiceStatisticsEntry> = emptyList()
) : Encodable {
    override fun encode(): ByteArray {
        return encodeList(accounts) + encodeList(statistics)
    }
}
