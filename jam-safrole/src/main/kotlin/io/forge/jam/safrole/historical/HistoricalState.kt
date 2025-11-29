package io.forge.jam.safrole.historical

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeList
import kotlinx.serialization.Serializable

@Serializable
data class HistoricalState(
    val beta: HistoricalBetaContainer
) : Encodable {
    override fun encode(): ByteArray = beta.encode()
}

@Serializable
data class HistoricalBetaContainer(
    val history: List<HistoricalBeta> = emptyList(),
    val mmr: HistoricalMmr = HistoricalMmr(emptyList())
) : Encodable {
    override fun encode(): ByteArray = encodeList(history) + mmr.encode()
}
