package io.forge.jam.safrole.historical

import io.forge.jam.core.Encodable
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.encodeList
import kotlinx.serialization.Serializable

@Serializable
data class HistoricalState(
    val beta: HistoricalBetaContainer
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<HistoricalState, Int> {
            val (beta, betaBytes) = HistoricalBetaContainer.fromBytes(data, offset)
            return Pair(HistoricalState(beta), betaBytes)
        }
    }

    override fun encode(): ByteArray = beta.encode()
}

@Serializable
data class HistoricalBetaContainer(
    val history: List<HistoricalBeta> = emptyList(),
    val mmr: HistoricalMmr = HistoricalMmr(emptyList())
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<HistoricalBetaContainer, Int> {
            var currentOffset = offset
            // history - variable-size list with compact integer length
            val (historyLength, historyLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += historyLengthBytes
            val history = mutableListOf<HistoricalBeta>()
            for (i in 0 until historyLength.toInt()) {
                val (beta, betaBytes) = HistoricalBeta.fromBytes(data, currentOffset)
                history.add(beta)
                currentOffset += betaBytes
            }
            // mmr
            val (mmr, mmrBytes) = HistoricalMmr.fromBytes(data, currentOffset)
            currentOffset += mmrBytes
            return Pair(HistoricalBetaContainer(history, mmr), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray = encodeList(history) + mmr.encode()
}
