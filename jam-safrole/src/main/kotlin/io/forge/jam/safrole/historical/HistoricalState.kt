package io.forge.jam.safrole.historical

import kotlinx.serialization.Serializable

@Serializable
data class HistoricalState(
    val beta: List<HistoricalBeta>
)
