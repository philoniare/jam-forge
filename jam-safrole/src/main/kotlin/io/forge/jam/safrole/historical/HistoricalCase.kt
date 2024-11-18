package io.forge.jam.safrole.historical

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class HistoricalCase(
    val input: HistoricalInput,
    @SerialName("pre_state")
    val preState: HistoricalState,
    @SerialName("post_state")
    val postState: HistoricalState
)
