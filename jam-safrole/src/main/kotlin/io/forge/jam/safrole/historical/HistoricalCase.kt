package io.forge.jam.safrole.historical

import io.forge.jam.core.Encodable
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class HistoricalCase(
    val input: HistoricalInput,
    @SerialName("pre_state")
    val preState: HistoricalState,
    @SerialName("post_state")
    val postState: HistoricalState
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<HistoricalCase, Int> {
            var currentOffset = offset
            val (input, inputBytes) = HistoricalInput.fromBytes(data, currentOffset)
            currentOffset += inputBytes
            val (preState, preStateBytes) = HistoricalState.fromBytes(data, currentOffset)
            currentOffset += preStateBytes
            val (postState, postStateBytes) = HistoricalState.fromBytes(data, currentOffset)
            currentOffset += postStateBytes
            return Pair(HistoricalCase(input, preState, postState), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray = input.encode() + preState.encode() + postState.encode()
}
