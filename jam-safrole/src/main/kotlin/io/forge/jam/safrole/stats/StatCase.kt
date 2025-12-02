package io.forge.jam.safrole.stats

import io.forge.jam.core.Encodable
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class StatCase(
    val input: StatInput,
    @SerialName("pre_state")
    val preState: StatState,
    val output: StatOutput? = null,
    @SerialName("post_state")
    val postState: StatState
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, coresCount: Int, validatorsCount: Int): Pair<StatCase, Int> {
            var currentOffset = offset

            val (input, inputBytes) = StatInput.fromBytes(data, currentOffset, coresCount, validatorsCount)
            currentOffset += inputBytes

            val (preState, preStateBytes) = StatState.fromBytes(data, currentOffset, validatorsCount)
            currentOffset += preStateBytes

            // output is always null in encoding (per encode method)
            val (postState, postStateBytes) = StatState.fromBytes(data, currentOffset, validatorsCount)
            currentOffset += postStateBytes

            return Pair(StatCase(input, preState, null, postState), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val inputBytes = input.encode()
        val preStateBytes = preState.encode()
        val postStateBytes = postState.encode()
        return inputBytes + preStateBytes + postStateBytes
    }
}


