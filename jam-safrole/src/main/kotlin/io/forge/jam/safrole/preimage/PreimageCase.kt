package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class PreimageCase(
    val input: PreimageInput,
    @SerialName("pre_state")
    val preState: PreimageState,
    val output: PreimageOutput,
    @SerialName("post_state")
    val postState: PreimageState
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0): Pair<PreimageCase, Int> {
            var currentOffset = offset
            val (input, inputBytes) = PreimageInput.fromBytes(data, currentOffset)
            currentOffset += inputBytes
            val (preState, preStateBytes) = PreimageState.fromBytes(data, currentOffset)
            currentOffset += preStateBytes
            val (output, outputBytes) = PreimageOutput.fromBytes(data, currentOffset)
            currentOffset += outputBytes
            val (postState, postStateBytes) = PreimageState.fromBytes(data, currentOffset)
            currentOffset += postStateBytes
            return Pair(PreimageCase(input, preState, output, postState), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val inputBytes = input.encode()
        val preStateBytes = preState.encode()
        val outputBytes = output.encode()
        val postStateBytes = postState.encode()
        return inputBytes + preStateBytes + outputBytes + postStateBytes
    }
}


