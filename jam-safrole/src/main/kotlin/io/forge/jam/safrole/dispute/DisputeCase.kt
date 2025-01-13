package io.forge.jam.safrole.dispute

import io.forge.jam.core.Encodable
import io.forge.jam.safrole.safrole.DisputeInput
import io.forge.jam.safrole.safrole.DisputeState
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class DisputeCase(
    val input: DisputeInput,
    @SerialName("pre_state")
    val preState: DisputeState,
    val output: DisputeOutput,
    @SerialName("post_state")
    val postState: DisputeState
) : Encodable {
    override fun encode(): ByteArray {
        val inputBytes = input.encode()
        val preStateBytes = preState.encode()
        val outputBytes = output.encode()
        val postStateBytes = postState.encode()
        return inputBytes + preStateBytes + outputBytes + postStateBytes
    }
}

