package io.forge.jam.safrole.safrole

import io.forge.jam.core.Encodable
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class SafroleCase(
    val input: SafroleInput,
    @SerialName("pre_state")
    val preState: SafroleState,
    val output: SafroleOutput,
    @SerialName("post_state")
    val postState: SafroleState
) : Encodable {
    override fun encode(): ByteArray {
        val inputBytes = input.encode()
        val preStateBytes = preState.encode()
        val outputBytes = output.encode()
        println("output: ${outputBytes.toList()}")
        val postStateBytes = postState.encode()
        println("postState: ${postStateBytes.toList()}")
        return inputBytes + preStateBytes + outputBytes + postStateBytes
    }
}
