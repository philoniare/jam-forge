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
    override fun encode(): ByteArray {
        val inputBytes = input.encode()
        val preStateBytes = preState.encode()
        val outputBytes = byteArrayOf(0)
        val postStateBytes = postState.encode()
        return inputBytes + preStateBytes + outputBytes + postStateBytes
    }
}


