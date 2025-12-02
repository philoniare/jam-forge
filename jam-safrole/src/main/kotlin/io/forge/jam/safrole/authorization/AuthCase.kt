package io.forge.jam.safrole.authorization

import io.forge.jam.core.Encodable
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class AuthCase(
    val input: AuthInput,
    @SerialName("pre_state")
    val preState: AuthState,
    val output: AuthOutput? = null,
    @SerialName("post_state")
    val postState: AuthState
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, coresCount: Int): Pair<AuthCase, Int> {
            var currentOffset = offset

            val (input, inputBytes) = AuthInput.fromBytes(data, currentOffset)
            currentOffset += inputBytes

            val (preState, preStateBytes) = AuthState.fromBytes(data, currentOffset, coresCount)
            currentOffset += preStateBytes

            // output is always null in encoding (per encode method)
            val (postState, postStateBytes) = AuthState.fromBytes(data, currentOffset, coresCount)
            currentOffset += postStateBytes

            return Pair(AuthCase(input, preState, null, postState), currentOffset - offset)
        }
    }

    override fun encode(): ByteArray {
        val inputBytes = input.encode()
        val preStateBytes = preState.encode()
        // NULL output encodes to empty byte array
        val outputBytes = output?.encode() ?: ByteArray(0)
        val postStateBytes = postState.encode()
        return inputBytes + preStateBytes + outputBytes + postStateBytes
    }
}


