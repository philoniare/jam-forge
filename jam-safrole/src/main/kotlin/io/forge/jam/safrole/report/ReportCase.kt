package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class ReportCase(
    val input: ReportInput,
    @SerialName("pre_state")
    val preState: ReportState,
    val output: ReportOutput,
    @SerialName("post_state")
    val postState: ReportState
) : Encodable {
    override fun encode(): ByteArray {
        val inputBytes = input.encode()
        val preStateBytes = preState.encode()
        val outputBytes = output.encode()
        val postStateBytes = postState.encode()
        return inputBytes + preStateBytes + outputBytes + postStateBytes
    }

    fun encodeDebug(): Map<String, Int> {
        return mapOf(
            "input" to input.encode().size,
            "preState" to preState.encode().size,
            "output" to output.encode().size,
            "postState" to postState.encode().size,
            "total" to encode().size
        )
    }
}

