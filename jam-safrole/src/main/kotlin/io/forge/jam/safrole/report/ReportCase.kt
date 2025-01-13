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
}

