package io.forge.jam.safrole.report

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
)

