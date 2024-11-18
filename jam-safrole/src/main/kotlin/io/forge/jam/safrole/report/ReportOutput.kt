package io.forge.jam.safrole.report

import io.forge.jam.safrole.OutputMarks
import kotlinx.serialization.Serializable

@Serializable
data class ReportOutput(
    val ok: OutputMarks? = null,
    @Serializable(with = ReportErrorCodeSerializer::class)
    val err: ReportErrorCode? = null
)
