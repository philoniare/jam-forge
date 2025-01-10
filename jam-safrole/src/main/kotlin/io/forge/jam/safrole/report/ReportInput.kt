package io.forge.jam.safrole.report

import io.forge.jam.core.GuaranteeExtrinsic
import kotlinx.serialization.Serializable

@Serializable
data class ReportInput(
    val guarantees: List<GuaranteeExtrinsic>,
    val slot: Long? = null,
)
