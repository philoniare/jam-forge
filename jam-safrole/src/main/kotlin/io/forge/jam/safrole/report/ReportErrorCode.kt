package io.forge.jam.safrole.report

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
enum class ReportErrorCode {
    @SerialName("anchor_not_recent")
    ANCHOR_NOT_RECENT,
}
