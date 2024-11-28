package io.forge.jam.safrole.report

import io.forge.jam.core.GuaranteeExtrinsic
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class ReportInput(
    val guarantees: List<GuaranteeExtrinsic>,
    val slot: Long? = null,

    @Serializable(with = JamByteArrayListHexSerializer::class)
    val entropy: List<JamByteArray>,

    @Serializable(with = JamByteArrayListHexSerializer::class)
    val offenders: List<JamByteArray>? = null,
)
