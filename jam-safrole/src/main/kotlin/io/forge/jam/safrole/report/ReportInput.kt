package io.forge.jam.safrole.report

import io.forge.jam.core.GuaranteeExtrinsic
import io.forge.jam.core.serializers.ByteArrayListHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class ReportInput(
    val guarantees: List<GuaranteeExtrinsic>,
    val slot: Long? = null,

    @Serializable(with = ByteArrayListHexSerializer::class)
    val entropy: List<ByteArray>,

    @Serializable(with = ByteArrayListHexSerializer::class)
    val offenders: List<ByteArray>? = null,
)
