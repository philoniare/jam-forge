package io.forge.jam.safrole.report

import io.forge.jam.core.serializers.ByteArrayListHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class ReportOutputMarks(
    val reported: List<ReportPackage>,
    @Serializable(with = ByteArrayListHexSerializer::class)
    val reporters: List<ByteArray>
)
