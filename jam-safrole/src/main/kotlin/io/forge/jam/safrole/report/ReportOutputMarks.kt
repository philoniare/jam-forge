package io.forge.jam.safrole.report

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import kotlinx.serialization.Serializable

@Serializable
data class ReportOutputMarks(
    val reported: List<ReportPackage>,
    @Serializable(with = JamByteArrayListHexSerializer::class)
    val reporters: List<JamByteArray>
)
