package io.forge.jam.safrole

import io.forge.jam.core.serializers.ByteArrayHexSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class AvailabilityAssignment(
    @SerialName("dummy_work_report")
    @Serializable(with = ByteArrayHexSerializer::class)
    val dummyWorkReport: ByteArray,

    val timeout: Long
)
