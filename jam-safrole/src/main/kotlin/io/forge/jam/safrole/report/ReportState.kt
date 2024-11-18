package io.forge.jam.safrole.report

import io.forge.jam.core.serializers.ByteArrayNestedListSerializer
import io.forge.jam.safrole.AvailabilityAssignment
import io.forge.jam.safrole.ValidatorKey
import io.forge.jam.safrole.historical.HistoricalBeta
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class ReportState(
    @SerialName("avail_assignments")
    val availAssignments: List<AvailabilityAssignment?>,
    @SerialName("curr_validators")
    val currValidators: List<ValidatorKey>,
    @SerialName("prev_validators")
    val prevValidators: List<ValidatorKey>,
    @SerialName("recent_blocks")
    val recentBlocks: List<HistoricalBeta>,
    @SerialName("auth_pools")
    @Serializable(with = ByteArrayNestedListSerializer::class)
    val authPools: List<List<ByteArray>>,
    @SerialName("services")
    @Serializable(with = ServiceListSerializer::class)
    val services: List<Pair<Long, Service>>
)
