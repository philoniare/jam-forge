package io.forge.jam.safrole.report

import io.forge.jam.core.JamByteArray
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
    val authPools: List<List<JamByteArray>>,
    @SerialName("services")
    @Serializable(with = ServiceListSerializer::class)
    val services: List<Pair<Long, Service>>
) {
    fun deepCopy(): ReportState {
        return ReportState(
            availAssignments = availAssignments.map { it?.copy() },
            currValidators = currValidators.map { it.copy() },
            prevValidators = prevValidators.map { it.copy() },
            recentBlocks = recentBlocks.map { it.copy() },
            authPools = authPools.map { innerList ->
                innerList.map { it.clone() }
            },
            services = services.map { (id, service) ->
                Pair(id, service.copy())
            }
        )
    }
}
