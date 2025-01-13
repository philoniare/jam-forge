package io.forge.jam.safrole.report

import io.forge.jam.core.*
import io.forge.jam.core.serializers.ByteArrayNestedListSerializer
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
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

    @Serializable(with = JamByteArrayListHexSerializer::class)
    val entropy: List<JamByteArray>,
    @Serializable(with = JamByteArrayListHexSerializer::class)
    val offenders: List<JamByteArray>,

    @SerialName("recent_blocks")
    val recentBlocks: List<HistoricalBeta>,
    @SerialName("auth_pools")
    @Serializable(with = ByteArrayNestedListSerializer::class)
    val authPools: List<List<JamByteArray>>,
    @SerialName("services")
    val services: List<ServiceItem>
) : Encodable {
    override fun encode(): ByteArray {
        val availAssignmentsBytes = encodeOptionalList(availAssignments, false)
        val currValidatorsBytes = encodeList(currValidators, false)
        val prevValidatorsBytes = encodeList(prevValidators, false)
        val entropyBytes = encodeList(entropy, false)
        val offenderBytes = encodeList(offenders)
        val recentBlocksBytes = encodeList(recentBlocks)
        val authPoolBytes = encodeNestedList(authPools, includeLength = false)
        val servicesBytes = encodeList(services)
        return availAssignmentsBytes + currValidatorsBytes + prevValidatorsBytes + entropyBytes + offenderBytes + recentBlocksBytes + authPoolBytes + servicesBytes
    }

    fun deepCopy(): ReportState {
        return ReportState(
            availAssignments = availAssignments.map { it?.copy() },
            currValidators = currValidators.map { it.copy() },
            prevValidators = prevValidators.map { it.copy() },
            recentBlocks = recentBlocks.map { it.copy() },
            authPools = authPools.map { innerList ->
                innerList.map { it.clone() }
            },
            entropy = entropy.map { it.copy() },
            offenders = offenders.map({ it.copy() }),
            services = services.map { it.copy() }
        )
    }
}
