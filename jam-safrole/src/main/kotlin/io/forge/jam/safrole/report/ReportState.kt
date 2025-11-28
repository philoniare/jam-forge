package io.forge.jam.safrole.report

import io.forge.jam.core.*
import io.forge.jam.core.serializers.ByteArrayNestedListSerializer
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import io.forge.jam.safrole.AvailabilityAssignment
import io.forge.jam.safrole.ValidatorKey
import io.forge.jam.safrole.historical.HistoricalBetaContainer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class ReportState(
    @SerialName("avail_assignments")
    var availAssignments: List<AvailabilityAssignment?>,

    @SerialName("curr_validators")
    val currValidators: List<ValidatorKey>,

    @SerialName("prev_validators")
    val prevValidators: List<ValidatorKey>,

    @Serializable(with = JamByteArrayListHexSerializer::class)
    val entropy: List<JamByteArray>,
    @Serializable(with = JamByteArrayListHexSerializer::class)
    val offenders: List<JamByteArray>,

    @SerialName("recent_blocks")
    val recentBlocks: HistoricalBetaContainer,
    @SerialName("auth_pools")
    @Serializable(with = ByteArrayNestedListSerializer::class)
    val authPools: List<List<JamByteArray>>,
    @SerialName("accounts")
    val accounts: List<ServiceItem>
) : Encodable {
    override fun encode(): ByteArray {
        val availAssignmentsBytes = encodeOptionalList(availAssignments, false)
        val currValidatorsBytes = encodeList(currValidators, false)
        val prevValidatorsBytes = encodeList(prevValidators, false)
        val entropyBytes = encodeList(entropy, false)
        val offenderBytes = encodeList(offenders)
        val recentBlocksBytes = encodeList(recentBlocks.history) + recentBlocks.mmr.encode()
        val authPoolBytes = encodeNestedList(authPools, includeLength = false)
        val accountsBytes = encodeList(accounts)
        return availAssignmentsBytes + currValidatorsBytes + prevValidatorsBytes + entropyBytes + offenderBytes + recentBlocksBytes + authPoolBytes + accountsBytes
    }

    fun deepCopy(): ReportState {
        return ReportState(
            availAssignments = availAssignments.map { it?.copy() },
            currValidators = currValidators.map { it.copy() },
            prevValidators = prevValidators.map { it.copy() },
            recentBlocks = HistoricalBetaContainer(
                history = recentBlocks.history.map { it.copy() },
                mmr = recentBlocks.mmr.copy()
            ),
            authPools = authPools.map { innerList ->
                innerList.map { it.clone() }
            },
            entropy = entropy.map { it.copy() },
            offenders = offenders.map({ it.copy() }),
            accounts = accounts.map { it.copy() }
        )
    }
}
