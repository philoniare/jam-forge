package io.forge.jam.safrole.report

import io.forge.jam.core.*
import io.forge.jam.core.serializers.ByteArrayNestedListSerializer
import io.forge.jam.core.serializers.JamByteArrayListHexSerializer
import io.forge.jam.safrole.AvailabilityAssignment
import io.forge.jam.safrole.ValidatorKey
import io.forge.jam.safrole.accumulation.ServiceStatisticsEntry
import io.forge.jam.safrole.historical.HistoricalBeta
import io.forge.jam.safrole.historical.HistoricalBetaContainer
import io.forge.jam.safrole.historical.HistoricalMmr
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
    val accounts: List<ServiceItem>,
    @SerialName("cores_statistics")
    val coresStatistics: List<CoreStatisticsRecord> = emptyList(),
    @SerialName("services_statistics")
    val servicesStatistics: List<ServiceStatisticsEntry> = emptyList()
) : Encodable {
    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, coresCount: Int, validatorsCount: Int): Pair<ReportState, Int> {
            var currentOffset = offset

            // availAssignments - fixed size (coresCount), optional items
            val availAssignments = mutableListOf<AvailabilityAssignment?>()
            for (i in 0 until coresCount) {
                val optionByte = data[currentOffset].toInt() and 0xFF
                currentOffset += 1
                if (optionByte == 0) {
                    availAssignments.add(null)
                } else {
                    val (assignment, assignmentBytes) = AvailabilityAssignment.fromBytes(data, currentOffset)
                    currentOffset += assignmentBytes
                    availAssignments.add(assignment)
                }
            }

            // currValidators - fixed size (validatorsCount)
            val currValidators = mutableListOf<ValidatorKey>()
            for (i in 0 until validatorsCount) {
                currValidators.add(ValidatorKey.fromBytes(data, currentOffset))
                currentOffset += ValidatorKey.SIZE
            }

            // prevValidators - fixed size (validatorsCount)
            val prevValidators = mutableListOf<ValidatorKey>()
            for (i in 0 until validatorsCount) {
                prevValidators.add(ValidatorKey.fromBytes(data, currentOffset))
                currentOffset += ValidatorKey.SIZE
            }

            // entropy - fixed size (4 items of 32 bytes each)
            val entropy = mutableListOf<JamByteArray>()
            for (i in 0 until 4) {
                entropy.add(JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32)))
                currentOffset += 32
            }

            // offenders - variable size list of 32-byte hashes
            val (offendersLength, offendersLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += offendersLengthBytes
            val offenders = mutableListOf<JamByteArray>()
            for (i in 0 until offendersLength.toInt()) {
                offenders.add(JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32)))
                currentOffset += 32
            }

            // recentBlocks - history list (variable) + mmr
            val (historyLength, historyLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += historyLengthBytes
            val history = mutableListOf<HistoricalBeta>()
            for (i in 0 until historyLength.toInt()) {
                val (beta, betaBytes) = HistoricalBeta.fromBytes(data, currentOffset)
                history.add(beta)
                currentOffset += betaBytes
            }
            val (mmr, mmrBytes) = HistoricalMmr.fromBytes(data, currentOffset)
            currentOffset += mmrBytes
            val recentBlocks = HistoricalBetaContainer(history, mmr)

            // authPools - fixed size outer (coresCount), variable inner lists of 32-byte hashes
            val authPools = mutableListOf<List<JamByteArray>>()
            for (i in 0 until coresCount) {
                val (innerLength, innerLengthBytes) = decodeCompactInteger(data, currentOffset)
                currentOffset += innerLengthBytes
                val innerList = mutableListOf<JamByteArray>()
                for (j in 0 until innerLength.toInt()) {
                    innerList.add(JamByteArray(data.copyOfRange(currentOffset, currentOffset + 32)))
                    currentOffset += 32
                }
                authPools.add(innerList)
            }

            // accounts - variable size list
            val (accountsLength, accountsLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += accountsLengthBytes
            val accounts = mutableListOf<ServiceItem>()
            for (i in 0 until accountsLength.toInt()) {
                accounts.add(ServiceItem.fromBytes(data, currentOffset))
                currentOffset += ServiceItem.SIZE
            }

            // coresStatistics - fixed size (coresCount), variable-size items
            val coresStatistics = mutableListOf<CoreStatisticsRecord>()
            for (i in 0 until coresCount) {
                val (record, recordBytes) = CoreStatisticsRecord.fromBytes(data, currentOffset)
                coresStatistics.add(record)
                currentOffset += recordBytes
            }

            // servicesStatistics - variable size list
            val (servicesStatsLength, servicesStatsLengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += servicesStatsLengthBytes
            val servicesStatistics = mutableListOf<ServiceStatisticsEntry>()
            for (i in 0 until servicesStatsLength.toInt()) {
                val (entry, entryBytes) = ServiceStatisticsEntry.fromBytes(data, currentOffset)
                servicesStatistics.add(entry)
                currentOffset += entryBytes
            }

            return Pair(
                ReportState(
                    availAssignments, currValidators, prevValidators, entropy, offenders,
                    recentBlocks, authPools, accounts, coresStatistics, servicesStatistics
                ),
                currentOffset - offset
            )
        }
    }

    override fun encode(): ByteArray {
        val availAssignmentsBytes = encodeOptionalList(availAssignments, false)
        val currValidatorsBytes = encodeList(currValidators, false)
        val prevValidatorsBytes = encodeList(prevValidators, false)
        val entropyBytes = encodeList(entropy, false)
        val offenderBytes = encodeList(offenders)
        val recentBlocksBytes = encodeList(recentBlocks.history) + recentBlocks.mmr.encode()
        val authPoolBytes = encodeNestedList(authPools, includeLength = false)
        val accountsBytes = encodeList(accounts)
        val coresStatsBytes = encodeList(coresStatistics, false)
        val servicesStatsBytes = encodeList(servicesStatistics)
        return availAssignmentsBytes + currValidatorsBytes + prevValidatorsBytes + entropyBytes + offenderBytes + recentBlocksBytes + authPoolBytes + accountsBytes + coresStatsBytes + servicesStatsBytes
    }

    fun encodeDebug(): Map<String, Int> {
        val availAssignmentsBytes = encodeOptionalList(availAssignments, false)
        val currValidatorsBytes = encodeList(currValidators, false)
        val prevValidatorsBytes = encodeList(prevValidators, false)
        val entropyBytes = encodeList(entropy, false)
        val offenderBytes = encodeList(offenders)
        val historyBytes = encodeList(recentBlocks.history)
        val mmrBytes = recentBlocks.mmr.encode()
        val authPoolBytes = encodeNestedList(authPools, includeLength = false)
        val accountsBytes = encodeList(accounts)
        val coresStatsBytes = encodeList(coresStatistics, false)
        val servicesStatsBytes = encodeList(servicesStatistics)
        return mapOf(
            "availAssignments" to availAssignmentsBytes.size,
            "currValidators" to currValidatorsBytes.size,
            "prevValidators" to prevValidatorsBytes.size,
            "entropy" to entropyBytes.size,
            "offenders" to offenderBytes.size,
            "history" to historyBytes.size,
            "mmr" to mmrBytes.size,
            "authPools" to authPoolBytes.size,
            "accounts" to accountsBytes.size,
            "coresStats" to coresStatsBytes.size,
            "servicesStats" to servicesStatsBytes.size,
            "total" to encode().size
        )
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
            accounts = accounts.map { it.copy() },
            coresStatistics = coresStatistics.map { it.copy() },
            servicesStatistics = servicesStatistics.map { it.copy() }
        )
    }
}
