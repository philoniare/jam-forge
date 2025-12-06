package io.forge.jam.safrole.traces

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.JamByteArrayList
import io.forge.jam.core.WorkReport
import io.forge.jam.safrole.*
import io.forge.jam.safrole.accumulation.AccumulationState
import io.forge.jam.safrole.accumulation.Privileges
import io.forge.jam.safrole.accumulation.ReadyRecord
import io.forge.jam.safrole.accumulation.ServiceStatisticsEntry
import io.forge.jam.safrole.assurance.AssuranceState
import io.forge.jam.safrole.authorization.AuthState
import io.forge.jam.safrole.historical.HistoricalBetaContainer
import io.forge.jam.safrole.historical.HistoricalState
import io.forge.jam.safrole.preimage.*
import io.forge.jam.safrole.report.*
import io.forge.jam.safrole.safrole.SafroleState
import io.forge.jam.safrole.stats.StatCount
import io.forge.jam.safrole.stats.StatState

/**
 * Unified JAM state container holding all state components.
 *
 * This is the comprehensive state structure that encompasses all 16+ state components
 * as defined in the Gray Paper. It provides conversion methods to individual STF state types.
 */
data class FullJamState(
    // τ - Current timeslot
    val timeslot: Long,

    // η - Entropy pool (4 x 32-byte hashes)
    val entropyPool: List<JamByteArray>,

    // κ - Current validators
    val currentValidators: List<ValidatorKey>,

    // λ - Previous validators
    val previousValidators: List<ValidatorKey>,

    // ι - Validator queue (pending validators)
    val validatorQueue: List<ValidatorKey>,

    // γ - Safrole state
    val safroleGammaK: List<ValidatorKey>,  // γk - next epoch validators
    val safroleGammaZ: JamByteArray,         // γz - ring root
    val safroleGammaS: TicketsOrKeys,        // γs - sealing sequence
    val safroleGammaA: List<TicketBody>,     // γa - ticket accumulator

    // φc - Core authorization pools
    val authPools: List<List<JamByteArray>>,

    // φ - Authorization queues
    val authQueues: List<List<JamByteArray>>,

    // β - Recent history
    val recentHistory: HistoricalBetaContainer,

    // ρ - Pending reports (availability assignments)
    val reports: List<AvailabilityAssignment?>,

    // ψ - Judgements
    val judgements: Psi,

    // χ - Privileged services
    val privilegedServices: Privileges,

    // Activity statistics (current and last epoch)
    val activityStatsCurrent: List<StatCount>,
    val activityStatsLast: List<StatCount>,

    // Core statistics (per core)
    val coreStatistics: List<CoreStatisticsRecord> = emptyList(),

    // Accumulation queue
    val accumulationQueue: List<List<ReadyRecord>>,

    // Accumulation history (historically accumulated hashes)
    val accumulationHistory: List<List<JamByteArray>>,

    // Last accumulation outputs - Set of (serviceIndex, hash) pairs allowing same service multiple times
    val lastAccumulationOutputs: Set<io.forge.jam.safrole.accumulation.Commitment>,

    // δ - Service accounts
    val serviceAccounts: List<AccumulationServiceItem>,

    // Service statistics
    val serviceStatistics: List<ServiceStatisticsEntry>,

    // Raw service data keyvals (storage, preimages, preimage info)
    // These are kept as raw keyvals because the state keys are hashed
    // and we cannot recover the original keys for re-encoding.
    val rawServiceDataKvs: List<KeyValue> = emptyList()
) {
    /**
     * Convert to SafroleState for Safrole STF.
     */
    fun toSafroleState(): SafroleState {
        val eta = JamByteArrayList()
        entropyPool.forEach { eta.add(it) }

        return SafroleState(
            tau = timeslot,
            eta = eta,
            lambda = previousValidators,
            kappa = currentValidators,
            gammaK = safroleGammaK,
            iota = validatorQueue,
            gammaA = safroleGammaA,
            gammaS = safroleGammaS,
            gammaZ = safroleGammaZ,
            psi = judgements,
            rho = reports.toMutableList()
        )
    }

    /**
     * Convert to AssuranceState for Assurance STF.
     */
    fun toAssuranceState(): AssuranceState {
        return AssuranceState(
            availAssignments = reports,
            currValidators = currentValidators
        )
    }

    /**
     * Convert to AuthState for Authorization STF.
     */
    fun toAuthState(): AuthState {
        return AuthState(
            authPools = authPools,
            authQueues = authQueues
        )
    }

    /**
     * Convert to HistoricalState for History STF.
     */
    fun toHistoryState(): HistoricalState {
        return HistoricalState(beta = recentHistory)
    }

    /**
     * Convert to ReportState for Report STF.
     */
    fun toReportState(): ReportState {
        // Convert AccumulationServiceItem to ServiceItem for ReportState
        val serviceItems = serviceAccounts.map { accItem ->
            ServiceItem(
                id = accItem.id,
                data = accItem.data.toServiceData()
            )
        }

        return ReportState(
            availAssignments = reports,
            currValidators = currentValidators,
            prevValidators = previousValidators,
            entropy = entropyPool,
            offenders = judgements.offenders.toList(),
            recentBlocks = recentHistory,
            authPools = authPools,
            accounts = serviceItems,
            coresStatistics = emptyList(), // Will be updated from state
            servicesStatistics = serviceStatistics
        )
    }

    /**
     * Convert to PreimageState for Preimage STF.
     */
    fun toPreimageState(): PreimageState {
        val accounts = serviceAccounts.map { accItem ->
            accItem.toPreimageAccount()
        }
        return PreimageState(accounts = accounts, statistics = serviceStatistics)
    }

    /**
     * Convert to StatState for Statistics STF.
     */
    fun toStatState(): StatState {
        return StatState(
            valsCurrStats = activityStatsCurrent,
            valsLastStats = activityStatsLast,
            slot = timeslot,
            currValidators = currentValidators
        )
    }

    /**
     * Convert to AccumulationState for Accumulation STF.
     */
    fun toAccumulationState(): AccumulationState {
        val entropy = if (entropyPool.isNotEmpty()) entropyPool[0] else JamByteArray(ByteArray(32))

        // Convert raw keyvals to map indexed by state key
        val rawServiceDataMap = rawServiceDataKvs.associate { kv ->
            kv.key to kv.value
        }.toMutableMap()

        return AccumulationState(
            slot = timeslot,
            entropy = entropy,
            readyQueue = accumulationQueue.map { it.toList() }.toMutableList(),
            accumulated = accumulationHistory.map { it.toList() }.toMutableList(),
            privileges = privilegedServices,
            statistics = serviceStatistics,
            accounts = serviceAccounts,
            rawServiceDataByStateKey = rawServiceDataMap
        )
    }

    /**
     * Get available reports (non-null assignments).
     */
    fun getAvailableReports(): List<WorkReport> {
        return reports.filterNotNull().map { it.report }
    }

    companion object {
        /**
         * Create an empty/default FullJamState.
         */
        fun empty(config: TinyConfig = TinyConfig): FullJamState {
            val emptyValidators = List(config.VALIDATORS_COUNT) {
                ValidatorKey(
                    JamByteArray(ByteArray(32)),
                    JamByteArray(ByteArray(32)),
                    JamByteArray(ByteArray(144)),
                    JamByteArray(ByteArray(128))
                )
            }
            val emptyEntropy = List(4) { JamByteArray(ByteArray(32)) }
            val emptyAuthPools = List(config.CORES_COUNT) { emptyList<JamByteArray>() }
            val emptyAuthQueues = List(config.CORES_COUNT) { List(80) { JamByteArray(ByteArray(32)) } }
            val emptyReports = List(config.CORES_COUNT) { null as AvailabilityAssignment? }
            val emptyAccQueue = List(config.EPOCH_LENGTH) { emptyList<ReadyRecord>() }
            val emptyAccHistory = List(config.EPOCH_LENGTH) { emptyList<JamByteArray>() }
            val emptyStats = List(config.VALIDATORS_COUNT) { StatCount() }

            return FullJamState(
                timeslot = 0,
                entropyPool = emptyEntropy,
                currentValidators = emptyValidators,
                previousValidators = emptyValidators,
                validatorQueue = emptyValidators,
                safroleGammaK = emptyValidators,
                safroleGammaZ = JamByteArray(ByteArray(144)),
                safroleGammaS = TicketsOrKeys(),
                safroleGammaA = emptyList(),
                authPools = emptyAuthPools,
                authQueues = emptyAuthQueues,
                recentHistory = HistoricalBetaContainer(),
                reports = emptyReports,
                judgements = Psi(),
                privilegedServices = Privileges(0, List(config.CORES_COUNT) { 0L }, 0, 0, emptyList()),
                activityStatsCurrent = emptyStats,
                activityStatsLast = emptyStats,
                accumulationQueue = emptyAccQueue,
                accumulationHistory = emptyAccHistory,
                lastAccumulationOutputs = emptySet(),
                serviceAccounts = emptyList(),
                serviceStatistics = emptyList()
            )
        }
    }
}

/**
 * Extension to convert AccumulationServiceData to ServiceData for ReportState.
 */
private fun AccumulationServiceData.toServiceData(): ServiceData {
    return ServiceData(service = this.service)
}

/**
 * Extension to convert AccumulationServiceItem to PreimageAccount.
 */
private fun AccumulationServiceItem.toPreimageAccount(): PreimageAccount {
    return PreimageAccount(
        id = this.id,
        data = AccountInfo(
            preimages = this.data.preimages,
            lookupMeta = this.data.preimagesStatus.map { status ->
                PreimageHistory(
                    key = PreimageHistoryKey(
                        hash = status.hash,
                        length = status.status.size.toLong()
                    ),
                    value = status.status
                )
            }
        )
    )
}
