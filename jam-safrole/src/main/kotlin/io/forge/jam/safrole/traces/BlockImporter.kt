package io.forge.jam.safrole.traces

import io.forge.jam.core.*
import io.forge.jam.safrole.accumulation.*
import io.forge.jam.safrole.assurance.AssuranceConfig
import io.forge.jam.safrole.assurance.AssuranceState
import io.forge.jam.safrole.assurance.AssuranceStateTransition
import io.forge.jam.safrole.authorization.AuthConfig
import io.forge.jam.safrole.authorization.AuthState
import io.forge.jam.safrole.authorization.AuthorizationStateTransition
import io.forge.jam.safrole.historical.HistoricalBetaContainer
import io.forge.jam.safrole.historical.HistoricalState
import io.forge.jam.safrole.historical.HistoryTransition
import io.forge.jam.safrole.preimage.PreimageOutput
import io.forge.jam.safrole.preimage.PreimageState
import io.forge.jam.safrole.preimage.PreimageStateTransition
import io.forge.jam.safrole.report.*
import io.forge.jam.safrole.safrole.SafroleConfig
import io.forge.jam.safrole.safrole.SafroleState
import io.forge.jam.safrole.safrole.SafroleStateTransition
import io.forge.jam.safrole.stats.StatConfig
import io.forge.jam.safrole.stats.StatState
import io.forge.jam.safrole.stats.StatStateTransition

/**
 * Result of a block import operation.
 */
sealed class ImportResult {
    data class Success(
        val postState: RawState,
        val computedFullState: FullJamState,
        val safroleState: SafroleState? = null
    ) : ImportResult()

    data class Failure(val error: ImportError, val message: String = "") : ImportResult()
}

/**
 * Errors that can occur during block import.
 */
enum class ImportError {
    INVALID_HEADER,
    INVALID_PARENT,
    INVALID_SLOT,
    INVALID_STATE_ROOT,
    SAFROLE_ERROR,
    ASSURANCE_ERROR,
    AUTHORIZATION_ERROR,
    DISPUTE_ERROR,
    HISTORY_ERROR,
    PREIMAGE_ERROR,
    REPORT_ERROR,
    STATISTICS_ERROR,
    ACCUMULATION_ERROR,
    UNKNOWN_ERROR
}

/**
 * Configuration for the BlockImporter.
 */
data class ImporterConfig(
    val validatorsCount: Int = TinyConfig.VALIDATORS_COUNT,
    val epochLength: Int = TinyConfig.EPOCH_LENGTH,
    val coresCount: Int = TinyConfig.CORES_COUNT,
    val ticketCutoff: Int = TinyConfig.TICKET_CUTOFF,
    val ringSize: Int = TinyConfig.RING_SIZE,
    val maxTicketAttempts: Int = TinyConfig.MAX_TICKET_ATTEMPTS,
    val preimageExpungeDelay: Int = TinyConfig.PREIMAGE_EXPUNGE_DELAY,
    val rotationPeriod: Int = TinyConfig.CONFIG.rotationPeriod
) {
    fun toSafroleConfig(): SafroleConfig = SafroleConfig(
        validatorsCount = validatorsCount,
        epochLength = epochLength.toLong(),
        coresCount = coresCount,
        ticketCutoff = ticketCutoff.toLong(),
        ringSize = ringSize,
        maxTicketAttempts = maxTicketAttempts
    )

    fun toAuthConfig(): AuthConfig = AuthConfig(
        CORE_COUNT = coresCount.toLong()
    )

    fun toStatConfig(): StatConfig = StatConfig(
        EPOCH_LENGTH = epochLength.toLong()
    )

    fun toAccumulationConfig(): AccumulationConfig = AccumulationConfig(
        EPOCH_LENGTH = epochLength,
        MAX_BLOCK_HISTORY = 8,
        AUTH_QUEUE_SIZE = 80
    )

    fun toAssuranceConfig(): AssuranceConfig = AssuranceConfig(
        VALIDATOR_COUNT = validatorsCount.toLong(),
        CORE_COUNT = coresCount.toLong()
    )

    fun toReportConfig(): ReportStateConfig = ReportStateConfig(
        MAX_LOOKUP_ANCHOR_AGE = 8L, // Configurable
        MAX_DEPENDENCIES = 8L,
        MAX_ACCUMULATION_GAS = 10_000_000L,
        MAX_CORES = coresCount,
        ROTATION_PERIOD = rotationPeriod.toLong(),
        MAX_VALIDATORS = validatorsCount,
        EPOCH_LENGTH = epochLength
    )
}

/**
 * BlockImporter handles importing blocks and applying all state transitions.
 * This follows the Gray Paper specification for block import order:
 * 1. Safrole - Block production and VRF validation (includes Disputes)
 * 2. Assurances - Process availability assurances
 * 3. Reports - Process work reports (guarantees)
 * 4. Accumulation - Execute PVM accumulation
 * 5. History - Update recent blocks history
 * 6. Authorizations - Update authorization pools
 * 7. Preimages - Handle preimage provisioning
 * 8. Statistics - Update chain statistics
 */
class BlockImporter(private val config: ImporterConfig = ImporterConfig()) {

    private val safroleSTF = SafroleStateTransition(config.toSafroleConfig())
    private val assuranceSTF = AssuranceStateTransition(config.toAssuranceConfig())
    private val reportSTF = ReportStateTransition(config.toReportConfig())
    private val authSTF = AuthorizationStateTransition(config.toAuthConfig())
    private val historySTF = HistoryTransition()
    private val preimageSTF = PreimageStateTransition()
    private val statSTF = StatStateTransition(config.toStatConfig())
    private val accumulationSTF = AccumulationStateTransition(config.toAccumulationConfig())

    /**
     * Imports a block and applies all state transitions.
     * @param block The block to import
     * @param preState The state before the block (raw keyvals)
     * @return ImportResult indicating success with new state or failure with error
     */
    fun importBlock(block: Block, preState: RawState): ImportResult {
        try {
            // Step 1: Decode full pre-state from keyvals
            val fullPreState = StateCodec.decodeFullState(preState.keyvals)

            // Step 2: Run Safrole STF (includes Disputes)
            val safroleInput = InputExtractor.extractSafroleInput(block, fullPreState)
            val safrolePreState = fullPreState.toSafroleState()
            val (safrolePostState, safroleOutput) = safroleSTF.transition(safroleInput, safrolePreState)

            if (safroleOutput.err != null) {
                return ImportResult.Failure(
                    ImportError.SAFROLE_ERROR,
                    "Safrole STF error: ${safroleOutput.err}"
                )
            }

            // Step 3: Run Assurances STF
            val assuranceInput = InputExtractor.extractAssuranceInput(block, fullPreState)
            val assurancePreState = AssuranceState(
                availAssignments = fullPreState.reports,
                currValidators = safrolePostState.kappa
            )
            val (assurancePostState, assuranceOutput) = assuranceSTF.transition(assuranceInput, assurancePreState)

            if (assuranceOutput.err != null) {
                return ImportResult.Failure(
                    ImportError.ASSURANCE_ERROR,
                    "Assurance STF error: ${assuranceOutput.err}"
                )
            }

            // Get available reports from assurances
            val availableReports = assuranceOutput.ok?.reported ?: emptyList()
            if (availableReports.isNotEmpty() || block.extrinsic.guarantees.isNotEmpty()) {
                println("[DEBUG] Block ${block.header.slot}: ${availableReports.size} available reports, ${block.extrinsic.guarantees.size} guarantees")
            }

            // Step 4: Run Reports STF
            val updatedRecentHistory = updateRecentHistoryPartial(
                fullPreState.recentHistory,
                block.header.parentStateRoot
            )
            val reportInput = ReportInput(
                guarantees = block.extrinsic.guarantees,
                slot = block.header.slot
            )
            val reportPreState =
                buildReportState(fullPreState, safrolePostState, assurancePostState, updatedRecentHistory)
            val (reportPostState, reportOutput) = reportSTF.transition(reportInput, reportPreState)

            if (reportOutput.err != null) {
                return ImportResult.Failure(
                    ImportError.REPORT_ERROR,
                    "Report STF error: ${reportOutput.err}"
                )
            }

            // Step 5: Run Accumulation STF
            // Pass the updated service statistics from Reports STF to Accumulation STF
            val accumulationInput = InputExtractor.extractAccumulationInput(availableReports, block.header.slot)
            val baseAccumulationState = fullPreState.toAccumulationState()
            // Use service statistics from Reports STF (which includes guarantee processing updates)
            val postSafroleEntropy = if (safrolePostState.eta.isNotEmpty()) {
                safrolePostState.eta[0]
            } else {
                JamByteArray(ByteArray(32))
            }
            if (block.header.slot == 6L) {
                println(
                    "[DEBUG-ENTROPY] Block 6: preState.entropy[0]=${
                        fullPreState.entropyPool.getOrNull(0)?.toHex()
                    }"
                )
                println("[DEBUG-ENTROPY] Block 6: postSafrole.eta[0]=${postSafroleEntropy.toHex()}")
            }
            val accumulationPreState = baseAccumulationState.copy(
                statistics = reportPostState.servicesStatistics,
                entropy = postSafroleEntropy
            )
            val (accumulationPostState, accumulationOutput) = accumulationSTF.transition(
                accumulationInput,
                accumulationPreState
            )

            val accumulateRoot = accumulationOutput.ok ?: JamByteArray(ByteArray(32))
            println("[DEBUG-HISTORY] Block ${block.header.slot}: accumulateRoot=${accumulateRoot.toHex()}")

            // Step 6: Run History STF
            val historyInput = InputExtractor.extractHistoryInput(block, accumulateRoot)
            val historyPreState = fullPreState.toHistoryState()
            val historyPostState = historySTF.stf(historyInput, historyPreState)

            // Step 7: Run Authorization STF
            val authInput = InputExtractor.extractAuthInputFromBlock(block, fullPreState)
            val authPreState = fullPreState.toAuthState()
            val (authPostState, _) = authSTF.transition(authInput, authPreState)

            // Step 8: Run Preimages STF
            val rawServiceDataMap = fullPreState.rawServiceDataKvs.associate { kv ->
                kv.key to kv.value
            }
            val preimageInput = InputExtractor.extractPreimageInput(block, block.header.slot, rawServiceDataMap)
            val preimagePreState = fullPreState.toPreimageState()

            val (preimagePostState, preimageOutput) = preimageSTF.transition(preimageInput, preimagePreState)

            // Step 9: Run Statistics STF
            val statInput = InputExtractor.extractStatInput(block, fullPreState)
            val statPreState = fullPreState.toStatState()
            val (statPostState, _) = statSTF.transition(statInput, statPreState)

            // Step 10: Compute final core statistics (combines guarantees, available reports, assurances)
            val finalCoreStats = computeFinalCoreStatistics(
                guaranteeStats = reportPostState.coresStatistics,
                availableReports = availableReports,
                assurances = block.extrinsic.assurances,
                maxCores = config.coresCount
            )

            // Step 11: Compute final service statistics (fresh each block)
            val finalServiceStats = computeFinalServiceStatistics(
                guarantees = block.extrinsic.guarantees,
                preimages = block.extrinsic.preimages,
                accumulationStats = accumulationOutput.accumulationStats
            )

            // Step 12: Merge all post-states into unified FullJamState
            val mergedState = mergePostStates(
                preState = fullPreState,
                safrolePost = safrolePostState,
                assurancePost = assurancePostState,
                reportPost = reportPostState,
                accumulationPost = accumulationPostState,
                accumulationOutput = accumulationOutput,
                historyPost = historyPostState,
                authPost = authPostState,
                preimagePost = preimagePostState,
                preimageOutput = preimageOutput,
                statPost = statPostState,
                finalCoreStats = finalCoreStats,
                finalServiceStats = finalServiceStats
            )

            // Step 11: Encode merged state back to keyvals
            val postKeyvals = StateEncoder.encodeFullState(mergedState)

            // Step 12: Compute state root via Merkle trie
            val stateRoot = StateMerklization.stateMerklize(postKeyvals)

            val rawPostState = RawState(stateRoot, postKeyvals)

            return ImportResult.Success(rawPostState, mergedState, safrolePostState)
        } catch (e: Exception) {
            e.printStackTrace()
            return ImportResult.Failure(ImportError.UNKNOWN_ERROR, e.message ?: "Unknown error")
        }
    }

    /**
     * Compute final core statistics by combining:
     * 1. Guarantee-based stats (bundleSize, gasUsed, extrinsicCount, etc.) from Reports STF
     * 2. dataSize from available reports
     * 3. assuranceCount/popularity from assurance extrinsics
     */
    private fun computeFinalCoreStatistics(
        guaranteeStats: List<CoreStatisticsRecord>,
        availableReports: List<WorkReport>,
        assurances: List<AssuranceExtrinsic>,
        maxCores: Int
    ): List<CoreStatisticsRecord> {
        // Start with guarantee-based stats (or fresh zeros if empty)
        val coreStats = if (guaranteeStats.size >= maxCores) {
            guaranteeStats.toMutableList()
        } else {
            val list = guaranteeStats.toMutableList()
            while (list.size < maxCores) {
                list.add(CoreStatisticsRecord())
            }
            list
        }

        // Add dataSize from available reports
        // dataSize = package.length + segmentsSize
        // segmentsSize = segmentSize * ceil((segmentCount * 65) / 64)
        val segmentSize = 4104L // Default segment size for tiny config
        for (report in availableReports) {
            val coreIndex = report.coreIndex.toInt()
            if (coreIndex >= 0 && coreIndex < coreStats.size) {
                val packageLength = report.packageSpec.length.toLong()
                // segmentCount comes from work results exports
                val segmentCount = report.results.sumOf { it.refineLoad.exports }
                val segmentsSize = segmentSize * ((segmentCount * 65 + 63) / 64)
                val dataSize = packageLength + segmentsSize

                val current = coreStats[coreIndex]
                coreStats[coreIndex] = current.copy(
                    daLoad = current.daLoad + dataSize
                )
            }
        }

        // Add assuranceCount/popularity from assurances
        // Each assurance has a bitfield indicating which cores the validator attests to
        for (assurance in assurances) {
            val bitfield = assurance.bitfield.bytes
            for (coreIndex in 0 until maxCores) {
                // Check if bit at coreIndex is set in the bitfield
                val byteIndex = coreIndex / 8
                val bitIndex = coreIndex % 8
                if (byteIndex < bitfield.size) {
                    val attested = (bitfield[byteIndex].toInt() and (1 shl bitIndex)) != 0
                    if (attested && coreIndex < coreStats.size) {
                        val current = coreStats[coreIndex]
                        coreStats[coreIndex] = current.copy(
                            popularity = current.popularity + 1
                        )
                    }
                }
            }
        }

        return coreStats
    }

    /**
     * Compute fresh service statistics by combining:
     * 1. Work reports from guarantees (refinementCount, gasUsed, imports, exports, extrinsicCount, extrinsicSize)
     * 2. Preimages (providedCount, providedSize)
     * 3. Accumulation results (accumulateCount, accumulateGasUsed)
     */
    private fun computeFinalServiceStatistics(
        guarantees: List<GuaranteeExtrinsic>,
        preimages: List<Preimage>,
        accumulationStats: Map<Long, Pair<Long, Int>>  // serviceId -> (gasUsed, workItemCount)
    ): List<ServiceStatisticsEntry> {
        // Collect all service IDs from all sources
        val serviceIds = mutableSetOf<Long>()

        // From guarantees
        for (guarantee in guarantees) {
            for (result in guarantee.report.results) {
                serviceIds.add(result.serviceId)
            }
        }
        // From preimages
        for (preimage in preimages) {
            serviceIds.add(preimage.requester)
        }
        // From accumulation
        serviceIds.addAll(accumulationStats.keys)

        // Initialize fresh stats for each service
        val statsMap = mutableMapOf<Long, ServiceActivityRecord>()
        for (serviceId in serviceIds) {
            statsMap[serviceId] = ServiceActivityRecord()
        }

        // Update from guarantees (work reports)
        for (guarantee in guarantees) {
            for (result in guarantee.report.results) {
                val serviceId = result.serviceId
                val current = statsMap[serviceId] ?: ServiceActivityRecord()
                val refineLoad = result.refineLoad
                statsMap[serviceId] = current.copy(
                    refinementCount = current.refinementCount + 1L,
                    refinementGasUsed = current.refinementGasUsed + refineLoad.gasUsed.toLong(),
                    imports = current.imports + refineLoad.imports.toLong(),
                    exports = current.exports + refineLoad.exports.toLong(),
                    extrinsicCount = current.extrinsicCount + refineLoad.extrinsicCount.toLong(),
                    extrinsicSize = current.extrinsicSize + refineLoad.extrinsicSize.toLong()
                )
            }
        }

        // Update from preimages
        for (preimage in preimages) {
            val serviceId = preimage.requester
            val current = statsMap[serviceId] ?: ServiceActivityRecord()
            statsMap[serviceId] = current.copy(
                providedCount = current.providedCount + 1,
                providedSize = current.providedSize + preimage.blob.size
            )
        }

        // Update from accumulation
        for ((serviceId, pair) in accumulationStats) {
            val (gasUsed, workItemCount) = pair
            val current = statsMap[serviceId] ?: ServiceActivityRecord()
            statsMap[serviceId] = current.copy(
                accumulateCount = current.accumulateCount + workItemCount,
                accumulateGasUsed = current.accumulateGasUsed + gasUsed
            )
        }

        // Return sorted list by service ID
        val result = statsMap.entries.map { (id, record) ->
            ServiceStatisticsEntry(id = id, record = record)
        }.sortedBy { it.id }

        // Debug: print computed service stats
        if (result.isNotEmpty()) {
            println("[DEBUG-SERVICE-STATS] computed: ${result.map { "${it.id}: pc=${it.record.providedCount}, ps=${it.record.providedSize}, rc=${it.record.refinementCount}, rgu=${it.record.refinementGasUsed}, imp=${it.record.imports}, ec=${it.record.extrinsicCount}, es=${it.record.extrinsicSize}, exp=${it.record.exports}, ac=${it.record.accumulateCount}, agu=${it.record.accumulateGasUsed}" }}")
        }
        return result
    }

    /**
     * Merge all STF post-states into a unified FullJamState.
     */
    private fun mergePostStates(
        preState: FullJamState,
        safrolePost: SafroleState,
        assurancePost: AssuranceState,
        reportPost: ReportState,
        accumulationPost: AccumulationState,
        accumulationOutput: AccumulationOutput,
        historyPost: HistoricalState,
        authPost: AuthState,
        preimagePost: PreimageState,
        preimageOutput: PreimageOutput,
        statPost: StatState,
        finalCoreStats: List<CoreStatisticsRecord>,
        finalServiceStats: List<ServiceStatisticsEntry>
    ): FullJamState {
        // Merge judgements from Safrole (which includes Disputes)
        val judgements = safrolePost.psi ?: preState.judgements

        // Merge service accounts from accumulation and preimage
        val serviceAccounts = mergeServiceAccounts(
            accumulationPost.accounts,
            preimagePost.accounts
        )

        return FullJamState(
            // From Safrole
            timeslot = safrolePost.tau,
            entropyPool = safrolePost.eta.toList(),
            currentValidators = safrolePost.kappa,
            previousValidators = safrolePost.lambda,
            validatorQueue = safrolePost.iota,
            safroleGammaK = safrolePost.gammaK,
            safroleGammaZ = safrolePost.gammaZ,
            safroleGammaS = safrolePost.gammaS,
            safroleGammaA = safrolePost.gammaA,

            // From Authorization
            authPools = authPost.authPools,
            authQueues = authPost.authQueues,

            // From History
            recentHistory = historyPost.beta,

            // From Reports STF (updates availAssignments with new guarantees)
            reports = reportPost.availAssignments,

            // From Safrole (Disputes)
            judgements = judgements,

            // From Accumulation
            privilegedServices = accumulationPost.privileges,
            accumulationQueue = accumulationPost.readyQueue,
            accumulationHistory = accumulationPost.accumulated,
            lastAccumulationOutputs = accumulationOutput.outputs,

            // Service accounts merged
            serviceAccounts = serviceAccounts,
            // Service statistics computed fresh each block
            serviceStatistics = finalServiceStats,

            // From Statistics
            activityStatsCurrent = statPost.valsCurrStats,
            activityStatsLast = statPost.valsLastStats,

            // Core statistics computed from guarantees, available reports, and assurances
            coreStatistics = finalCoreStats,

            // Raw service data keyvals - merge preState with storage/preimage modifications from merged accounts
            // Pass the post-accumulation rawServiceDataByStateKey which tracks deletions
            rawServiceDataKvs = mergeServiceDataKvs(
                preState.rawServiceDataKvs,
                serviceAccounts,
                accumulationPost.rawServiceDataByStateKey,
                accumulationOutput.outputs.keys.map { it.toInt() }.toSet(),
                preState.serviceAccounts.map { it.id.toInt() }.toSet(),
                preimageOutput.rawServiceDataUpdates
            )
        )
    }

    /**
     * Merge raw service data keyvals from preState with modifications from accumulation.
     * Uses postAccumulationRawData as the authoritative source for which keys should exist
     */
    private fun mergeServiceDataKvs(
        preStateKvs: List<KeyValue>,
        accumulationAccounts: List<AccumulationServiceItem>,
        postAccumulationRawData: Map<JamByteArray, JamByteArray>,
        yieldedServiceIds: Set<Int>, // Services that yielded (from outputs.keys)
        preStateAccountIds: Set<Int>, // Service IDs that existed in pre-state
        preimageRawUpdates: Map<JamByteArray, JamByteArray> // Raw updates from preimage extrinsic
    ): List<KeyValue> {
        // Build map from existing keyvals for efficient lookup and update
        val kvMap = preStateKvs.associateBy { it.key.toHex() }.toMutableMap()

        val modifiedServiceIds = postAccumulationRawData.keys.mapNotNull { key ->
            if (key.size == 31) {
                // Extract service index from interleaved bytes (positions 0, 2, 4, 6)
                (key.bytes[0].toInt() and 0xFF) or
                    ((key.bytes[2].toInt() and 0xFF) shl 8) or
                    ((key.bytes[4].toInt() and 0xFF) shl 16) or
                    ((key.bytes[6].toInt() and 0xFF) shl 24)
            } else null
        }.toSet()

        val allAccumulatedServiceIds = yieldedServiceIds + modifiedServiceIds

        // For accumulated services, remove keys that are NOT in postAccumulationRawData
        // (those were deleted during accumulation)
        val postRawDataKeyHexes = postAccumulationRawData.keys.map { it.toHex() }.toSet()
        val keysToRemove = kvMap.keys.filter { keyHex ->
            // Check if this key belongs to an accumulated service
            val keyBytes = keyHex.chunked(2).map { it.toInt(16).toByte() }.toByteArray()
            if (keyBytes.size == 31) {
                // Extract service index from interleaved bytes (positions 0, 2, 4, 6)
                val serviceIndex = (keyBytes[0].toInt() and 0xFF) or
                    ((keyBytes[2].toInt() and 0xFF) shl 8) or
                    ((keyBytes[4].toInt() and 0xFF) shl 16) or
                    ((keyBytes[6].toInt() and 0xFF) shl 24)
                // Remove if: 1) belongs to accumulated service AND 2) not in post-accumulation data
                serviceIndex in allAccumulatedServiceIds && keyHex !in postRawDataKeyHexes
            } else {
                false
            }
        }
        keysToRemove.forEach { kvMap.remove(it) }

        // Add/update entries from postAccumulationRawData (includes new writes and updates)
        for ((key, value) in postAccumulationRawData) {
            kvMap[key.toHex()] = KeyValue(key = key, value = value)
        }

        for (account in accumulationAccounts.filter { it.id.toInt() !in preStateAccountIds }) {
            val serviceId = account.id.toInt()

            // Add/update storage entries (only for new accounts)
            for (storageEntry in account.data.storage) {
                val stateKey = StateKeys.serviceStorageKey(serviceId, storageEntry.key)
                kvMap[stateKey.toHex()] = KeyValue(key = stateKey, value = storageEntry.value)
            }
            // Add preimage/preimageStatus for new accounts only
            for (preimage in account.data.preimages) {
                val stateKey = StateKeys.servicePreimageKey(serviceId, preimage.hash)
                kvMap[stateKey.toHex()] = KeyValue(key = stateKey, value = preimage.blob)
            }
        }

        // Add/update entries from preimage extrinsic (for existing accounts)
        for ((key, value) in preimageRawUpdates) {
            kvMap[key.toHex()] = KeyValue(key = key, value = value)
        }

        // Return sorted list
        return kvMap.values.sortedBy { it.key.toHex() }
    }

    /**
     * Encode a list of timestamps (longs) as compact-prefixed bytes.
     */
    private fun encodeTimestampList(timestamps: List<Long>): ByteArray {
        val lengthBytes = encodeCompactInteger(timestamps.size.toLong())
        val dataBytes = timestamps.fold(ByteArray(0)) { acc, ts ->
            acc + encodeFixedWidthInteger(ts, 4, false)
        }
        return lengthBytes + dataBytes
    }

    /**
     * Update recent history's last item with the parent state root.
     * This MUST be called BEFORE validating guarantees
     * The last history entry was created with stateRoot=0, and we now set it to the
     * actual computed state root of the parent block.
     */
    private fun updateRecentHistoryPartial(
        recentHistory: HistoricalBetaContainer,
        parentStateRoot: JamByteArray
    ): HistoricalBetaContainer {
        if (recentHistory.history.isEmpty()) {
            return recentHistory
        }
        val history = recentHistory.history.toMutableList()
        val lastItem = history.last()
        history[history.lastIndex] = lastItem.copy(stateRoot = parentStateRoot)
        return recentHistory.copy(history = history)
    }

    /**
     * Build ReportState from FullJamState, Safrole post-state, and Assurance post-state.
     * Uses availability assignments from Assurance post-state (which has cleared cores
     * for reports that became available).
     */
    private fun buildReportState(
        fullPreState: FullJamState,
        safrolePost: SafroleState,
        assurancePost: AssuranceState,
        updatedRecentHistory: HistoricalBetaContainer
    ): ReportState {
        // Convert AccumulationServiceItem to ServiceItem for ReportState
        val serviceItems = fullPreState.serviceAccounts.map { accItem ->
            ServiceItem(
                id = accItem.id,
                data = accItem.data.toServiceData()
            )
        }

        return ReportState(
            // Use availability assignments from Assurance post-state (cores cleared after reports available)
            availAssignments = assurancePost.availAssignments,
            currValidators = safrolePost.kappa,
            prevValidators = safrolePost.lambda,
            entropy = safrolePost.eta,
            offenders = fullPreState.judgements.offenders.toList(),
            recentBlocks = updatedRecentHistory,
            authPools = fullPreState.authPools,
            accounts = serviceItems,
            coresStatistics = fullPreState.coreStatistics,
            servicesStatistics = fullPreState.serviceStatistics
        )
    }

    /**
     * Extension to convert AccumulationServiceData to ServiceData for ReportState.
     */
    private fun AccumulationServiceData.toServiceData(): ServiceData {
        return ServiceData(service = this.service)
    }

    /**
     * Merge service accounts from accumulation and preimage states.
     */
    private fun mergeServiceAccounts(
        accumulationAccounts: List<AccumulationServiceItem>,
        preimageAccounts: List<io.forge.jam.safrole.preimage.PreimageAccount>
    ): List<AccumulationServiceItem> {
        // Build map from accumulation accounts
        val accountMap = accumulationAccounts.associateBy { it.id }.toMutableMap()

        // Merge preimage data into accounts
        for (preimageAccount in preimageAccounts) {
            val existing = accountMap[preimageAccount.id]
            if (existing != null) {
                // Update preimages and lookup metadata
                val updatedData = existing.data.copy(
                    preimages = preimageAccount.data.preimages,
                    preimagesStatus = preimageAccount.data.lookupMeta.map { meta ->
                        PreimagesStatusMapEntry(
                            hash = meta.key.hash,
                            status = meta.value
                        )
                    }
                )
                accountMap[preimageAccount.id] = AccumulationServiceItem(preimageAccount.id, updatedData)
            }
        }

        return accountMap.values.sortedBy { it.id }
    }

    /**
     * Imports a block and returns just the computed SafroleState for comparison.
     * This is useful for trace testing where we want to compare typed state.
     */
    fun importBlockForSafrole(block: Block, preState: RawState): Pair<SafroleState?, String?> {
        return try {
            val result = importBlock(block, preState)
            when (result) {
                is ImportResult.Success -> Pair(result.safroleState, null)
                is ImportResult.Failure -> Pair(null, result.message)
            }
        } catch (e: Exception) {
            Pair(null, "Exception: ${e.message}")
        }
    }

    /**
     * Validates that a block import produces the expected post-state.
     * Used for testing against trace vectors.
     */
    fun validateBlockImport(
        block: Block,
        preState: RawState,
        expectedPostState: RawState
    ): Boolean {
        val result = importBlock(block, preState)
        return when (result) {
            is ImportResult.Success -> {
                // Compare computed post-state root with expected
                result.postState.stateRoot.contentEquals(expectedPostState.stateRoot)
            }

            is ImportResult.Failure -> false
        }
    }
}
