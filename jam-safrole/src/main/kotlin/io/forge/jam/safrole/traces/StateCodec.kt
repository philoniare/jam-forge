package io.forge.jam.safrole.traces

import io.forge.jam.core.*
import io.forge.jam.safrole.*
import io.forge.jam.safrole.accumulation.Privileges
import io.forge.jam.safrole.accumulation.ReadyRecord
import io.forge.jam.safrole.accumulation.ServiceStatisticsEntry
import io.forge.jam.safrole.authorization.AUTH_QUEUE_SIZE
import io.forge.jam.safrole.historical.HistoricalBetaContainer
import io.forge.jam.safrole.report.AccumulationServiceData
import io.forge.jam.safrole.report.AccumulationServiceItem
import io.forge.jam.safrole.report.CoreStatisticsRecord
import io.forge.jam.safrole.safrole.SafroleGammaState
import io.forge.jam.safrole.safrole.SafroleInput
import io.forge.jam.safrole.safrole.SafroleState
import io.forge.jam.safrole.stats.StatCount
import io.forge.jam.vrfs.BandersnatchWrapper
import org.bouncycastle.jcajce.provider.digest.Blake2b

/**
 * Codec for encoding/decoding state between raw keyvals and typed state structures.
 * Delegates to data class fromBytes() methods for decoding.
 */
object StateCodec {
    /**
     * Blake2b-256 hash function.
     */
    private fun blake2b256(data: ByteArray): ByteArray {
        val digest = Blake2b.Blake2b256()
        digest.update(data, 0, data.size)
        return digest.digest()
    }

    /**
     * Decodes the full JAM state from keyvals.
     * Groups keyvals by state key prefix and delegates to individual fromBytes() methods.
     */
    fun decodeFullState(keyvals: List<KeyValue>, config: TinyConfig = TinyConfig): FullJamState {
        // Group keyvals by component prefix
        val grouped = mutableMapOf<Int, MutableList<KeyValue>>()
        val serviceAccountKvs = mutableMapOf<Int, KeyValue>()
        val serviceDataKvs = mutableMapOf<Int, MutableList<KeyValue>>()

        for (kv in keyvals) {
            val keyByte = kv.key.bytes[0].toInt() and 0xFF

            when {
                keyByte == 0xFF -> {
                    // Service account key (δ) - extract service index
                    val serviceIndex = extractServiceIndex255(kv.key)
                    serviceAccountKvs[serviceIndex] = kv
                }

                isServiceDataKey(keyByte) -> {
                    // Service storage/preimage key - extract service index
                    val serviceIndex = extractServiceIndexInterleaved(kv.key)
                    serviceDataKvs.getOrPut(serviceIndex) { mutableListOf() }.add(kv)
                }

                else -> {
                    grouped.getOrPut(keyByte) { mutableListOf() }.add(kv)
                }
            }
        }

        // Decode individual components
        val timeslot = grouped[StateKeys.TIMESLOT.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeFixedWidthInteger(it.value.bytes, 0, 4, false)
        } ?: 0L

        val entropyPool = grouped[StateKeys.ENTROPY_POOL.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeEntropyPoolAsList(it.value.bytes)
        } ?: List(4) { JamByteArray(ByteArray(32)) }

        val currentValidators = grouped[StateKeys.CURRENT_VALIDATORS.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeValidatorList(it.value.bytes, config.VALIDATORS_COUNT)
        } ?: emptyList()

        val previousValidators = grouped[StateKeys.PREVIOUS_VALIDATORS.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeValidatorList(it.value.bytes, config.VALIDATORS_COUNT)
        } ?: emptyList()

        val validatorQueue = grouped[StateKeys.VALIDATOR_QUEUE.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeValidatorList(it.value.bytes, config.VALIDATORS_COUNT)
        } ?: emptyList()

        // Safrole gamma state
        val safroleGamma = grouped[StateKeys.SAFROLE_STATE.toInt() and 0xFF]?.firstOrNull()?.let {
            SafroleGammaState.fromBytes(it.value.bytes, 0, config.VALIDATORS_COUNT, config.EPOCH_LENGTH)
        }

        // Authorization pools and queues
        val authPools = grouped[StateKeys.CORE_AUTHORIZATION_POOL.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeAuthPools(it.value.bytes, config.CORES_COUNT)
        } ?: List(config.CORES_COUNT) { emptyList() }

        val authQueues = grouped[StateKeys.AUTHORIZATION_QUEUE.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeAuthQueues(it.value.bytes, config.CORES_COUNT)
        } ?: List(config.CORES_COUNT) { List(AUTH_QUEUE_SIZE) { JamByteArray(ByteArray(32)) } }

        // Recent history (β)
        val recentHistory = grouped[StateKeys.RECENT_HISTORY.toInt() and 0xFF]?.firstOrNull()?.let {
            HistoricalBetaContainer.fromBytes(it.value.bytes, 0).first
        } ?: HistoricalBetaContainer()

        // Pending reports (ρ)
        val reports = grouped[StateKeys.REPORTS.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeReports(it.value.bytes, config.CORES_COUNT)
        } ?: List(config.CORES_COUNT) { null }

        // Judgements (ψ)
        val judgements = grouped[StateKeys.JUDGEMENTS.toInt() and 0xFF]?.firstOrNull()?.let {
            Psi.fromBytes(it.value.bytes, 0).first
        } ?: Psi()

        // Privileged services (χ)
        val privilegedServices = grouped[StateKeys.PRIVILEGED_SERVICES.toInt() and 0xFF]?.firstOrNull()?.let {
            Privileges.fromBytes(it.value.bytes, 0, config.CORES_COUNT).first
        } ?: Privileges(0, List(config.CORES_COUNT) { 0L }, 0, 0, emptyList())

        // Activity statistics
        val activityStats = grouped[StateKeys.ACTIVITY_STATISTICS.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeActivityStatistics(it.value.bytes, config.VALIDATORS_COUNT, config.CORES_COUNT)
        } ?: ActivityStatisticsData(
            List(config.VALIDATORS_COUNT) { StatCount() },
            List(config.VALIDATORS_COUNT) { StatCount() },
            List(config.CORES_COUNT) { CoreStatisticsRecord() },
            emptyList()
        )
        val activityStatsCurrent = activityStats.current
        val activityStatsLast = activityStats.last
        val coreStatistics = activityStats.coreStats
        // Service stats from activity statistics key (0x0d)
        val activityServiceStats = activityStats.serviceStats

        // Accumulation queue
        val accumulationQueue = grouped[StateKeys.ACCUMULATION_QUEUE.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeAccumulationQueue(it.value.bytes, config.EPOCH_LENGTH)
        } ?: List(config.EPOCH_LENGTH) { emptyList() }

        // Accumulation history
        val accumulationHistory = grouped[StateKeys.ACCUMULATION_HISTORY.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeAccumulationHistory(it.value.bytes, config.EPOCH_LENGTH)
        } ?: List(config.EPOCH_LENGTH) { emptyList() }

        // Last accumulation outputs
        val lastAccumulationOutputs =
            grouped[StateKeys.LAST_ACCUMULATION_OUTPUTS.toInt() and 0xFF]?.firstOrNull()?.let {
                decodeLastAccumulationOutputs(it.value.bytes)
            } ?: emptyMap()

        // Service accounts - combine account metadata with storage/preimage data
        val serviceAccountsList = serviceAccountKvs.map { (serviceIndex, kv) ->
            val (accountData, _) = AccumulationServiceData.fromBytes(kv.value.bytes, 0)
            AccumulationServiceItem(serviceIndex.toUInt().toLong(), accountData)
        }.sortedBy { it.id }.toMutableList()

        // Collect all raw service data keyvals for pass-through
        val rawServiceDataKvs = serviceDataKvs.values.flatten().sortedBy { it.key.toHex() }

        // Extract preimages and preimage info from service data keyvals and merge into service accounts
        // Preimage data is stored separately with interleaved keys
        serviceDataKvs.forEach { (serviceIndex, kvList) ->
            val accountIdx = serviceAccountsList.indexOfFirst { it.id == serviceIndex.toUInt().toLong() }
            if (accountIdx >= 0) {
                val account = serviceAccountsList[accountIdx]
                val preimagesList = account.data.preimages.toMutableList()
                val preimageStatusList = account.data.preimagesStatus.toMutableList()

                // First pass: collect preimage blobs to build hash->size map
                val hashToBlob = mutableMapOf<String, JamByteArray>()
                kvList.forEach { kv ->
                    // Large values (>1000 bytes) are likely preimages (service code)
                    if (kv.value.bytes.size > 1000) {
                        val hash = JamByteArray(blake2b256(kv.value.bytes))
                        preimagesList.add(io.forge.jam.safrole.preimage.PreimageHash(hash, kv.value))
                        hashToBlob[hash.toHex()] = kv.value
                    }
                }

                // Second pass: extract preimage info entries
                kvList.forEach { kv ->
                    val valueSize = kv.value.bytes.size
                    // Valid sizes: 1, 5, 9, 13 bytes
                    if (valueSize in listOf(1, 5, 9, 13)) {
                        val bytes = kv.value.bytes
                        val count = bytes[0].toInt() and 0xFF
                        if (count <= 3 && valueSize == 1 + count * 4) {
                            // This is a preimage info entry - extract timeslots
                            val timeslots = mutableListOf<Long>()
                            for (i in 0 until count) {
                                val offset = 1 + i * 4
                                val ts = (bytes[offset].toLong() and 0xFF) or
                                    ((bytes[offset + 1].toLong() and 0xFF) shl 8) or
                                    ((bytes[offset + 2].toLong() and 0xFF) shl 16) or
                                    ((bytes[offset + 3].toLong() and 0xFF) shl 24)
                                timeslots.add(ts)
                            }

                            val actualKeyHex = kv.key.toHex()
                            for ((hashHex, blob) in hashToBlob) {
                                val hash = JamByteArray(hashHex.chunked(2).map { it.toInt(16).toByte() }.toByteArray())
                                val computedKey = io.forge.jam.safrole.accumulation.computePreimageInfoStateKey(
                                    serviceIndex.toUInt().toLong(), blob.size, hash
                                )
                                if (computedKey.toHex() == actualKeyHex) {
                                    preimageStatusList.add(
                                        io.forge.jam.safrole.report.PreimagesStatusMapEntry(hash, timeslots)
                                    )
                                    break
                                }
                            }
                        }
                    }
                }

                // Update the account with merged preimages and status
                if (preimagesList.size > account.data.preimages.size || preimageStatusList.size > account.data.preimagesStatus.size) {
                    val updatedData = account.data.copy(
                        preimages = preimagesList,
                        preimagesStatus = preimageStatusList
                    )
                    serviceAccountsList[accountIdx] = account.copy(data = updatedData)
                }
            }
        }

        val serviceAccounts = serviceAccountsList.toList()

        // Service statistics - merge from activity stats (0x0d) and standalone key (0x11)
        // The activity stats key contains service stats, and there may also be a standalone key
        val standaloneServiceStats = grouped[StateKeys.SERVICE_STATISTICS.toInt() and 0xFF]?.firstOrNull()?.let {
            decodeServiceStatistics(it.value.bytes)
        } ?: emptyList()

        // Merge: prefer standalone if present, otherwise use from activity stats
        val serviceStatistics = if (standaloneServiceStats.isNotEmpty()) {
            standaloneServiceStats
        } else {
            activityServiceStats
        }

        return FullJamState(
            timeslot = timeslot,
            entropyPool = entropyPool,
            currentValidators = currentValidators,
            previousValidators = previousValidators,
            validatorQueue = validatorQueue,
            safroleGammaK = safroleGamma?.gammaK ?: emptyList(),
            safroleGammaZ = safroleGamma?.gammaZ ?: JamByteArray(ByteArray(144)),
            safroleGammaS = safroleGamma?.gammaS ?: TicketsOrKeys(),
            safroleGammaA = safroleGamma?.gammaA ?: emptyList(),
            authPools = authPools,
            authQueues = authQueues,
            recentHistory = recentHistory,
            reports = reports,
            judgements = judgements,
            privilegedServices = privilegedServices,
            activityStatsCurrent = activityStatsCurrent,
            activityStatsLast = activityStatsLast,
            coreStatistics = coreStatistics,
            accumulationQueue = accumulationQueue,
            accumulationHistory = accumulationHistory,
            lastAccumulationOutputs = lastAccumulationOutputs,
            serviceAccounts = serviceAccounts,
            serviceStatistics = serviceStatistics,
            rawServiceDataKvs = rawServiceDataKvs
        )
    }

    /**
     * Checks if a key byte indicates a service data key (interleaved encoding).
     * Service data keys don't have a fixed prefix - the service index is interleaved.
     */
    private fun isServiceDataKey(keyByte: Int): Boolean {
        // Known non-service key prefixes
        val knownPrefixes = setOf(
            StateKeys.CORE_AUTHORIZATION_POOL.toInt() and 0xFF,
            StateKeys.AUTHORIZATION_QUEUE.toInt() and 0xFF,
            StateKeys.RECENT_HISTORY.toInt() and 0xFF,
            StateKeys.SAFROLE_STATE.toInt() and 0xFF,
            StateKeys.JUDGEMENTS.toInt() and 0xFF,
            StateKeys.ENTROPY_POOL.toInt() and 0xFF,
            StateKeys.VALIDATOR_QUEUE.toInt() and 0xFF,
            StateKeys.CURRENT_VALIDATORS.toInt() and 0xFF,
            StateKeys.PREVIOUS_VALIDATORS.toInt() and 0xFF,
            StateKeys.REPORTS.toInt() and 0xFF,
            StateKeys.TIMESLOT.toInt() and 0xFF,
            StateKeys.PRIVILEGED_SERVICES.toInt() and 0xFF,
            StateKeys.ACTIVITY_STATISTICS.toInt() and 0xFF,
            StateKeys.ACCUMULATION_QUEUE.toInt() and 0xFF,
            StateKeys.ACCUMULATION_HISTORY.toInt() and 0xFF,
            StateKeys.LAST_ACCUMULATION_OUTPUTS.toInt() and 0xFF,
            StateKeys.SERVICE_STATISTICS.toInt() and 0xFF,
            0xFF // Service account prefix
        )
        return keyByte !in knownPrefixes
    }

    /**
     * Extracts service index from a key with prefix 255.
     */
    private fun extractServiceIndex255(key: JamByteArray): Int {
        val bytes = key.bytes
        return (bytes[1].toInt() and 0xFF) or
            ((bytes[3].toInt() and 0xFF) shl 8) or
            ((bytes[5].toInt() and 0xFF) shl 16) or
            ((bytes[7].toInt() and 0xFF) shl 24)
    }

    /**
     * Extracts service index from a service data key (interleaved encoding).
     */
    private fun extractServiceIndexInterleaved(key: JamByteArray): Int {
        val bytes = key.bytes
        return (bytes[0].toInt() and 0xFF) or
            ((bytes[2].toInt() and 0xFF) shl 8) or
            ((bytes[4].toInt() and 0xFF) shl 16) or
            ((bytes[6].toInt() and 0xFF) shl 24)
    }

    /**
     * Decodes entropy pool and returns as List.
     */
    private fun decodeEntropyPoolAsList(value: ByteArray): List<JamByteArray> {
        val result = mutableListOf<JamByteArray>()
        var offset = 0
        for (i in 0 until 4) {
            if (offset + 32 <= value.size) {
                result.add(JamByteArray(value.copyOfRange(offset, offset + 32)))
                offset += 32
            } else {
                result.add(JamByteArray(ByteArray(32)))
            }
        }
        return result
    }

    /**
     * Decodes authorization pools.
     */
    private fun decodeAuthPools(value: ByteArray, coresCount: Int): List<List<JamByteArray>> {
        var offset = 0
        val pools = mutableListOf<List<JamByteArray>>()
        for (i in 0 until coresCount) {
            val (length, lengthBytes) = decodeCompactInteger(value, offset)
            offset += lengthBytes
            val pool = mutableListOf<JamByteArray>()
            for (j in 0 until length.toInt()) {
                pool.add(JamByteArray(value.copyOfRange(offset, offset + 32)))
                offset += 32
            }
            pools.add(pool)
        }
        return pools
    }

    /**
     * Decodes authorization queues.
     */
    private fun decodeAuthQueues(value: ByteArray, coresCount: Int): List<List<JamByteArray>> {
        var offset = 0
        val queues = mutableListOf<List<JamByteArray>>()
        for (i in 0 until coresCount) {
            val queue = mutableListOf<JamByteArray>()
            for (j in 0 until AUTH_QUEUE_SIZE) {
                queue.add(JamByteArray(value.copyOfRange(offset, offset + 32)))
                offset += 32
            }
            queues.add(queue)
        }
        return queues
    }

    /**
     * Decodes pending reports (availability assignments).
     */
    private fun decodeReports(value: ByteArray, coresCount: Int): List<AvailabilityAssignment?> {
        var offset = 0
        val reports = mutableListOf<AvailabilityAssignment?>()
        for (i in 0 until coresCount) {
            val discriminator = value[offset].toInt() and 0xFF
            offset += 1
            if (discriminator == 0) {
                reports.add(null)
            } else {
                val (assignment, assignmentBytes) = AvailabilityAssignment.fromBytes(value, offset)
                reports.add(assignment)
                offset += assignmentBytes
            }
        }
        return reports
    }

    /**
     * Data class for decoded activity statistics.
     */
    data class ActivityStatisticsData(
        val current: List<StatCount>,
        val last: List<StatCount>,
        val coreStats: List<CoreStatisticsRecord>,
        val serviceStats: List<ServiceStatisticsEntry>
    )

    /**
     * Decodes activity statistics: validator stats (current, last) + core stats + service stats.
     */
    private fun decodeActivityStatistics(
        value: ByteArray,
        validatorsCount: Int,
        coresCount: Int
    ): ActivityStatisticsData {
        var offset = 0
        val current = mutableListOf<StatCount>()
        for (i in 0 until validatorsCount) {
            current.add(StatCount.fromBytes(value, offset))
            offset += StatCount.SIZE
        }
        val last = mutableListOf<StatCount>()
        for (i in 0 until validatorsCount) {
            last.add(StatCount.fromBytes(value, offset))
            offset += StatCount.SIZE
        }

        // Decode core statistics
        val coreStats = mutableListOf<CoreStatisticsRecord>()
        for (i in 0 until coresCount) {
            val (coreRecord, coreBytes) = CoreStatisticsRecord.fromBytes(value, offset)
            coreStats.add(coreRecord)
            offset += coreBytes
        }

        // Decode service statistics (compact length + entries)
        val serviceStats = mutableListOf<ServiceStatisticsEntry>()
        if (offset < value.size) {
            val (length, lengthBytes) = decodeCompactInteger(value, offset)
            offset += lengthBytes
            for (i in 0 until length.toInt()) {
                val (entry, entryBytes) = ServiceStatisticsEntry.fromBytes(value, offset)
                serviceStats.add(entry)
                offset += entryBytes
            }
        }

        return ActivityStatisticsData(current, last, coreStats, serviceStats)
    }

    /**
     * Decodes accumulation queue.
     */
    private fun decodeAccumulationQueue(value: ByteArray, epochLength: Int): List<List<ReadyRecord>> {
        var offset = 0
        val queue = mutableListOf<List<ReadyRecord>>()
        for (i in 0 until epochLength) {
            val (length, lengthBytes) = decodeCompactInteger(value, offset)
            offset += lengthBytes
            val records = mutableListOf<ReadyRecord>()
            for (j in 0 until length.toInt()) {
                val (record, recordBytes) = ReadyRecord.fromBytes(value, offset)
                records.add(record)
                offset += recordBytes
            }
            queue.add(records)
        }
        return queue
    }

    /**
     * Decodes accumulation history.
     */
    private fun decodeAccumulationHistory(value: ByteArray, epochLength: Int): List<List<JamByteArray>> {
        var offset = 0
        val history = mutableListOf<List<JamByteArray>>()
        for (i in 0 until epochLength) {
            val (length, lengthBytes) = decodeCompactInteger(value, offset)
            offset += lengthBytes
            val hashes = mutableListOf<JamByteArray>()
            for (j in 0 until length.toInt()) {
                hashes.add(JamByteArray(value.copyOfRange(offset, offset + 32)))
                offset += 32
            }
            history.add(hashes)
        }
        return history
    }

    /**
     * Decodes last accumulation outputs.
     */
    private fun decodeLastAccumulationOutputs(value: ByteArray): Map<Long, JamByteArray> {
        var offset = 0
        val (length, lengthBytes) = decodeCompactInteger(value, offset)
        offset += lengthBytes
        val outputs = mutableMapOf<Long, JamByteArray>()
        for (i in 0 until length.toInt()) {
            val serviceId = decodeFixedWidthInteger(value, offset, 4, false)
            offset += 4
            val hash = JamByteArray(value.copyOfRange(offset, offset + 32))
            offset += 32
            outputs[serviceId] = hash
        }
        return outputs
    }

    /**
     * Decodes service statistics.
     */
    private fun decodeServiceStatistics(value: ByteArray): List<ServiceStatisticsEntry> {
        var offset = 0
        val (length, lengthBytes) = decodeCompactInteger(value, offset)
        offset += lengthBytes
        val entries = mutableListOf<ServiceStatisticsEntry>()
        for (i in 0 until length.toInt()) {
            val (entry, entryBytes) = ServiceStatisticsEntry.fromBytes(value, offset)
            entries.add(entry)
            offset += entryBytes
        }
        return entries
    }

    /**
     * Decodes SafroleState from keyvals.
     * Delegates to data class fromBytes() methods.
     */
    fun decodeSafroleState(keyvals: List<KeyValue>, config: TinyConfig = TinyConfig): SafroleState {
        var tau: Long = 0
        val eta = JamByteArrayList()
        var kappa: List<ValidatorKey> = emptyList()
        var lambda: List<ValidatorKey> = emptyList()
        var gammaK: List<ValidatorKey> = emptyList()
        var iota: List<ValidatorKey> = emptyList()
        var gammaA: List<TicketBody> = emptyList()
        var gammaS: TicketsOrKeys = TicketsOrKeys()
        var gammaZ: JamByteArray = JamByteArray(ByteArray(144))

        for (kv in keyvals) {
            val keyByte = kv.key.bytes[0].toInt() and 0xFF
            val value = kv.value.bytes

            when (keyByte) {
                StateKeys.TIMESLOT.toInt() and 0xFF -> {
                    // Timeslot: 4 bytes little-endian
                    tau = decodeFixedWidthInteger(value, 0, 4, false)
                }

                StateKeys.ENTROPY_POOL.toInt() and 0xFF -> {
                    // Entropy pool: 4 x 32-byte hashes
                    decodeEntropyPool(value, eta)
                }

                StateKeys.CURRENT_VALIDATORS.toInt() and 0xFF -> {
                    // Current validators (κ) - delegate to ValidatorKey.fromBytes()
                    kappa = decodeValidatorList(value, config.VALIDATORS_COUNT)
                }

                StateKeys.PREVIOUS_VALIDATORS.toInt() and 0xFF -> {
                    // Previous validators (λ)
                    lambda = decodeValidatorList(value, config.VALIDATORS_COUNT)
                }

                StateKeys.VALIDATOR_QUEUE.toInt() and 0xFF -> {
                    // Pending validators (ι)
                    iota = decodeValidatorList(value, config.VALIDATORS_COUNT)
                }

                StateKeys.SAFROLE_STATE.toInt() and 0xFF -> {
                    // Safrole gamma state (γk, γz, γs, γa) - delegate to SafroleGammaState.fromBytes()
                    val decoded = SafroleGammaState.fromBytes(value, 0, config.VALIDATORS_COUNT, config.EPOCH_LENGTH)
                    gammaK = decoded.gammaK
                    gammaA = decoded.gammaA
                    gammaS = decoded.gammaS
                    gammaZ = decoded.gammaZ
                }
            }
        }

        // Default entropy if not found
        if (eta.isEmpty()) {
            repeat(4) { eta.add(JamByteArray(ByteArray(32))) }
        }

        return SafroleState(
            tau = tau,
            eta = eta,
            lambda = lambda,
            kappa = kappa,
            gammaK = gammaK,
            iota = iota,
            gammaA = gammaA,
            gammaS = gammaS,
            gammaZ = gammaZ
        )
    }

    /**
     * Decodes entropy pool from raw bytes.
     */
    private fun decodeEntropyPool(value: ByteArray, eta: JamByteArrayList) {
        // Entropy pool is 4 x 32-byte hashes (128 bytes total)
        var offset = 0
        for (i in 0 until 4) {
            if (offset + 32 <= value.size) {
                eta.add(JamByteArray(value.copyOfRange(offset, offset + 32)))
                offset += 32
            } else {
                eta.add(JamByteArray(ByteArray(32)))
            }
        }
    }

    /**
     * Decodes a list of validators from raw bytes using ValidatorKey.fromBytes().
     */
    private fun decodeValidatorList(value: ByteArray, expectedCount: Int): List<ValidatorKey> {
        return decodeFixedList(value, 0, expectedCount, ValidatorKey.SIZE) { data, offset ->
            ValidatorKey.fromBytes(data, offset)
        }
    }

    /**
     * Extracts SafroleInput from a Block using jam-core data structures.
     */
    fun extractSafroleInput(block: Block, preState: SafroleState): SafroleInput {
        val header = block.header

        // Extract ticket envelopes from extrinsic - tickets is directly List<TicketEnvelope>
        val tickets = block.extrinsic.tickets

        // Extract disputes if present
        val disputes = block.extrinsic.disputes.takeIf {
            it.verdicts.isNotEmpty() || it.culprits.isNotEmpty() || it.faults.isNotEmpty()
        }

        // The header.entropySource is a 96-byte Bandersnatch IETF VRF signature.
        // We need to extract the 32-byte VRF output using native code.
        val entropyBytes = header.entropySource.bytes
        val vrfOutput = if (entropyBytes.size >= 96) {
            try {
                val output = BandersnatchWrapper.getIetfVrfOutput(entropyBytes)
                JamByteArray(output)
            } catch (e: Exception) {
                // Fallback if native extraction fails
                JamByteArray(entropyBytes.copyOfRange(0, 32))
            }
        } else if (entropyBytes.size >= 32) {
            JamByteArray(entropyBytes.copyOfRange(0, 32))
        } else {
            header.entropySource
        }

        return SafroleInput(
            slot = header.slot,
            entropy = vrfOutput,
            extrinsic = tickets,
            disputes = disputes
        )
    }
}
