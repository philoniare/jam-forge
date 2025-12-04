package io.forge.jam.safrole.traces

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.core.encodeFixedWidthInteger
import io.forge.jam.core.encodeList
import io.forge.jam.safrole.AvailabilityAssignment
import io.forge.jam.safrole.ValidatorKey
import io.forge.jam.safrole.accumulation.ReadyRecord
import io.forge.jam.safrole.accumulation.ServiceStatisticsEntry
import io.forge.jam.safrole.report.CoreStatisticsRecord
import io.forge.jam.safrole.safrole.SafroleGammaState
import io.forge.jam.safrole.stats.StatCount

/**
 * Encoder for converting typed state structures back to raw keyvals.
 * This is the reverse of StateCodec.decodeFullState().
 */
object StateEncoder {

    /**
     * Encodes the full JAM state to keyvals.
     */
    fun encodeFullState(state: FullJamState, config: TinyConfig = TinyConfig): List<KeyValue> {
        val keyvals = mutableListOf<KeyValue>()

        // Timeslot
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.TIMESLOT),
                value = JamByteArray(encodeFixedWidthInteger(state.timeslot, 4, false))
            )
        )

        // Entropy pool
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.ENTROPY_POOL),
                value = JamByteArray(encodeEntropyPool(state.entropyPool))
            )
        )

        // Current validators
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.CURRENT_VALIDATORS),
                value = JamByteArray(encodeValidatorList(state.currentValidators))
            )
        )

        // Previous validators
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.PREVIOUS_VALIDATORS),
                value = JamByteArray(encodeValidatorList(state.previousValidators))
            )
        )

        // Validator queue
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.VALIDATOR_QUEUE),
                value = JamByteArray(encodeValidatorList(state.validatorQueue))
            )
        )

        // Safrole state
        val gammaState = SafroleGammaState(
            gammaK = state.safroleGammaK,
            gammaA = state.safroleGammaA,
            gammaS = state.safroleGammaS,
            gammaZ = state.safroleGammaZ
        )
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.SAFROLE_STATE),
                value = JamByteArray(encodeSafroleGammaState(gammaState))
            )
        )

        // Core authorization pools
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.CORE_AUTHORIZATION_POOL),
                value = JamByteArray(encodeAuthPools(state.authPools))
            )
        )

        // Authorization queues
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.AUTHORIZATION_QUEUE),
                value = JamByteArray(encodeAuthQueues(state.authQueues))
            )
        )

        // Recent history
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.RECENT_HISTORY),
                value = JamByteArray(state.recentHistory.encode())
            )
        )

        // Pending reports
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.REPORTS),
                value = JamByteArray(encodeReports(state.reports))
            )
        )

        // Judgements
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.JUDGEMENTS),
                value = JamByteArray(state.judgements.encode())
            )
        )

        // Privileged services
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.PRIVILEGED_SERVICES),
                value = JamByteArray(state.privilegedServices.encode())
            )
        )

        // Activity statistics (validator + core + service)
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS),
                value = JamByteArray(
                    encodeActivityStatistics(
                        state.activityStatsCurrent,
                        state.activityStatsLast,
                        state.coreStatistics,
                        state.serviceStatistics
                    )
                )
            )
        )

        // Accumulation queue
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.ACCUMULATION_QUEUE),
                value = JamByteArray(encodeAccumulationQueue(state.accumulationQueue))
            )
        )

        // Accumulation history
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.ACCUMULATION_HISTORY),
                value = JamByteArray(encodeAccumulationHistory(state.accumulationHistory))
            )
        )

        // Last accumulation outputs
        keyvals.add(
            KeyValue(
                key = StateKeys.simpleKey(StateKeys.LAST_ACCUMULATION_OUTPUTS),
                value = JamByteArray(encodeLastAccumulationOutputs(state.lastAccumulationOutputs))
            )
        )

        // Service accounts
        for (serviceAccount in state.serviceAccounts) {
            // Service account metadata (key prefix 0xFF)
            keyvals.add(
                KeyValue(
                    key = StateKeys.serviceAccountKey(serviceAccount.id.toInt()),
                    value = JamByteArray(serviceAccount.data.service.encode())
                )
            )
        }

        keyvals.addAll(state.rawServiceDataKvs)

        // Sort by key for deterministic ordering
        return keyvals.sortedBy { it.key.toHex() }
    }

    /**
     * Encodes preimage info value (list of timeslots).
     */
    private fun encodePreimageInfoValue(timeslots: List<Long>): ByteArray {
        return timeslots.fold(ByteArray(0)) { acc, timeslot ->
            acc + encodeFixedWidthInteger(timeslot, 4, false)
        }
    }

    /**
     * Encodes entropy pool.
     */
    private fun encodeEntropyPool(entropyPool: List<JamByteArray>): ByteArray {
        return entropyPool.fold(ByteArray(0)) { acc, hash -> acc + hash.bytes }
    }

    /**
     * Encodes a list of validators.
     */
    private fun encodeValidatorList(validators: List<ValidatorKey>): ByteArray {
        return validators.fold(ByteArray(0)) { acc, validator -> acc + validator.encode() }
    }

    /**
     * Encodes Safrole gamma state.
     */
    private fun encodeSafroleGammaState(state: SafroleGammaState): ByteArray {
        val gammaKBytes = state.gammaK.fold(ByteArray(0)) { acc, v -> acc + v.encode() }
        val gammaZBytes = state.gammaZ.bytes
        val gammaSBytes = state.gammaS.encode()
        val gammaABytes = encodeList(state.gammaA) // Compact length prefix
        return gammaKBytes + gammaZBytes + gammaSBytes + gammaABytes
    }

    /**
     * Encodes authorization pools.
     */
    private fun encodeAuthPools(pools: List<List<JamByteArray>>): ByteArray {
        return pools.fold(ByteArray(0)) { acc, pool ->
            val lengthBytes = encodeCompactInteger(pool.size.toLong())
            val poolBytes = pool.fold(ByteArray(0)) { innerAcc, hash -> innerAcc + hash.bytes }
            acc + lengthBytes + poolBytes
        }
    }

    /**
     * Encodes authorization queues.
     */
    private fun encodeAuthQueues(queues: List<List<JamByteArray>>): ByteArray {
        return queues.fold(ByteArray(0)) { acc, queue ->
            val queueBytes = queue.fold(ByteArray(0)) { innerAcc, hash -> innerAcc + hash.bytes }
            acc + queueBytes
        }
    }

    /**
     * Encodes pending reports.
     */
    private fun encodeReports(reports: List<AvailabilityAssignment?>): ByteArray {
        return reports.fold(ByteArray(0)) { acc, assignment ->
            if (assignment == null) {
                acc + byteArrayOf(0)
            } else {
                acc + byteArrayOf(1) + assignment.encode()
            }
        }
    }

    /**
     * Encodes activity statistics: validator stats (current, last) + core stats + service stats.
     */
    private fun encodeActivityStatistics(
        current: List<StatCount>,
        last: List<StatCount>,
        coreStats: List<CoreStatisticsRecord>,
        serviceStats: List<ServiceStatisticsEntry>
    ): ByteArray {
        val currentBytes = current.fold(ByteArray(0)) { acc, stat -> acc + stat.encode() }
        val lastBytes = last.fold(ByteArray(0)) { acc, stat -> acc + stat.encode() }
        val coreBytes = coreStats.fold(ByteArray(0)) { acc, stat -> acc + stat.encode() }

        // Service statistics: compact length prefix + sorted entries by service ID
        // For now, encode as empty list (compact 0)
        val serviceBytes = encodeCompactInteger(serviceStats.size.toLong()) +
            serviceStats.sortedBy { it.id }.fold(ByteArray(0)) { acc, entry ->
                acc + encodeFixedWidthInteger(entry.id, 4, false) + entry.record.encode()
            }

        return currentBytes + lastBytes + coreBytes + serviceBytes
    }

    /**
     * Encodes accumulation queue.
     */
    private fun encodeAccumulationQueue(queue: List<List<ReadyRecord>>): ByteArray {
        return queue.fold(ByteArray(0)) { acc, records ->
            val lengthBytes = encodeCompactInteger(records.size.toLong())
            val recordsBytes = records.fold(ByteArray(0)) { innerAcc, record -> innerAcc + record.encode() }
            acc + lengthBytes + recordsBytes
        }
    }

    /**
     * Encodes accumulation history.
     */
    private fun encodeAccumulationHistory(history: List<List<JamByteArray>>): ByteArray {
        return history.fold(ByteArray(0)) { acc, hashes ->
            val lengthBytes = encodeCompactInteger(hashes.size.toLong())
            val hashesBytes = hashes.fold(ByteArray(0)) { innerAcc, hash -> innerAcc + hash.bytes }
            acc + lengthBytes + hashesBytes
        }
    }

    /**
     * Encodes last accumulation outputs.
     */
    private fun encodeLastAccumulationOutputs(outputs: Map<Long, JamByteArray>): ByteArray {
        val sorted = outputs.entries.sortedBy { it.key }
        val lengthBytes = encodeCompactInteger(sorted.size.toLong())
        val outputsBytes = sorted.fold(ByteArray(0)) { acc, (serviceId, hash) ->
            acc + encodeFixedWidthInteger(serviceId, 4, false) + hash.bytes
        }
        return lengthBytes + outputsBytes
    }

    /**
     * Encodes service statistics.
     */
    private fun encodeServiceStatistics(statistics: List<ServiceStatisticsEntry>): ByteArray {
        return encodeList(statistics)
    }
}
