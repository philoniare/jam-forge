package io.forge.jam.safrole.accumulation

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.WorkReport
import org.bouncycastle.jcajce.provider.digest.Keccak
import java.nio.ByteBuffer
import java.nio.ByteOrder

/**
 * Accumulation state transition implementing Gray Paper equations 25-90, 417-424.
 * Handles ring buffer management for ready_queue and accumulated arrays,
 * dependency resolution, and PVM execution for service accumulation.
 */
class AccumulationStateTransition(private val config: AccumulationConfig) {
    private val executor = AccumulationExecutor(config)

    fun transition(
        input: AccumulationInput,
        preState: AccumulationState
    ): Pair<AccumulationState, AccumulationOutput> {
        val m = (input.slot % config.EPOCH_LENGTH).toInt()
        (preState.slot % config.EPOCH_LENGTH).toInt()
        val deltaT = (input.slot - preState.slot).toInt().coerceAtLeast(1)

        // 1. Collect all historically accumulated hashes (for dependency checking)
        val historicallyAccumulated = preState.accumulated.flatten().toMutableSet()

        // 2. Partition new reports into immediate vs queued
        val immediateReports = input.reports.filter {
            it.context.prerequisites.isEmpty() && it.segmentRootLookup.isEmpty()
        }
        val queuedReports = input.reports.filter {
            it.context.prerequisites.isNotEmpty() || it.segmentRootLookup.isNotEmpty()
        }

        // 3. Track newly accumulated package hashes this block
        val newAccumulated = mutableSetOf<JamByteArray>()

        // Add immediate reports to accumulated set
        immediateReports.forEach { report ->
            newAccumulated.add(report.packageSpec.hash)
            historicallyAccumulated.add(report.packageSpec.hash)
        }

        // 4. Build working copy of ready queue with edited dependencies
        // This preserves all existing records so we can extract accumulatable ones first
        val workingReadyQueue = MutableList<List<ReadyRecord>>(config.EPOCH_LENGTH) { slotIdx ->
            val oldRecords = preState.readyQueue.getOrNull(slotIdx) ?: emptyList()
            editReadyQueue(oldRecords, historicallyAccumulated)
        }

        // Add new queued reports to the current slot m (BEFORE extraction)
        val newRecords = queuedReports.map { report ->
            ReadyRecord(
                report = report,
                dependencies = (report.context.prerequisites +
                    report.segmentRootLookup.map { it.workPackageHash })
                    .filter { it !in historicallyAccumulated }
            )
        }
        workingReadyQueue[m] = workingReadyQueue[m] + newRecords

        // 5. Extract accumulatable reports from ready queue
        val allQueuedWithSlots = workingReadyQueue.flatMapIndexed { slotIdx, records ->
            records.map { record -> Pair(slotIdx, record) }
        }

        val (readyToAccumulate, stillQueuedWithSlots) = extractAccumulatableWithSlots(
            allQueuedWithSlots,
            historicallyAccumulated
        )

        // 6. Add ready-to-accumulate reports to accumulated set
        readyToAccumulate.forEach { report ->
            newAccumulated.add(report.packageSpec.hash)
            historicallyAccumulated.add(report.packageSpec.hash)
        }


        // 7. Rebuild ready queue with remaining records
        val newQueuedReportsNotAccumulated = stillQueuedWithSlots
            .filter { it.first == m && newRecords.any { nr -> nr.report.packageSpec.hash == it.second.report.packageSpec.hash } }
            .map { it.second }

        val finalReadyQueue = MutableList<List<ReadyRecord>>(config.EPOCH_LENGTH) { idx ->
            val i = ((m - idx) % config.EPOCH_LENGTH + config.EPOCH_LENGTH) % config.EPOCH_LENGTH
            when {
                i == 0 -> {
                    // Current slot: ONLY new queued reports from this block (old items at this slot are dropped)
                    newQueuedReportsNotAccumulated
                }

                i >= 1 && i < deltaT -> {
                    // Slots that wrapped around - clear them
                    emptyList()
                }

                else -> {
                    // Other slots: keep remaining items that weren't accumulated
                    stillQueuedWithSlots.filter { it.first == idx }.map { it.second }
                }
            }
        }

        // 8. Execute PVM for all accumulated reports
        val allToAccumulate = immediateReports + readyToAccumulate
        val partialState = preState.toPartialState()

        val execResult = executeAccumulation(
            partialState = partialState,
            reports = allToAccumulate,
            timeslot = input.slot,
            entropy = preState.entropy
        )
        val newPartialState = execResult.postState
        val gasUsedPerService = execResult.gasUsedMap
        val commitments = execResult.commitments

        // Debug: print gas used per service
        if (gasUsedPerService.isNotEmpty()) {
            println("[DEBUG-GAS] slot=${input.slot}, gasUsedPerService=$gasUsedPerService, commitments=${commitments.mapValues { it.value.toHex() }}")
        }

        // 9. Rotate accumulated array (sliding window: always shift by 1, add new at end)
        val newAccumulatedList = newAccumulated.toList().sortedBy { it.toHex() }
        val newAccumulatedArray = MutableList(config.EPOCH_LENGTH) { idx ->
            if (idx == config.EPOCH_LENGTH - 1) {
                // New items at last position
                newAccumulatedList
            } else {
                // Shift left by 1: position i gets what was at position i+1
                preState.accumulated.getOrNull(idx + 1) ?: emptyList()
            }
        }

        // 10. Update statistics (for accumulated field tracking)
        val workItemsPerService = countWorkItemsPerService(allToAccumulate)
        val newStatistics = updateStatistics(
            preState.statistics,
            gasUsedPerService,
            workItemsPerService
        )

        // 11. Build accumulation stats for fresh service statistics computation
        // Must include ALL services that accumulated (including always-accumulate services)
        val accumulationStats: AccumulationStats = gasUsedPerService.mapValues { (serviceId, gasUsed) ->
            val count = workItemsPerService[serviceId] ?: 0
            Pair(gasUsed, count)
        }

        // 12. Build final state (accounts and statistics are val, so need to create new state)
        val finalState = AccumulationState(
            slot = input.slot,
            entropy = preState.entropy.copy(),
            readyQueue = finalReadyQueue,
            accumulated = newAccumulatedArray,
            privileges = preState.privileges.copy(),
            statistics = newStatistics,
            accounts = newPartialState.toAccumulationServiceItems()
        )

        // 13. Compute commitment root from yields
        val outputHash = computeCommitmentRoot(commitments)

        return Pair(finalState, AccumulationOutput(ok = outputHash, accumulationStats = accumulationStats))
    }

    /**
     * Compute the Keccak Merkle root of service commitments.
     * Sorted by service index, then Merklized with Keccak.
     */
    private fun computeCommitmentRoot(commitments: Map<Long, JamByteArray>): JamByteArray {
        if (commitments.isEmpty()) {
            return JamByteArray(ByteArray(32) { 0 })
        }

        // Sort by service index and encode each commitment
        val sortedCommitments = commitments.entries.sortedBy { it.key }
        val nodes = sortedCommitments.map { (serviceId, hash) ->
            // Encode service index as 4-byte LE + 32-byte hash
            val buffer = ByteBuffer.allocate(4 + 32).order(ByteOrder.LITTLE_ENDIAN)
            buffer.putInt(serviceId.toInt())
            buffer.put(hash.bytes)
            buffer.array()
        }

        // Binary Merkle tree with Keccak-256
        return JamByteArray(binaryMerklize(nodes))
    }

    /**
     * Binary Merkle tree with Keccak-256.
     */
    private fun binaryMerklize(leaves: List<ByteArray>): ByteArray {
        if (leaves.isEmpty()) {
            return ByteArray(32) { 0 }
        }
        if (leaves.size == 1) {
            return keccak256(leaves[0])
        }

        // Pad to power of 2
        val paddedLeaves = leaves.toMutableList()
        while (paddedLeaves.size and (paddedLeaves.size - 1) != 0) {
            paddedLeaves.add(ByteArray(32) { 0 })
        }

        // Hash leaves
        var currentLevel = paddedLeaves.map { keccak256(it) }

        // Build tree upward
        while (currentLevel.size > 1) {
            val nextLevel = mutableListOf<ByteArray>()
            for (i in currentLevel.indices step 2) {
                val left = currentLevel[i]
                val right = if (i + 1 < currentLevel.size) currentLevel[i + 1] else ByteArray(32) { 0 }
                nextLevel.add(keccak256(left + right))
            }
            currentLevel = nextLevel
        }

        return currentLevel[0]
    }

    private fun keccak256(data: ByteArray): ByteArray {
        val digest = Keccak.Digest256()
        digest.update(data, 0, data.size)
        return digest.digest()
    }

    /**
     * Extract accumulatable reports while preserving slot information.
     */
    private fun extractAccumulatableWithSlots(
        queueWithSlots: List<Pair<Int, ReadyRecord>>,
        initiallyAccumulated: Set<JamByteArray>
    ): Pair<List<WorkReport>, List<Pair<Int, ReadyRecord>>> {
        val accumulated = initiallyAccumulated.toMutableSet()
        val result = mutableListOf<WorkReport>()
        var remaining = queueWithSlots.toList()

        do {
            val (ready, notReady) = remaining.partition { (_, record) ->
                record.dependencies.all { it in accumulated }
            }
            if (ready.isEmpty()) break

            ready.forEach { (_, record) ->
                result.add(record.report)
                accumulated.add(record.report.packageSpec.hash)
            }
            remaining = notReady
        } while (true)

        return Pair(result, remaining)
    }

    /**
     * Result of accumulation execution including yields for commitment calculation.
     */
    data class AccumulationExecResult(
        val postState: PartialState,
        val gasUsedMap: Map<Long, Long>,
        val commitments: Map<Long, JamByteArray>  // service -> yield hash
    )

    /**
     * Execute PVM accumulation for all reports.
     */
    private fun executeAccumulation(
        partialState: PartialState,
        reports: List<WorkReport>,
        timeslot: Long,
        entropy: JamByteArray
    ): AccumulationExecResult {
        val gasUsedMap = mutableMapOf<Long, Long>()
        val commitments = mutableMapOf<Long, JamByteArray>()
        var currentState = partialState

        // Group work items by service, preserving report order
        val serviceOperands = mutableMapOf<Long, MutableList<AccumulationOperand>>()

        for (report in reports) {
            for (result in report.results) {
                val operand = OperandTuple(
                    packageHash = report.packageSpec.hash,
                    segmentRoot = report.packageSpec.exportsRoot,
                    authorizerHash = report.authorizerHash,
                    payloadHash = result.payloadHash,
                    gasLimit = result.accumulateGas,
                    authTrace = report.authOutput,
                    result = result.result,
                    codeHash = result.codeHash
                )
                serviceOperands.computeIfAbsent(result.serviceId) { mutableListOf() }
                    .add(AccumulationOperand.WorkItem(operand))
            }
        }

        // Collect all services to accumulate (from reports + always-accumulate + transfers)
        val servicesToAccumulate = mutableSetOf<Long>()
        servicesToAccumulate.addAll(serviceOperands.keys)
        servicesToAccumulate.addAll(partialState.alwaysAccers.keys)

        // If no services to process, return empty result
        if (servicesToAccumulate.isEmpty()) {
            return AccumulationExecResult(partialState, emptyMap(), emptyMap())
        }

        // Execute for each service in sorted order
        for (serviceId in servicesToAccumulate.sorted()) {
            // Get operands for this service (may be empty for always-accumulate services)
            val operands = serviceOperands[serviceId] ?: mutableListOf()

            // Calculate total gas limit for this service batch
            // Include always-accumulate gas + work item gas + transfer gas
            val alwaysAccGas = partialState.alwaysAccers[serviceId] ?: 0L
            val workItemGas = operands.filterIsInstance<AccumulationOperand.WorkItem>()
                .sumOf { it.operand.gasLimit }
            val transferGas = operands.filterIsInstance<AccumulationOperand.Transfer>()
                .sumOf { it.transfer.gasLimit }
            val totalGasLimit = workItemGas + alwaysAccGas + transferGas

            val execResult = executor.executeService(
                partialState = currentState,
                timeslot = timeslot,
                serviceId = serviceId,
                gasLimit = totalGasLimit,
                entropy = entropy,
                operands = operands
            )

            currentState = execResult.postState
            gasUsedMap[serviceId] = (gasUsedMap[serviceId] ?: 0L) + execResult.gasUsed

            // Collect yield/commitment if present
            execResult.yield?.let { commitments[serviceId] = it }
        }

        return AccumulationExecResult(currentState, gasUsedMap, commitments)
    }

    /**
     * Update service statistics with accumulation results.
     */
    private fun updateStatistics(
        existing: List<ServiceStatisticsEntry>,
        gasUsedPerService: Map<Long, Long>,
        workItemsPerService: Map<Long, Int>
    ): List<ServiceStatisticsEntry> {
        val statsMap = existing.associateBy { it.id }.toMutableMap()

        for ((serviceId, gasUsed) in gasUsedPerService) {
            val workItems = workItemsPerService[serviceId] ?: 0
            val current = statsMap[serviceId]

            if (current != null) {
                statsMap[serviceId] = ServiceStatisticsEntry(
                    id = serviceId,
                    record = current.record.copy(
                        accumulateCount = current.record.accumulateCount + workItems,
                        accumulateGasUsed = current.record.accumulateGasUsed + gasUsed
                    )
                )
            } else {
                statsMap[serviceId] = ServiceStatisticsEntry(
                    id = serviceId,
                    record = ServiceActivityRecord(
                        accumulateCount = workItems.toLong(),
                        accumulateGasUsed = gasUsed
                    )
                )
            }
        }

        return statsMap.values.sortedBy { it.id }
    }
}
