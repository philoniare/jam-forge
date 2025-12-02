package io.forge.jam.safrole.accumulation

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.WorkReport

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
        val prevM = (preState.slot % config.EPOCH_LENGTH).toInt()
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

        // 4. Build the new ready queue with proper ring buffer management
        // First, copy all slots from pre_state and edit dependencies
        val newReadyQueue = MutableList<List<ReadyRecord>>(config.EPOCH_LENGTH) { slotIdx ->
            val oldRecords = preState.readyQueue.getOrNull(slotIdx) ?: emptyList()
            editReadyQueue(oldRecords, historicallyAccumulated)
        }

        // Clear slots that were skipped (from prevM+1 to m, modulo EPOCH_LENGTH)
        // These are slots older than one epoch that should be dropped
        if (deltaT >= 1) {
            for (offset in 1..deltaT.coerceAtMost(config.EPOCH_LENGTH)) {
                val clearIdx = ((prevM + offset) % config.EPOCH_LENGTH + config.EPOCH_LENGTH) % config.EPOCH_LENGTH
                newReadyQueue[clearIdx] = emptyList()
            }
        }

        // Add new queued reports to the current slot m
        val newRecords = queuedReports.map { report ->
            ReadyRecord(
                report = report,
                dependencies = (report.context.prerequisites +
                    report.segmentRootLookup.map { it.workPackageHash })
                    .filter { it !in historicallyAccumulated }
            )
        }
        // Append to existing records at current slot
        newReadyQueue[m] = newReadyQueue[m] + newRecords

        // 5. Extract accumulatable reports from ready queue (Q function - topological sort)
        // Flatten and process all queued records
        val allQueuedWithSlots = newReadyQueue.flatMapIndexed { slotIdx, records ->
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

        // 7. Rebuild ready queue with remaining records (preserving slot positions)
        val finalReadyQueue = MutableList<List<ReadyRecord>>(config.EPOCH_LENGTH) { emptyList() }
        stillQueuedWithSlots.groupBy { it.first }.forEach { (slotIdx, pairs) ->
            finalReadyQueue[slotIdx] = pairs.map { it.second }
        }

        // 8. Execute PVM for all accumulated reports
        val allToAccumulate = immediateReports + readyToAccumulate
        val partialState = preState.toPartialState()

        val (newPartialState, gasUsedPerService) = executeAccumulation(
            partialState = partialState,
            reports = allToAccumulate,
            timeslot = input.slot,
            entropy = preState.entropy
        )

        // 9. Rotate accumulated array (sliding window: drop oldest, add new at end)
        // For multi-slot gaps, shift by deltaT positions (clear intermediate slots)
        val newAccumulatedList = newAccumulated.toList().sortedBy { it.toHex() }
        val newAccumulatedArray = if (deltaT >= config.EPOCH_LENGTH) {
            // If we jumped an entire epoch, clear everything except new items
            MutableList(config.EPOCH_LENGTH) { if (it == config.EPOCH_LENGTH - 1) newAccumulatedList else emptyList() }
        } else {
            // Shift left by deltaT, padding with empty lists, then add new at end
            val shifted = preState.accumulated.drop(deltaT) + List(deltaT - 1) { emptyList<JamByteArray>() } + listOf(newAccumulatedList)
            shifted.toMutableList()
        }

        // 10. Update statistics
        val workItemsPerService = countWorkItemsPerService(allToAccumulate)
        val newStatistics = updateStatistics(
            preState.statistics,
            gasUsedPerService,
            workItemsPerService
        )

        // 11. Build final state (accounts and statistics are val, so need to create new state)
        val finalState = AccumulationState(
            slot = input.slot,
            entropy = preState.entropy.copy(),
            readyQueue = finalReadyQueue,
            accumulated = newAccumulatedArray,
            privileges = preState.privileges.copy(),
            statistics = newStatistics,
            accounts = newPartialState.toAccumulationServiceItems()
        )

        return Pair(finalState, AccumulationOutput(ok = JamByteArray(ByteArray(32) { 0 })))
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
     * Execute PVM accumulation for all reports.
     */
    private fun executeAccumulation(
        partialState: PartialState,
        reports: List<WorkReport>,
        timeslot: Long,
        entropy: JamByteArray
    ): Pair<PartialState, Map<Long, Long>> {
        if (reports.isEmpty()) {
            return Pair(partialState, emptyMap())
        }

        val gasUsedMap = mutableMapOf<Long, Long>()
        var currentState = partialState

        // Group work items by service, preserving report order
        val serviceOperands = mutableMapOf<Long, MutableList<AccumulationOperand>>()
        
        for (report in reports) {
            for (result in report.results) {
                val operand = OperandTuple(
                    packageHash = report.packageSpec.hash,
                    segmentRoot = report.packageSpec.erasureRoot,
                    authorizerHash = report.authorizerHash,
                    payloadHash = result.payloadHash,
                    gasLimit = result.accumulateGas,
                    authTrace = report.authOutput,
                    result = result.result
                )
                serviceOperands.computeIfAbsent(result.serviceId) { mutableListOf() }
                    .add(AccumulationOperand.WorkItem(operand))
            }
        }

        println("[ACC] Services to accumulate: ${serviceOperands.keys}")

        // Execute for each service
        for ((serviceId, operands) in serviceOperands) {
            println("[ACC] Processing service $serviceId with ${operands.size} operands")
            // Calculate total gas limit for this service batch
            val totalGasLimit = operands.filterIsInstance<AccumulationOperand.WorkItem>()
                .sumOf { it.operand.gasLimit }

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
        }

        return Pair(currentState, gasUsedMap)
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
