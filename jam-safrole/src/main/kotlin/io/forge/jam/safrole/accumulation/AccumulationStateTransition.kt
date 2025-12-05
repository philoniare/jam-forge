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

        // 8. Execute PVM for accumulated reports (respecting gas budget)
        val allToAccumulate = immediateReports + readyToAccumulate
        val partialState = preState.toPartialState()

        // Calculate total gas budget
        val sumPrivilegedGas = partialState.alwaysAccers.values.sum()
        val minTotalGas = config.WORK_REPORT_ACCUMULATION_GAS * config.CORES_COUNT + sumPrivilegedGas
        val totalGasLimit = maxOf(config.TOTAL_ACCUMULATION_GAS, minTotalGas)

        // Execute outer accumulation with recursive deferred transfer processing
        val outerResult = outerAccumulate(
            partialState = partialState,
            transfers = emptyList(),
            workReports = allToAccumulate,
            alwaysAccers = partialState.alwaysAccers.toMap(),
            gasLimit = totalGasLimit,
            timeslot = input.slot,
            entropy = preState.entropy
        )

        // Determine which reports were actually accumulated (based on reportsAccumulated count)
        val reportsToAccumulate = allToAccumulate.take(outerResult.reportsAccumulated)

        // Rebuild newAccumulated to only include reports that will actually be accumulated
        // (after gas filtering). This is critical for correct accumulation history.
        val actuallyAccumulated = mutableSetOf<JamByteArray>()
        reportsToAccumulate.forEach { report ->
            actuallyAccumulated.add(report.packageSpec.hash)
        }

        val newPartialState = outerResult.postState
        val gasUsedPerService = outerResult.gasUsedMap
        val commitments = outerResult.commitments

        // Debug: print gas used per service
        if (gasUsedPerService.isNotEmpty()) {
            println("[DEBUG-GAS] slot=${input.slot}, gasUsedPerService=$gasUsedPerService, commitments=${commitments.mapValues { it.value.toHex() }}")
        }

        // 9. Rotate accumulated array (sliding window: always shift by 1, add new at end)
        // Use actuallyAccumulated (post gas-filtering) for correct history
        val newAccumulatedList = actuallyAccumulated.toList().sortedBy { it.toHex() }
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
        // Use reportsToAccumulate (filtered by gas budget) for correct stats
        val workItemsPerService = countWorkItemsPerService(reportsToAccumulate)
        val newStatistics = updateStatistics(
            preState.statistics,
            gasUsedPerService,
            workItemsPerService
        )

        // 11. Build accumulation stats for fresh service statistics computation
        // Only include services that actually did something (gasUsed > 0 or workItems > 0)
        // Services that only received transfers without executing are NOT included
        val accumulationStats: AccumulationStats = gasUsedPerService
            .mapValues { (serviceId, gasUsed) ->
                val count = workItemsPerService[serviceId] ?: 0
                Pair(gasUsed, count)
            }
            .filter { (_, stats) -> stats.first > 0 || stats.second > 0 }

        // 12. Update lastAccumulationSlot for all services in accumulationStats
        for ((serviceId, _) in accumulationStats) {
            val account = newPartialState.accounts[serviceId]
            if (account != null) {
                newPartialState.accounts[serviceId] = account.copy(
                    info = account.info.copy(lastAccumulationSlot = input.slot)
                )
            }
        }

        // 13. Build final state
        val finalState = AccumulationState(
            slot = input.slot,
            entropy = preState.entropy.copy(),
            readyQueue = finalReadyQueue,
            accumulated = newAccumulatedArray,
            privileges = Privileges(
                bless = newPartialState.manager,
                assign = newPartialState.assigners.toList(),
                designate = newPartialState.delegator,
                register = newPartialState.registrar,
                alwaysAcc = newPartialState.alwaysAccers.entries.sortedBy { it.key }
                    .map { (id, gas) -> AlwaysAccItem(id, gas) }
            ),
            statistics = newStatistics,
            accounts = newPartialState.toAccumulationServiceItems(),
            rawServiceDataByStateKey = newPartialState.rawServiceDataByStateKey.toMutableMap()
        )

        // 14. Compute commitment root from yields
        val outputHash = computeCommitmentRoot(commitments)

        return Pair(
            finalState,
            AccumulationOutput(ok = outputHash, accumulationStats = accumulationStats, outputs = commitments)
        )
    }

    /**
     * Compute the Keccak Merkle root of service commitments.
     * Sorted by service index, then Merklized with Keccak.
     */
    private fun computeCommitmentRoot(commitments: Map<Long, JamByteArray>): JamByteArray {
        if (commitments.isEmpty()) {
            val root = JamByteArray(ByteArray(32) { 0 })
            println("[KOTLIN-ACCUM-ROOT] nodes=0, root=${root.toHex()}")
            return root
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
        val root = JamByteArray(binaryMerklize(nodes))
        println("[KOTLIN-ACCUM-ROOT] nodes=${nodes.size}, root=${root.toHex()}")
        return root
    }

    /**
     * Well-balanced binary Merkle function
     */
    private fun binaryMerklize(leaves: List<ByteArray>): ByteArray {
        return when {
            leaves.isEmpty() -> ByteArray(32) { 0 }
            leaves.size == 1 -> keccak256(leaves[0])
            else -> {
                val result = binaryMerklizeHelper(leaves)
                when (result) {
                    is MerklizeResult.Leaf -> keccak256(result.data)
                    is MerklizeResult.Hash -> result.hash
                }
            }
        }
    }

    /**
     * Merkle result can be either a leaf (unhashed data) or a hash.
     */
    private sealed class MerklizeResult {
        data class Leaf(val data: ByteArray) : MerklizeResult()
        data class Hash(val hash: ByteArray) : MerklizeResult()

        fun toByteArray(): ByteArray = when (this) {
            is Leaf -> data
            is Hash -> hash
        }
    }

    /**
     * Helper for well-balanced binary Merkle tree.
     */
    private fun binaryMerklizeHelper(nodes: List<ByteArray>): MerklizeResult {
        return when (nodes.size) {
            0 -> MerklizeResult.Hash(ByteArray(32) { 0 })
            1 -> MerklizeResult.Leaf(nodes[0])
            else -> {
                val mid = (nodes.size + 1) / 2  // roundup of half
                val left = nodes.subList(0, mid)
                val right = nodes.subList(mid, nodes.size)
                val leftResult = binaryMerklizeHelper(left)
                val rightResult = binaryMerklizeHelper(right)
                // Hash with "node" prefix as per GP E.1.1
                MerklizeResult.Hash(
                    keccakHashWithPrefix(
                        "node".toByteArray(),
                        leftResult.toByteArray(),
                        rightResult.toByteArray()
                    )
                )
            }
        }
    }

    private fun keccak256(data: ByteArray): ByteArray {
        val digest = Keccak.Digest256()
        digest.update(data, 0, data.size)
        return digest.digest()
    }

    private fun keccakHashWithPrefix(prefix: ByteArray, left: ByteArray, right: ByteArray): ByteArray {
        val digest = Keccak.Digest256()
        digest.update(prefix, 0, prefix.size)
        digest.update(left, 0, left.size)
        digest.update(right, 0, right.size)
        return digest.digest()
    }

    /**
     * Filter reports that fit within the gas budget.
     */
    private fun filterReportsByGasBudget(
        reports: List<WorkReport>,
        gasLimit: Long
    ): List<WorkReport> {
        var sumGasRequired = 0L
        val result = mutableListOf<WorkReport>()

        println("[DEBUG-GAS-FILTER] Starting filter with ${reports.size} reports, gasLimit=$gasLimit")
        for ((reportIdx, report) in reports.withIndex()) {
            // Check each work item (digest) individually
            var canAccumulate = true
            val reportStartGas = sumGasRequired
            for ((itemIdx, workResult) in report.results.withIndex()) {
                if (workResult.accumulateGas + sumGasRequired > gasLimit) {
                    println("[DEBUG-GAS-FILTER] Report $reportIdx item $itemIdx rejected: ${workResult.accumulateGas} + $sumGasRequired > $gasLimit")
                    canAccumulate = false
                    break
                }
                sumGasRequired += workResult.accumulateGas
            }

            if (canAccumulate) {
                result.add(report)
                println(
                    "[DEBUG-GAS-FILTER] Report $reportIdx accepted: hash=${
                        report.packageSpec.hash.toHex().take(16)
                    }..., items=${report.results.size}, gas=${sumGasRequired - reportStartGas}, totalGas=$sumGasRequired"
                )
            } else {
                // Can't fit this report, stop processing
                println("[DEBUG-GAS-FILTER] Report $reportIdx rejected, stopping")
                break
            }
        }

        if (result.size < reports.size) {
            println("[DEBUG-GAS-FILTER] Filtered ${reports.size} reports to ${result.size} due to gas limit $gasLimit (used $sumGasRequired)")
        }

        return result
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
        val commitments: Map<Long, JamByteArray>,  // service -> yield hash
        val deferredTransfers: List<DeferredTransfer> = emptyList()  // transfers to process in next round
    )

    /**
     * Outer accumulation result for tracking how many reports were accumulated.
     */
    data class OuterAccumulationResult(
        val reportsAccumulated: Int,
        val postState: PartialState,
        val gasUsedMap: Map<Long, Long>,
        val commitments: Map<Long, JamByteArray>
    )

    /**
     * Outer accumulation function
     * Recursively processes work reports and deferred transfers.
     */
    private fun outerAccumulate(
        partialState: PartialState,
        transfers: List<DeferredTransfer>,
        workReports: List<WorkReport>,
        alwaysAccers: Map<Long, Long>,
        gasLimit: Long,
        timeslot: Long,
        entropy: JamByteArray
    ): OuterAccumulationResult {
        // Count how many reports can fit in gas budget
        var i = 0
        var sumGasRequired = 0L

        for (report in workReports) {
            var canAccumulate = true
            for (digest in report.results) {
                if (digest.accumulateGas + sumGasRequired > gasLimit) {
                    canAccumulate = false
                    break
                }
                sumGasRequired += digest.accumulateGas
            }
            if (canAccumulate) {
                i++
            } else {
                break
            }
        }

        val n = i + transfers.size + alwaysAccers.size

        if (n == 0) {
            println("[OUTER-ACCUM] No work to do - returning empty")
            return OuterAccumulationResult(
                reportsAccumulated = 0,
                postState = partialState,
                gasUsedMap = emptyMap(),
                commitments = emptyMap()
            )
        }

        println("[OUTER-ACCUM] Can accumulate $i reports, ${transfers.size} transfers, ${alwaysAccers.size} alwaysAcc services")

        // Execute parallel accumulation for this batch
        val parallelResult = executeAccumulation(
            partialState = partialState,
            reports = workReports.take(i),
            deferredTransfers = transfers,
            alwaysAccers = alwaysAccers,
            timeslot = timeslot,
            entropy = entropy
        )

        val parallelGasUsed = parallelResult.gasUsedMap.values.sum()
        val transfersGas = transfers.sumOf { it.gasLimit }

        // Recursively process remaining reports with new deferred transfers
        val remainingReports = workReports.drop(i)
        val newTransfers = parallelResult.deferredTransfers

        println("[OUTER-ACCUM] Parallel done: gasUsed=$parallelGasUsed, newTransfers=${newTransfers.size}, remaining=${remainingReports.size}")

        // Recursive call if there are new transfers or remaining reports
        val outerResult = outerAccumulate(
            partialState = parallelResult.postState,
            transfers = newTransfers,
            workReports = remainingReports,
            alwaysAccers = emptyMap(),  // Always-accumulate services only processed in first iteration
            gasLimit = gasLimit + transfersGas - parallelGasUsed,
            timeslot = timeslot,
            entropy = entropy
        )

        // Merge results
        val mergedGasUsed = (parallelResult.gasUsedMap.keys + outerResult.gasUsedMap.keys).associateWith { serviceId ->
            (parallelResult.gasUsedMap[serviceId] ?: 0L) + (outerResult.gasUsedMap[serviceId] ?: 0L)
        }

        return OuterAccumulationResult(
            reportsAccumulated = i + outerResult.reportsAccumulated,
            postState = outerResult.postState,
            gasUsedMap = mergedGasUsed,
            commitments = parallelResult.commitments + outerResult.commitments
        )
    }

    /**
     * Execute PVM accumulation for all reports.
     * This is the parallelized accumulation function Δ* from the Gray Paper.
     */
    private fun executeAccumulation(
        partialState: PartialState,
        reports: List<WorkReport>,
        deferredTransfers: List<DeferredTransfer>,
        alwaysAccers: Map<Long, Long>,
        timeslot: Long,
        entropy: JamByteArray
    ): AccumulationExecResult {
        val gasUsedMap = mutableMapOf<Long, Long>()
        val commitments = mutableMapOf<Long, JamByteArray>()
        val newDeferredTransfers = mutableListOf<DeferredTransfer>()
        val allProvisions = mutableSetOf<Pair<Long, JamByteArray>>()
        var currentState = partialState

        // Group work items by service, preserving report order
        val serviceOperands = mutableMapOf<Long, MutableList<AccumulationOperand>>()

        println("[DEBUG-REPORTS] Processing ${reports.size} reports, ${deferredTransfers.size} deferred transfers")
        for ((reportIdx, report) in reports.withIndex()) {
            println("[DEBUG-REPORTS] Report $reportIdx has ${report.results.size} results")
            for ((resultIdx, result) in report.results.withIndex()) {
                println("[DEBUG-REPORTS]   Result $resultIdx: serviceId=${result.serviceId}, gas=${result.accumulateGas}")
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

        // Add deferred transfers as operands for destination services
        for (transfer in deferredTransfers) {
            println("[DEBUG-TRANSFER] Adding transfer: src=${transfer.source}, dst=${transfer.destination}, amount=${transfer.amount}, gas=${transfer.gasLimit}")
            serviceOperands.computeIfAbsent(transfer.destination) { mutableListOf() }
                .add(AccumulationOperand.Transfer(transfer))
        }

        // Collect all services to accumulate (from reports + always-accumulate + transfers)
        val servicesToAccumulate = mutableSetOf<Long>()
        servicesToAccumulate.addAll(serviceOperands.keys)
        servicesToAccumulate.addAll(alwaysAccers.keys)

        println("[DEBUG-SERVICES] serviceOperands.keys=${serviceOperands.keys}, alwaysAccers=$alwaysAccers, servicesToAccumulate=$servicesToAccumulate")

        // If no services to process, return empty result
        if (servicesToAccumulate.isEmpty()) {
            return AccumulationExecResult(partialState, emptyMap(), emptyMap(), emptyList())
        }

        // Execute for each service in sorted order
        for (serviceId in servicesToAccumulate.sorted()) {
            // Get operands for this service (may be empty for always-accumulate services)
            val operands = serviceOperands[serviceId] ?: mutableListOf()

            // Calculate total gas limit for this service batch
            // Include always-accumulate gas + work item gas + transfer gas
            val alwaysAccGas = alwaysAccers[serviceId] ?: 0L
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

            // Collect new deferred transfers generated by this service
            newDeferredTransfers.addAll(execResult.deferredTransfers)

            // Collect provisions for preimage integration
            allProvisions.addAll(execResult.provisions)
        }

        // Process preimage integrations
        if (allProvisions.isNotEmpty()) {
            currentState = preimageIntegration(allProvisions, currentState, timeslot)
        }

        return AccumulationExecResult(currentState, gasUsedMap, commitments, newDeferredTransfers)
    }

    /**
     * Preimage integration function
     * For each provision, if the preimage info exists but is empty,
     * updates the preimage info with the current timeslot and stores the preimage blob.
     */
    private fun preimageIntegration(
        provisions: Set<Pair<Long, JamByteArray>>,
        state: PartialState,
        timeslot: Long
    ): PartialState {
        for ((serviceId, preimage) in provisions) {
            val account = state.accounts[serviceId] ?: continue

            // Hash the preimage to get the preimage hash
            val digest = org.bouncycastle.jcajce.provider.digest.Blake2b.Blake2b256()
            digest.update(preimage.bytes, 0, preimage.bytes.size)
            val preimageHash = JamByteArray(digest.digest())
            val length = preimage.bytes.size

            // Look up the preimage info entry
            val preimageKey = PreimageKey(preimageHash, length)
            val preimageInfo = account.preimageRequests[preimageKey]

            // If preimage info exists but is empty (solicited but not yet provided)
            if (preimageInfo != null && preimageInfo.requestedAt.isEmpty()) {
                // Update preimage info with current timeslot
                val newTimeslots = listOf(timeslot)
                account.preimageRequests[preimageKey] = PreimageRequest(newTimeslots)

                // Store the preimage blob
                account.preimages[preimageHash] = preimage

                // Update raw state data for preimage info
                val infoStateKey = computePreimageInfoStateKey(serviceId, length, preimageHash)
                state.rawServiceDataByStateKey[infoStateKey] = encodePreimageInfoValue(newTimeslots)

                // Update raw state data for preimage blob
                val blobStateKey = computeServiceDataStateKey(serviceId, 0xFFFFFFFEL, preimageHash)
                state.rawServiceDataByStateKey[blobStateKey] = preimage

                // Update service account footprint (items and bytes)
                val updatedInfo = account.info.copy(
                    items = account.info.items + 2,
                    bytes = account.info.bytes + 81 + length
                )
                state.accounts[serviceId] = account.copy(info = updatedInfo)
            }
        }
        return state
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
