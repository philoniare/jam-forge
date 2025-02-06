package io.forge.jam.safrole.accumulation

import blakeHash
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.WorkReport
import io.forge.jam.safrole.report.ServiceItem

class AccumulationStateTransition(private val config: AccumulationConfig) {

    private fun partitionReports(
        reports: List<WorkReport>
    ): Pair<List<WorkReport>, List<WorkReport>> {
        val immediate = reports.filter { report ->
            report.context.prerequisites.isEmpty() &&
                report.segmentRootLookup.isEmpty()
        }
        val queued = reports.filter { report ->
            !immediate.contains(report)
        }
        println("Partitioned reports: immediate = ${immediate.map { it.packageSpec.hash.toHex() }}, queued = ${queued.map { it.packageSpec.hash.toHex() }}")
        return Pair(immediate, queued)
    }

    /**
     * Update a single slot’s ready records:
     * - Convert newReports into ReadyRecords.
     * - Combine with existing records.
     * - For each record, remove dependencies that appear in accumulatedHashes.
     * - If the record ends up with no dependencies, drop it.
     * - Finally, deduplicate by the report hash.
     */
    private fun updateReadyQueueSlot(
        newReports: List<WorkReport>,
        existingRecords: List<ReadyRecord>,
        accumulatedHashes: Set<JamByteArray>
    ): List<ReadyRecord> {
        val newReadyRecords = newReports.map { report ->
            val hash = JamByteArray(getReportHash(report))
            println("New report converted to ReadyRecord: ${report.packageSpec.hash.toHex()} computed hash = ${hash.toHex()}")
            ReadyRecord(
                report = report,
                dependencies = report.context.prerequisites.map {
                    val depHex = JamByteArray(it.bytes).toHex()
                    println("  New dependency: $depHex")
                    JamByteArray(it.bytes)
                }
            )
        }
        val combined = existingRecords + newReadyRecords
        println("Combined ready records count: ${combined.size}")

        val filtered = combined.mapNotNull { record ->
            val reportHash = JamByteArray(getReportHash(record.report))
            println("Processing ReadyRecord for report ${record.report.packageSpec.hash.toHex()}, computed hash = ${reportHash.toHex()}")
            record.dependencies.forEach { dep ->
                println("  Dependency ${dep.toHex()} is ${if (accumulatedHashes.contains(dep)) "accumulated" else "not accumulated"}")
            }
            val updatedDeps = record.dependencies.filter { dep ->
                !accumulatedHashes.contains(dep)
            }
            println("  Updated dependencies: ${updatedDeps.map { it.toHex() }}")
            if (updatedDeps.isEmpty()) {
                println("  Dropping record for ${record.report.packageSpec.hash.toHex()} (no unsatisfied dependencies)")
                null
            } else {
                record.copy(dependencies = updatedDeps)
            }
        }
        println("Filtered ready records count (before deduplication): ${filtered.size}")
        val deduped = filtered.distinctBy { rec ->
            val h = JamByteArray(getReportHash(rec.report))
            println("Deduplication key for record ${rec.report.packageSpec.hash.toHex()} is ${h.toHex()}")
            h
        }
        println("Deduplicated ready records count: ${deduped.size}")
        return deduped
    }

    /**
     * Shift the entire ready queue from the pre-state.
     * For each slot index, if its “age” (relative to the current slot index m)
     * is less than diff, drop its records; otherwise update them.
     */
    private fun shiftReadyQueue(
        readyQueue: List<List<ReadyRecord>>,
        inputSlot: Long,
        diff: Int,
        accumulatedHashes: Set<JamByteArray>
    ): List<List<ReadyRecord>> {
        val epoch = config.EPOCH_LENGTH
        val newQueue = MutableList(epoch) { emptyList<ReadyRecord>() }
        val m = (inputSlot.toInt() % epoch)
        println("Shifting ready queue: inputSlot = $inputSlot, epoch = $epoch, current slot index m = $m, diff = $diff")
        for (i in 0 until epoch) {
            val age = if (m >= i) m - i else m + epoch - i
            println("Slot $i has age $age relative to current slot index $m")
            if (age < diff) {
                println("  Dropping slot $i (age $age < diff $diff)")
                newQueue[i] = emptyList()
            } else {
                newQueue[i] = updateReadyQueueSlot(
                    newReports = emptyList(),
                    existingRecords = readyQueue[i],
                    accumulatedHashes = accumulatedHashes
                )
                println("  Updated slot $i now has ${newQueue[i].size} records")
            }
        }
        return newQueue
    }

    private fun getAccumulatableReports(
        immediateReports: List<WorkReport>,
        readyQueue: List<List<ReadyRecord>>,
        accumulatedHashes: Set<JamByteArray>
    ): List<WorkReport> {
        val accumulatable = immediateReports.toMutableList()
        println("Accumulating reports from ready queue:")
        readyQueue.forEachIndexed { slot, queueSlot ->
            queueSlot.forEach { record ->
                println("  Slot $slot: record ${record.report.packageSpec.hash.toHex()} with dependencies ${record.dependencies.map { it.toHex() }}")
                if (record.dependencies.all { dep -> accumulatedHashes.contains(dep) }) {
                    println("    -> All dependencies satisfied, adding report ${record.report.packageSpec.hash.toHex()}")
                    accumulatable.add(record.report)
                }
            }
        }
        return accumulatable
    }

    private fun getAccumulatedHashes(
        accumulated: List<List<JamByteArray>>
    ): Set<JamByteArray> {
        val hashes = accumulated.flatten().toSet()
        println("Current accumulated hashes: ${hashes.map { it.toHex() }}")
        return hashes
    }

    private fun getReportHash(report: WorkReport): ByteArray {
        val encoded = report.encode()
        val hash = blakeHash(encoded)
        println("Computed hash for report ${report.packageSpec.hash.toHex()}: ${JamByteArray(hash).toHex()}")
        return hash
    }

    private fun updateAccounts(
        accounts: List<ServiceItem>,
        reports: List<WorkReport>
    ) {
        // TODO: Implement account updates based on work report accumulation
    }

    fun transition(
        input: AccumulationInput,
        preState: AccumulationState
    ): Pair<AccumulationState, AccumulationOutput> {
        println("\n=== Starting Transition ===")
        println("Input slot: ${input.slot}")
        println("Input reports: ${input.reports.map { it.packageSpec.hash.toHex() }}")
        println("Pre-state accumulated: ${preState.accumulated.map { slot -> slot.map { it.toHex() } }}")
        println("Pre-state ready queue sizes: ${preState.readyQueue.map { it.size }}")

        val postState = preState.deepCopy()
        val diff = (input.slot - preState.slot).toInt()
        val epoch = config.EPOCH_LENGTH

        // Use pre-state accumulated hashes
        var accumulatedHashes = getAccumulatedHashes(postState.accumulated)
        val (immediateReports, queuedReports) = partitionReports(input.reports)

        // Shift the ready queue based on pre-state accumulated hashes
        val shiftedQueue = shiftReadyQueue(
            readyQueue = postState.readyQueue,
            inputSlot = input.slot,
            diff = diff,
            accumulatedHashes = accumulatedHashes
        ).toMutableList()

        val m = (input.slot.toInt() % epoch)
        println("Updating current slot $m with queued reports: ${queuedReports.map { it.packageSpec.hash.toHex() }}")
        val updatedCurrentSlot = updateReadyQueueSlot(
            newReports = queuedReports,
            existingRecords = shiftedQueue[m],
            accumulatedHashes = accumulatedHashes
        )
        shiftedQueue[m] = updatedCurrentSlot
        postState.readyQueue = shiftedQueue

        // Determine accumulatable reports using the current (pre-new accumulation) accumulatedHashes.
        val accumulatableReports = getAccumulatableReports(
            immediateReports = immediateReports,
            readyQueue = postState.readyQueue,
            accumulatedHashes = accumulatedHashes
        )
        println("Accumulatable reports: ${accumulatableReports.map { it.packageSpec.hash.toHex() }}")

        // Process accumulation: update the current slot’s accumulated list with new work-report hashes.
        postState.accumulated[m] = accumulatableReports.map { report ->
            JamByteArray(getReportHash(report))
        }

        // Now update the accumulatedHashes to include the new accumulations.
        accumulatedHashes = getAccumulatedHashes(postState.accumulated)

        // **Critical fix:** Re-update the ready queue using the new accumulatedHashes,
        // so that dependencies that are now satisfied (by the new accumulation) are removed.
        postState.readyQueue = postState.readyQueue.map { slotRecords ->
            updateReadyQueueSlot(
                newReports = emptyList(),
                existingRecords = slotRecords,
                accumulatedHashes = accumulatedHashes
            )
        }.toMutableList()

        updateAccounts(postState.accounts, accumulatableReports)
        postState.slot = input.slot

        println("\n=== End State ===")
        println("Post-state accumulated: ${postState.accumulated.map { slot -> slot.map { it.toHex() } }}")
        println("Post-state ready queue sizes: ${postState.readyQueue.map { it.size }}")
        println("=== End Transition ===\n")

        return Pair(
            postState,
            AccumulationOutput(
                ok = JamByteArray(ByteArray(32) { 0 })
            )
        )
    }
}
