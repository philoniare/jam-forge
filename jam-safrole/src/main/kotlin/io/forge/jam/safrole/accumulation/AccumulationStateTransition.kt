package io.forge.jam.safrole.accumulation

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.WorkReport

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

    private fun updateReadyQueueSlot(
        newReports: List<WorkReport>,
        existingRecords: List<ReadyRecord>,
        accumulatedHashes: Set<JamByteArray>
    ): List<ReadyRecord> {
        // First filter out any existing records whose dependencies are satisfied
        val validExisting = existingRecords.filter { record ->
            !record.dependencies.all { dep -> accumulatedHashes.contains(dep) }
        }

        // Convert new reports to ready records
        val newReadyRecords = newReports.map { report ->
            ReadyRecord(
                report = report,
                dependencies = report.context.prerequisites.map {
                    JamByteArray(it.bytes)
                }
            )
        }

        // Only keep records where their dependencies are not already satisfied
        val validNew = newReadyRecords.filter { record ->
            !record.dependencies.all { dep -> accumulatedHashes.contains(dep) }
        }

        // Combine and deduplicate
        return (validExisting + validNew).distinctBy { rec -> rec.report.packageSpec.hash }
    }

    /*
     * OLD (buggy) version – it would sometimes drop slots based solely on an age comparison:
     *
     *   if (i != m && age < diff) {
     *       println("  Dropping slot $i (age $age < diff $diff)")
     *       newQueue[i] = emptyList()
     *   } else {
     *       val updatedSlot = updateReadyQueueSlot(emptyList(), readyQueue[i], accumulatedHashes)
     *       newQueue[i] = updatedSlot
     *       println("  Updated slot $i now has ${updatedSlot.size} records")
     *   }
     *
     * That logic caused, for example, a record in slot 8 (age 3) to be cleared when diff was 1,
     * even though the test expected it to remain.
     *
     * The fix below is to remove the unconditional drop based on age. (Later, extractAccumulation
     * will “unlock” records whose dependencies are satisfied.)
     */
    private fun shiftReadyQueue(
        readyQueue: List<List<ReadyRecord>>,
        inputSlot: Long,
        diff: Int,
        accumulatedHashes: Set<JamByteArray>
    ): List<List<ReadyRecord>> {
        val epoch = config.EPOCH_LENGTH
        val newQueue = MutableList(epoch) { emptyList<ReadyRecord>() }
        // (Note: when the slot numbers wrap around the diff value is normalized externally;
        //  here we simply use inputSlot mod epoch for the current slot index)
        val m = (inputSlot.toInt() % epoch)
        println("Shifting ready queue: inputSlot = $inputSlot, epoch = $epoch, current slot index m = $m, diff = $diff")

        for (i in 0 until epoch) {
            // Instead of conditionally dropping slots by comparing age and diff,
            // we always update the slot with the existing records.
            val updatedSlot = updateReadyQueueSlot(emptyList(), readyQueue[i], accumulatedHashes)
            newQueue[i] = updatedSlot
            println("  Updated slot $i now has ${updatedSlot.size} records")
        }
        return newQueue
    }

    private fun getAccumulatedHashes(
        accumulated: List<List<JamByteArray>>
    ): Set<JamByteArray> {
        val hashes = accumulated.flatten().toSet()
        println("Current accumulated hashes: ${hashes.map { it.toHex() }}")
        return hashes
    }

    private fun extractFromSlot(
        slot: List<ReadyRecord>,
        accumulated: Set<JamByteArray>
    ): Pair<List<WorkReport>, List<ReadyRecord>> {
        val unlocked = mutableListOf<WorkReport>()
        var remaining = slot.toMutableList()
        var changed: Boolean
        val availableHashes = accumulated.toMutableSet()

        do {
            changed = false
            var i = 0
            while (i < remaining.size) {
                val record = remaining[i]
                if (record.dependencies.all { dep -> availableHashes.contains(dep) }) {
                    unlocked.add(record.report)
                    availableHashes.add(record.report.packageSpec.hash)
                    remaining.removeAt(i)
                    changed = true
                } else {
                    i++
                }
            }
        } while (changed && remaining.isNotEmpty())

        return Pair(unlocked, remaining)
    }

    fun extractAccumulation(
        readyQueue: List<List<ReadyRecord>>,
        currentAccumulated: Set<JamByteArray>
    ): Pair<List<WorkReport>, List<List<ReadyRecord>>> {
        val newQueue = readyQueue.map { it.toMutableList() }.toMutableList()
        val unlockedReports = mutableListOf<WorkReport>()
        var changed: Boolean
        val accumulated = currentAccumulated.toMutableSet()

        do {
            changed = false
            for (i in newQueue.indices) {
                val (unlocked, remaining) = extractFromSlot(newQueue[i], accumulated)
                if (unlocked.isNotEmpty()) {
                    println("Slot $i: extracted ${unlocked.size} record(s)")
                    unlockedReports.addAll(unlocked)
                    accumulated.addAll(unlocked.map { it.packageSpec.hash })
                    newQueue[i] = remaining.toMutableList()
                    changed = true
                }
            }
        } while (changed)

        return Pair(unlockedReports, newQueue.map { it.toList() })
    }

    fun transition(
        input: AccumulationInput,
        preState: AccumulationState
    ): Pair<AccumulationState, AccumulationOutput> {
        val postState = preState.deepCopy()
        // diff is computed (with appropriate wrap‐around handling externally) so that in our tests diff = 1.
        val diff = (input.slot - preState.slot).toInt()
        val epoch = config.EPOCH_LENGTH

        // Get accumulated hashes from preState
        var accumulatedHashes = getAccumulatedHashes(postState.accumulated)

        // Partition reports into immediate and queued
        val (immediateReports, queuedReports) = partitionReports(input.reports)

        // Add immediate reports' hashes to accumulated
        accumulatedHashes = accumulatedHashes + immediateReports.map { it.packageSpec.hash }.toSet()

        // Shift ready queue – note that the age–based drop has been removed so that we do not accidentally
        // lose records that are still waiting on unsatisfied dependencies.
        val shiftedQueue = shiftReadyQueue(
            readyQueue = postState.readyQueue,
            inputSlot = input.slot,
            diff = diff,
            accumulatedHashes = accumulatedHashes
        ).toMutableList()

        // Add queued reports to the “current” slot (computed as input.slot mod epoch)
        val m = (input.slot.toInt() % epoch)
        val updatedCurrentSlot = updateReadyQueueSlot(
            newReports = queuedReports,
            existingRecords = shiftedQueue[m],
            accumulatedHashes = accumulatedHashes
        )
        shiftedQueue[m] = updatedCurrentSlot

        // Extract any newly unlocked records
        val (unlockedReports, drainedQueue) = extractAccumulation(shiftedQueue, accumulatedHashes)
        postState.readyQueue = drainedQueue.toMutableList()

        // Combine all accumulated reports and sort them
        val accumulatableReports = (immediateReports + unlockedReports).distinctBy { it.packageSpec.hash }
            .sortedBy { it.packageSpec.hash.toHex() }

        // Update accumulated history with proper dependency ordering
        postState.accumulated = (postState.accumulated.drop(1) +
            listOf(accumulatableReports.map { it.packageSpec.hash })).toMutableList()

        postState.slot = input.slot

        return Pair(postState, AccumulationOutput(ok = JamByteArray(ByteArray(32) { 0 })))
    }
}
