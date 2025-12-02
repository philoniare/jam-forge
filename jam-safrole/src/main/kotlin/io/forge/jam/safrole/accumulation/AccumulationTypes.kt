package io.forge.jam.safrole.accumulation

import io.forge.jam.core.ExecutionResult
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.WorkReport
import io.forge.jam.safrole.report.ServiceInfo

/**
 * Operand tuple as defined in Gray Paper equation 106.
 * Contains extracted work item data combined with work report context.
 */
data class OperandTuple(
    val packageHash: JamByteArray,     // Work package hash
    val segmentRoot: JamByteArray,     // Segment root from availability spec
    val authorizerHash: JamByteArray,  // Authorizer hash
    val payloadHash: JamByteArray,     // Work item payload hash
    val gasLimit: Long,                // Gas limit for accumulation
    val authTrace: JamByteArray,       // Authorizer trace output
    val result: ExecutionResult,       // Refinement result (blob or error)
    val codeHash: JamByteArray         // Code hash from work result (for preimage lookup)
)

/**
 * Deferred transfer as defined in Gray Paper equation 117.
 * Represents a transfer queued during accumulation for processing in next iteration.
 */
data class DeferredTransfer(
    val source: Long,       // Source service index
    val destination: Long,  // Destination service index
    val amount: Long,       // Balance to transfer
    val memo: JamByteArray, // Memo (128 bytes)
    val gasLimit: Long      // Gas limit for on_transfer handler
) {
    companion object {
        const val MEMO_SIZE = 128
    }
}

/**
 * Accumulation input - union of OperandTuple or DeferredTransfer
 * as defined in Gray Paper equation 126.
 */
sealed class AccumulationOperand {
    abstract fun encode(): ByteArray

    data class WorkItem(val operand: OperandTuple) : AccumulationOperand() {
        override fun encode(): ByteArray {
            val op = operand

            // AccumulationInput::OperandTuple (Variant 0)
            // Standard JAM enum encoding (1 byte).
            val variant = byteArrayOf(0)

            // GasLimit - JAM compact encoded
            val gasLimitBytes = io.forge.jam.core.encodeCompactInteger(op.gasLimit)

            // WorkResult - variant 0 + JAM-compact(len) + bytes for success, or variant for error
            val ok = op.result.ok
            val resultBytes = if (ok != null) {
                val len = io.forge.jam.core.encodeCompactInteger(ok.bytes.size.toLong())
                byteArrayOf(0) + len + ok.bytes // Tag 0 for Success
            } else {
                byteArrayOf(2) // Tag 2 for Panic (or other error)
            }

            // AuthTrace (JAM-compact(len) + bytes)
            val authTraceLen = io.forge.jam.core.encodeCompactInteger(op.authTrace.bytes.size.toLong())

            // Order: package, segRoot, authorizer, payload, gasLimit, workResult, authorizerTrace
            return variant + op.packageHash.bytes + op.segmentRoot.bytes + op.authorizerHash.bytes +
                op.payloadHash.bytes + gasLimitBytes + resultBytes + authTraceLen + op.authTrace.bytes
        }
    }

    data class Transfer(val transfer: DeferredTransfer) : AccumulationOperand() {
        override fun encode(): ByteArray {
            // Prepend tag 1 for Transfer
            // sender/destination are ServiceIndex (UInt32), amount/gasLimit are Balance/Gas (UInt64)
            return byteArrayOf(1) +
                encodeU32(transfer.source.toInt()) +
                encodeU32(transfer.destination.toInt()) +
                encodeLong(transfer.amount) +
                transfer.memo.bytes +
                encodeLong(transfer.gasLimit)
        }

        private fun encodeU32(value: Int): ByteArray {
            return ByteArray(4) { i -> ((value shr (i * 8)) and 0xFF).toByte() }
        }
    }

    companion object {
        fun encodeLong(value: Long): ByteArray {
            return ByteArray(8) { i -> ((value shr (i * 8)) and 0xFF).toByte() }
        }
    }
}

/**
 * Partial state as defined in Gray Paper equation 133.
 * Contains state components both needed and mutable by the accumulation process.
 */
data class PartialState(
    val accounts: MutableMap<Long, ServiceAccount>,
    val stagingSet: MutableList<JamByteArray>,           // Validator keys
    val authQueue: MutableList<MutableList<JamByteArray>>, // Per-core auth queues
    var manager: Long,                                    // Manager service ID
    val assigners: MutableList<Long>,                    // Per-core assigners
    var delegator: Long,                                 // Delegator service ID
    var registrar: Long,                                 // Registrar service ID
    val alwaysAccers: MutableMap<Long, Long>             // Always-accumulate services -> gas
) {
    fun deepCopy(): PartialState {
        return PartialState(
            accounts = accounts.mapValues { it.value.copy() }.toMutableMap(),
            stagingSet = stagingSet.map { it.copy() }.toMutableList(),
            authQueue = authQueue.map { it.map { h -> h.copy() }.toMutableList() }.toMutableList(),
            manager = manager,
            assigners = assigners.toMutableList(),
            delegator = delegator,
            registrar = registrar,
            alwaysAccers = alwaysAccers.toMutableMap()
        )
    }
}

/**
 * Service account combining service info with mutable storage and preimages.
 */
data class ServiceAccount(
    val info: ServiceInfo,
    val storage: MutableMap<JamByteArray, JamByteArray>,    // Key -> Value storage
    val preimages: MutableMap<JamByteArray, JamByteArray>,  // Hash -> Blob preimages
    val preimageRequests: MutableMap<PreimageKey, PreimageRequest>, // Requested preimages
    var lastAccumulated: Long = 0                            // Last accumulation timestamp
) {
    fun copy(): ServiceAccount {
        return ServiceAccount(
            info = info.copy(),
            storage = storage.toMutableMap(),
            preimages = preimages.toMutableMap(),
            preimageRequests = preimageRequests.toMutableMap(),
            lastAccumulated = lastAccumulated
        )
    }
}

/**
 * Key for preimage requests (hash + length).
 */
data class PreimageKey(
    val hash: JamByteArray,
    val length: Int
)

/**
 * Preimage request state.
 */
data class PreimageRequest(
    val requestedAt: List<Long> // Timestamps when requested
)

/**
 * Result of single-service accumulation as defined in Gray Paper equation 291.
 */
data class AccumulationOneResult(
    val postState: PartialState,                          // Modified state
    val deferredTransfers: List<DeferredTransfer>,        // Outgoing transfers
    val yield: JamByteArray?,                             // Accumulation output (32-byte hash or null)
    val gasUsed: Long,                                    // Actual gas consumed
    val provisions: Set<Pair<Long, JamByteArray>>         // Service/blob pairs to provision
)

/**
 * Result of parallel accumulation.
 */
data class AccumulationParResult(
    val postState: PartialState,
    val deferredTransfers: List<DeferredTransfer>,
    val outputs: Map<Long, JamByteArray>,                 // Service -> output hash
    val gasUsed: List<Pair<Long, Long>>                   // Service -> gas used
)

/**
 * Result of sequential accumulation.
 */
data class AccumulationSeqResult(
    val reportsAccumulated: Int,
    val postState: PartialState,
    val outputs: Map<Long, JamByteArray>,
    val gasUsed: List<Pair<Long, Long>>
)

/**
 * Execution exit reason for PVM.
 */
enum class ExitReason {
    HALT,        // Normal completion
    PANIC,       // Panic (use checkpoint state)
    OUT_OF_GAS,  // Gas exhausted
    PAGE_FAULT,  // Memory access error
    HOST_CALL,   // Awaiting host call response
    INVALID_CODE // Code compilation failed
}

/**
 * Accumulation context managing dual state (x for normal, y for checkpoint).
 */
class AccumulationContext(
    var x: PartialState,  // Normal execution state
    var y: PartialState,  // Checkpoint state (used on panic)
    val serviceIndex: Long,
    val timeslot: Long,
    val entropy: JamByteArray,
    val deferredTransfers: MutableList<DeferredTransfer> = mutableListOf(),
    val provisions: MutableSet<Pair<Long, JamByteArray>> = mutableSetOf(),
    var yield: JamByteArray? = null  // Accumulation output (32-byte hash)
) {
    /**
     * Checkpoint: copy current state x to checkpoint y.
     */
    fun checkpoint() {
        y = x.deepCopy()
    }

    /**
     * Collapse: select final state based on exit reason.
     * On panic, revert to checkpoint state y.
     */
    fun collapse(exitReason: ExitReason): PartialState {
        return when (exitReason) {
            ExitReason.PANIC -> y
            ExitReason.INVALID_CODE -> y
            else -> x
        }
    }
}

/**
 * Extract operand tuples from work reports for a specific service.
 */
fun extractOperandTuples(reports: List<WorkReport>, serviceId: Long): List<OperandTuple> {
    return reports.flatMap { report ->
        report.results
            .filter { it.serviceId == serviceId }
            .map { result ->
                OperandTuple(
                    packageHash = report.packageSpec.hash,
                    segmentRoot = report.packageSpec.erasureRoot,
                    authorizerHash = report.authorizerHash,
                    payloadHash = result.payloadHash,
                    gasLimit = result.accumulateGas,
                    authTrace = report.authOutput,
                    result = result.result,
                    codeHash = result.codeHash
                )
            }
    }
}

/**
 * Calculate total gas limit for a service from work reports and transfers.
 */
fun calculateServiceGasLimit(
    reports: List<WorkReport>,
    transfers: List<DeferredTransfer>,
    freeGas: Map<Long, Long>,
    serviceId: Long
): Long {
    val freeGasAmount = freeGas[serviceId] ?: 0L
    val transferGas = transfers.filter { it.destination == serviceId }.sumOf { it.gasLimit }
    val workItemGas = reports.flatMap { r -> r.results.filter { it.serviceId == serviceId } }
        .sumOf { it.accumulateGas }
    return freeGasAmount + transferGas + workItemGas
}

/**
 * E function (Gray Paper eq. 49-61): Edit ready queue by removing accumulated reports
 * and pruning fulfilled dependencies from remaining records.
 */
fun editReadyQueue(
    queue: List<ReadyRecord>,
    accumulatedHashes: Set<JamByteArray>
): List<ReadyRecord> {
    return queue
        // Remove records whose report has already been accumulated
        .filter { record -> record.report.packageSpec.hash !in accumulatedHashes }
        // Remove fulfilled dependencies from remaining records
        .map { record ->
            ReadyRecord(
                report = record.report,
                dependencies = record.dependencies.filter { it !in accumulatedHashes }
            )
        }
}

/**
 * Q function (Gray Paper eq. 63-73): Extract accumulatable reports via topological sort.
 * Returns reports that can be accumulated (all dependencies satisfied) and remaining queued records.
 */
fun extractAccumulatableReports(
    queue: List<ReadyRecord>,
    initiallyAccumulated: Set<JamByteArray>
): Pair<List<WorkReport>, List<ReadyRecord>> {
    val accumulated = initiallyAccumulated.toMutableSet()
    val result = mutableListOf<WorkReport>()
    var remaining = queue.toList()

    do {
        val (ready, notReady) = remaining.partition { record ->
            record.dependencies.all { it in accumulated }
        }
        if (ready.isEmpty()) break

        ready.forEach { record ->
            result.add(record.report)
            accumulated.add(record.report.packageSpec.hash)
        }
        remaining = notReady
    } while (true)

    return Pair(result, remaining)
}

/**
 * Count work items per service from accumulated reports.
 */
fun countWorkItemsPerService(reports: List<WorkReport>): Map<Long, Int> {
    return reports.flatMap { report ->
        report.results.map { it.serviceId }
    }.groupingBy { it }.eachCount()
}
