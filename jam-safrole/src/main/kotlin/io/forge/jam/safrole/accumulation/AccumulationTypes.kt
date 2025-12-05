package io.forge.jam.safrole.accumulation

import io.forge.jam.core.ExecutionResult
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.WorkReport
import io.forge.jam.safrole.report.ServiceInfo
import org.bouncycastle.jcajce.provider.digest.Blake2b
import java.nio.ByteBuffer
import java.nio.ByteOrder

/**
 * Computes a generic service data state key.
 * Result is a 31-byte state key.
 *
 * @param serviceIndex Service index (UInt32)
 * @param discriminator Discriminator value (UInt32):
 *   - UInt32.max (0xFFFFFFFF) for storage keys
 *   - UInt32.max - 1 (0xFFFFFFFE) for preimage blob keys
 *   - preimage length for preimage info keys
 * @param data The data to hash (storage key or preimage hash)
 */
fun computeServiceDataStateKey(serviceIndex: Long, discriminator: Long, data: JamByteArray): JamByteArray {
    val valEncoded = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(discriminator.toInt()).array()

    // h = valEncoded + data
    val h = valEncoded + data.bytes

    // a = blake2b256(h)
    val digest = Blake2b.Blake2b256()
    digest.update(h, 0, h.size)
    val a = digest.digest()

    // Construct the state key by interleaving service index with hash
    val serviceBytes = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(serviceIndex.toInt()).array()
    val stateKey = ByteArray(31)

    // First 8 bytes: interleave service index with hash
    for (i in 0 until 4) {
        stateKey[i * 2] = serviceBytes[i]
        stateKey[i * 2 + 1] = a[i]
    }

    // Remaining bytes from hash (bytes 4-26, which is 23 bytes)
    System.arraycopy(a, 4, stateKey, 8, 23)

    return JamByteArray(stateKey)
}

/**
 * Computes the state key for service storage.
 * Result is a 31-byte state key.
 */
fun computeStorageStateKey(serviceIndex: Long, storageKey: JamByteArray): JamByteArray {
    return computeServiceDataStateKey(serviceIndex, 0xFFFFFFFFL, storageKey)
}

/**
 * Computes the state key for preimage info entry.
 * Uses preimage length as discriminator.
 */
fun computePreimageInfoStateKey(serviceIndex: Long, length: Int, preimageHash: JamByteArray): JamByteArray {
    return computeServiceDataStateKey(serviceIndex, length.toLong(), preimageHash)
}

/**
 * Encodes a preimage info value (list of 0-3 timeslots).
 * Format: compact length + 4-byte LE timeslot values
 */
fun encodePreimageInfoValue(timeslots: List<Long>): JamByteArray {
    val result = mutableListOf<Byte>()

    // Compact length encoding
    val count = timeslots.size
    result.add(count.toByte())  // Simple 1-byte count for 0-3 values

    // 4-byte LE timeslot values
    for (ts in timeslots) {
        result.add((ts and 0xFF).toByte())
        result.add(((ts shr 8) and 0xFF).toByte())
        result.add(((ts shr 16) and 0xFF).toByte())
        result.add(((ts shr 24) and 0xFF).toByte())
    }

    return JamByteArray(result.toByteArray())
}

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
            val variant = encodeGrayPaperNatural(0)

            // GasLimit - Gray Paper natural number encoding (variableWidth)
            val gasLimitBytes = encodeGrayPaperNatural(op.gasLimit)

            // WorkResult - variant (UInt8) + Gray Paper natural(len) + bytes for success, or variant for error
            val ok = op.result.ok
            val resultBytes = if (ok != null) {
                val len = encodeGrayPaperNatural(ok.bytes.size.toLong())
                byteArrayOf(0) + len + ok.bytes // Tag 0 for Success (UInt8)
            } else {
                byteArrayOf(2) // Tag 2 for Panic (or other error, UInt8)
            }

            // AuthTrace (Gray Paper natural(len) + bytes)
            val authTraceLen = encodeGrayPaperNatural(op.authTrace.bytes.size.toLong())

            // Order: package, segRoot, authorizer, payload, gasLimit, workResult, authorizerTrace
            return variant + op.packageHash.bytes + op.segmentRoot.bytes + op.authorizerHash.bytes +
                op.payloadHash.bytes + gasLimitBytes + resultBytes + authTraceLen + op.authTrace.bytes
        }
    }

    data class Transfer(val transfer: DeferredTransfer) : AccumulationOperand() {
        override fun encode(): ByteArray {
            // Tag 1 for Transfer - uses compact/variableWidth encoding like OperandTuple
            // sender/destination are ServiceIndex (UInt32), amount/gasLimit are Balance/Gas (UInt64)
            return encodeGrayPaperNatural(1) +
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

        /**
         * Encode a non-negative integer using Gray Paper natural number format.
         *
         * Algorithm:
         * - For value 0: returns [0x00]
         * - For value 1-127: returns [value]
         * - For larger values: first byte has prefix indicating length, followed by LE bytes
         */
        fun encodeGrayPaperNatural(value: Long): ByteArray {
            if (value == 0L) return byteArrayOf(0)

            // Find how many 7-bit groups we need
            for (l in 0 until 8) {
                if (value < (1L shl (7 * (l + 1)))) {
                    // l is the number of additional bytes needed
                    val prefix = (256 - (1 shl (8 - l))).toByte()
                    val data = (value / (1L shl (8 * l))).toByte()
                    val firstByte = ((prefix.toInt() and 0xFF) + (data.toInt() and 0xFF)).toByte()

                    return if (l == 0) {
                        byteArrayOf(firstByte)
                    } else {
                        val result = ByteArray(1 + l)
                        result[0] = firstByte
                        for (i in 0 until l) {
                            result[1 + i] = ((value shr (8 * i)) and 0xFF).toByte()
                        }
                        result
                    }
                }
            }

            // 8 bytes needed (very large number)
            val result = ByteArray(9)
            result[0] = 0xFF.toByte()
            for (i in 0 until 8) {
                result[1 + i] = ((value shr (8 * i)) and 0xFF).toByte()
            }
            return result
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
    val alwaysAccers: MutableMap<Long, Long>,            // Always-accumulate services -> gas
    // Raw service data keyvals indexed by hashed state key (for storage/preimage lookups)
    val rawServiceDataByStateKey: MutableMap<JamByteArray, JamByteArray> = mutableMapOf()
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
            alwaysAccers = alwaysAccers.toMutableMap(),
            rawServiceDataByStateKey = rawServiceDataByStateKey.toMutableMap()
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
    val deferredTransfersCheckpoint: MutableList<DeferredTransfer> = mutableListOf(),
    val provisions: MutableSet<Pair<Long, JamByteArray>> = mutableSetOf(),
    val provisionsCheckpoint: MutableSet<Pair<Long, JamByteArray>> = mutableSetOf(),
    var yield: JamByteArray? = null,  // Accumulation output (32-byte hash) - x state
    var yieldCheckpoint: JamByteArray? = null,  // Checkpoint yield - y state (used on panic)
    var nextAccountIndex: Long = 65536,  // Next available service account index
    val minPublicServiceIndex: Long = 65536  // S_S from Gray Paper (2^16)
) {
    /**
     * Checkpoint: copy current state x to checkpoint y, including yield, provisions, and transfers.
     */
    fun checkpoint() {
        y = x.deepCopy()
        yieldCheckpoint = yield
        provisionsCheckpoint.clear()
        provisionsCheckpoint.addAll(provisions)
        deferredTransfersCheckpoint.clear()
        deferredTransfersCheckpoint.addAll(deferredTransfers)
    }

    /**
     * Collapse: select final state based on exit reason.
     * On panic or out of gas, revert to checkpoint state y.
     */
    fun collapse(exitReason: ExitReason): PartialState {
        return when (exitReason) {
            ExitReason.PANIC, ExitReason.OUT_OF_GAS -> y
            ExitReason.INVALID_CODE -> y
            else -> x
        }
    }

    /**
     * Get provisions based on exit reason.
     * On panic or out of gas, use checkpoint provisions.
     */
    fun getProvisions(exitReason: ExitReason): Set<Pair<Long, JamByteArray>> {
        return when (exitReason) {
            ExitReason.PANIC, ExitReason.OUT_OF_GAS -> provisionsCheckpoint.toSet()
            else -> provisions.toSet()
        }
    }

    /**
     * Get deferred transfers based on exit reason.
     * On panic or out of gas, use checkpoint transfers.
     */
    fun getDeferredTransfers(exitReason: ExitReason): List<DeferredTransfer> {
        return when (exitReason) {
            ExitReason.PANIC, ExitReason.OUT_OF_GAS -> deferredTransfersCheckpoint.toList()
            else -> deferredTransfers.toList()
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
                    segmentRoot = report.packageSpec.exportsRoot,
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
