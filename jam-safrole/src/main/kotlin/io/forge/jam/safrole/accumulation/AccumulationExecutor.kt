package io.forge.jam.safrole.accumulation

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.WorkReport
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.ArcBytes
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.program.ProgramParts
import io.forge.jam.pvm.program.Reg
import org.bouncycastle.crypto.digests.Blake2bDigest

/**
 * Orchestrates PVM execution for accumulation.
 * Handles service code loading, execution, and state management.
 */
class AccumulationExecutor(
    private val config: AccumulationConfig
) {
    private val engine: Engine
    private val moduleCache: MutableMap<JamByteArray, Module> = mutableMapOf()

    init {
        val engineConfig = Config.new(allowDynamicPaging = false)
        engine = Engine.new(engineConfig).getOrThrow()
    }

    /**
     * Execute accumulation for a single service.
     * Implements the Ψ_A function from Gray Paper.
     */
    fun executeService(
        partialState: PartialState,
        timeslot: Long,
        serviceId: Long,
        gasLimit: Long,
        entropy: JamByteArray,
        operands: List<AccumulationOperand>
    ): AccumulationOneResult {
        val account = partialState.accounts[serviceId]
            ?: return createEmptyResult(partialState, serviceId, timeslot, operands)

        println("[EXEC-START] service=$serviceId, initialBalance=${account.info.balance}, bytes=${account.info.bytes}, items=${account.info.items}")

        // Use the account's codeHash
        val codeHash = account.info.codeHash

        val blobStateKey = computeServiceDataStateKey(serviceId, 0xFFFFFFFEL, codeHash)
        val preimage = account.preimages[codeHash]?.bytes
            ?: partialState.rawServiceDataByStateKey[blobStateKey]?.bytes

        println(
            "[EXEC-CODE] service=$serviceId, codeHash=${
                codeHash.toHex().take(16)
            }, hasPreimagesMap=${account.preimages.containsKey(codeHash)}, rawDataKeys=${partialState.rawServiceDataByStateKey.keys.size}, blobKeyFound=${
                partialState.rawServiceDataByStateKey.containsKey(
                    blobStateKey
                )
            }"
        )

        if (preimage == null) {
            println("[EXEC-CODE] No code found for service $serviceId")
            return createEmptyResult(partialState, serviceId, timeslot, operands)
        }

        // Extract code blob from preimage (format: metaLength + metadata + code)
        val code = extractCodeBlob(preimage)
        if (code == null || code.isEmpty()) {
            return createEmptyResult(partialState, serviceId, timeslot, operands)
        }

        if (code.size > MAX_SERVICE_CODE_SIZE) {
            return createEmptyResult(partialState, serviceId, timeslot, operands)
        }

        // Apply incoming transfers to balance
        val transferBalance = operands.filterIsInstance<AccumulationOperand.Transfer>()
            .sumOf { it.transfer.amount }
        val updatedAccount = account.copy(
            info = account.info.copy(balance = account.info.balance + transferBalance)
        )
        val postTransferState = partialState.copy().also {
            it.accounts[serviceId] = updatedAccount
        }

        // Calculate initial nextAccountIndex
        val minPublicServiceIndex = config.MIN_PUBLIC_SERVICE_INDEX
        val initialIndex = calculateInitialIndex(serviceId, entropy, timeslot)
        val s = minPublicServiceIndex.toUInt()
        val modValue = UInt.MAX_VALUE - s - 255u
        val candidateIndex = s.toLong() + (initialIndex.toUInt() % modValue).toLong()
        val nextAccountIndex =
            findAvailableServiceIndex(candidateIndex, minPublicServiceIndex, postTransferState.accounts)
        println(
            "[EXEC] serviceId=$serviceId, timeslot=$timeslot, entropy(full)=${
                entropy.bytes.joinToString("") {
                    "%02x".format(
                        it
                    )
                }
            }"
        )
        println(
            "[EXEC] initialIndex=$initialIndex (0x${initialIndex.toString(16)}), modValue=$modValue (0x${
                modValue.toString(
                    16
                )
            })"
        )
        println(
            "[EXEC] candidateIndex=$candidateIndex (0x${candidateIndex.toString(16)}), nextAccountIndex=$nextAccountIndex (0x${
                nextAccountIndex.toString(
                    16
                )
            })"
        )

        // Create accumulation context with dual state
        val context = AccumulationContext(
            x = postTransferState.deepCopy(),
            y = postTransferState.deepCopy(),
            serviceIndex = serviceId,
            timeslot = timeslot,
            entropy = entropy,
            nextAccountIndex = nextAccountIndex,
            minPublicServiceIndex = minPublicServiceIndex
        )

        // Execute PVM
        val execResult = executePvm(context, code, gasLimit, operands)
        println("[EXEC-DONE] service=$serviceId, exitReason=${execResult.exitReason}, gasUsed=${execResult.gasUsed}, gasLimit=$gasLimit, codeSize=${code.size}, operands=${operands.size}, outputLen=${execResult.output?.size ?: 0}")

        // Collapse state based on exit reason
        val finalState = context.collapse(execResult.exitReason)

        // Update last_accumulation_slot for this service
        val serviceAccount = finalState.accounts[serviceId]
        if (serviceAccount != null) {
            finalState.accounts[serviceId] = serviceAccount.copy(
                info = serviceAccount.info.copy(lastAccumulationSlot = timeslot)
            )
        }

        val yield: JamByteArray? = when (execResult.exitReason) {
            ExitReason.PANIC, ExitReason.OUT_OF_GAS -> context.yieldCheckpoint
            ExitReason.HALT -> {
                val output = execResult.output
                if (output != null && output.size == 32) {
                    JamByteArray(output)
                } else {
                    // Fall back to YIELD host call result
                    context.yield
                }
            }

            else -> context.yield
        }

        val deferredTransfers = context.getDeferredTransfers(execResult.exitReason)
        println("[EXEC-RESULT] service=$serviceId, exitReason=${execResult.exitReason}, deferredTransfers=${deferredTransfers.size}")
        deferredTransfers.forEach { t ->
            println("[EXEC-RESULT]   transfer: src=${t.source}, dst=${t.destination}, amount=${t.amount}, gas=${t.gasLimit}")
        }

        return AccumulationOneResult(
            postState = finalState,
            deferredTransfers = deferredTransfers,
            yield = yield,
            gasUsed = execResult.gasUsed,
            provisions = context.getProvisions(execResult.exitReason)
        )
    }

    /**
     * Result of PVM execution including output data.
     */
    data class PvmExecResult(
        val exitReason: ExitReason,
        val gasUsed: Long,
        val output: ByteArray?  // Output from registers r7 (addr) and r8 (len) on halt
    )

    /**
     * Execute PVM code with host call handling.
     */
    private fun executePvm(
        context: AccumulationContext,
        code: ByteArray,
        gasLimit: Long,
        operands: List<AccumulationOperand>
    ): PvmExecResult {
        val module = getOrCompileModule(code)
        if (module == null) {
            return PvmExecResult(ExitReason.INVALID_CODE, 0L, null)
        }

        // Manually instantiate without step tracing (to avoid memory issues)
        val interpreted = InterpretedInstance.newFromModule(module, false)
        val backend = InstanceBackend.Interpreted(interpreted)
        val instance = RawInstance(module, backend, null)

        // Set up host call handler
        val hostCalls = AccumulationHostCalls(context, operands, config)

        // Set initial gas
        instance.setGas(gasLimit)
        val initialGas = gasLimit

        val entryPointPc = io.forge.jam.pvm.program.ProgramCounter(5u)

        // Encode input using compact encoding (Gray Paper natural numbers)
        val inputData = encodeGrayPaperNatural(context.timeslot) +
            encodeGrayPaperNatural(context.serviceIndex) +
            encodeGrayPaperNatural(operands.size.toLong())
        println(
            "[DEBUG-INPUTDATA] timeslot=${context.timeslot}, serviceIndex=${context.serviceIndex}, count=${operands.size}, bytes=${
                inputData.joinToString(
                    ""
                ) { "%02x".format(it) }
            }, size=${inputData.size}"
        )

        val RA_INIT = 0xFFFF0000uL
        val SP_INIT = 0xFEFE0000uL
        val INPUT_ADDR = 0xFEFF0000u

        instance.writeMemory(INPUT_ADDR, inputData, isExternal = true)

        instance.setReg(Reg.RA, RA_INIT)
        instance.setReg(Reg.SP, SP_INIT)
        instance.setReg(Reg.A0, INPUT_ADDR.toULong())
        instance.setReg(Reg.A1, inputData.size.toULong())

        // GP standard does not initialize A2-A5 - leave as 0
        instance.setReg(Reg.A2, 0uL)
        instance.setReg(Reg.A3, 0uL)
        instance.setReg(Reg.A4, 0uL)
        instance.setReg(Reg.A5, 0uL)


        // Set initial PC
        instance.setNextProgramCounter(entryPointPc)

        // Execute until completion
        var exitReason = ExitReason.HALT
        var hostCallCount = 0
        while (true) {
            val result = instance.run()
            if (result.isFailure) {
                exitReason = ExitReason.PANIC
                break
            }

            when (val interrupt = result.getOrNull()) {
                InterruptKind.Finished -> {
                    exitReason = ExitReason.HALT
                    break
                }

                InterruptKind.Panic -> {
                    exitReason = ExitReason.PANIC
                    break
                }

                InterruptKind.NotEnoughGas -> {
                    exitReason = ExitReason.OUT_OF_GAS
                    break
                }

                is InterruptKind.Ecalli -> {
                    hostCallCount++
                    // Deduct host call gas cost BEFORE execution
                    val gasCost = hostCalls.getGasCost(interrupt.value, instance)
                    val newGas = instance.gas() - gasCost
                    instance.setGas(newGas)
                    // Now dispatch with gas already charged
                    hostCalls.dispatch(interrupt.value, instance)
                }

                is InterruptKind.Segfault -> {
                    // val segfault = interrupt as InterruptKind.Segfault
                    // println("[SEGFAULT] service=${context.serviceIndex}, pageAddress=${segfault.fault.pageAddress}, pageSize=${segfault.fault.pageSize}")
                    exitReason = ExitReason.PAGE_FAULT
                    break
                }

                InterruptKind.Step -> {
                    // Continue for step tracing
                }

                null -> {
                    println("[PANIC-NULL] PC=${instance.programCounter()}, gas=${instance.gas()}")
                    exitReason = ExitReason.PANIC
                    break
                }
            }
        }

        val gasUsed = initialGas - instance.gas()
        val finalGas = instance.gas()
        println("[DEBUG-EXEC] hostCalls=$hostCallCount, initialGas=$initialGas, finalGas=$finalGas, gasUsed=$gasUsed, exitReason=$exitReason")

        val output: ByteArray? = if (exitReason == ExitReason.HALT) {
            val addr = instance.reg(Reg.A0).toUInt()
            val len = instance.reg(Reg.A1).toInt()
            if (len > 0) {
                val buffer = ByteArray(len)
                val result = instance.readMemoryInto(addr, buffer)
                if (result.isSuccess) buffer else null
            } else {
                ByteArray(0)
            }
        } else {
            null
        }

        return PvmExecResult(exitReason, gasUsed, output)
    }

    /**
     * Get or compile a module from code bytes.
     */
    private fun getOrCompileModule(code: ByteArray): Module? {
        val codeHash = JamByteArray(blake2b256(code))

        return moduleCache.getOrPut(codeHash) {
            try {
                var partsResult = ProgramParts.fromGenericBytes(ArcBytes.fromStatic(code))
                if (partsResult.isFailure) {
                    partsResult = ProgramParts.fromJamBytes(ArcBytes.fromStatic(code))
                }
                if (partsResult.isFailure) {
                    // Fallback: wrap raw bytes as PVM program with data in RO section
                    partsResult = Result.success(wrapAsPvm(code))
                }
                if (partsResult.isFailure) {
                    return null
                }
                val parts = partsResult.getOrNull() ?: return null
                if (parts.stackSize < 65536u) {
                    parts.stackSize = 65536u
                }
                // Ensure enough heap space for large operand lists (at least 256KB total RW)
                val minRwDataSize = 262144u // 256KB
                if (parts.rwDataSize < minRwDataSize) {
                    parts.rwDataSize = minRwDataSize
                }

                val actualRwLen =
                    if (parts.actualRwDataLen > 0u) parts.actualRwDataLen else parts.rwData.toByteArray().size.toUInt()
                parts.rwDataSize - actualRwLen

                val blobResult = ProgramBlob.fromParts(parts)
                if (blobResult.isFailure) {
                    return null
                }
                val blob = blobResult.getOrThrow()

                val moduleConfig = ModuleConfig.new(dynamicPaging = false)
                moduleConfig.setGasMetering(GasMeteringKind.Sync)
                moduleConfig.setPageSize(4096u)
                // Enable step tracing temporarily for debugging
                val enableStepTrace = true
                moduleConfig.setStepTracing(enableStepTrace)
                moduleConfig.setAuxDataSize(16908288u)

                val moduleResult = Module.fromBlob(engine, moduleConfig, blob)
                if (moduleResult.isFailure) {
                    return null
                }
                moduleResult.getOrThrow()
            } catch (e: Exception) {
                return null
            }
        }
    }


    /**
     * Create empty result when service code cannot be executed.
     * Still applies incoming transfers to the balance.
     */
    private fun createEmptyResult(
        state: PartialState,
        serviceId: Long? = null,
        timeslot: Long? = null,
        operands: List<AccumulationOperand> = emptyList()
    ): AccumulationOneResult {
        val finalState = if (serviceId != null) {
            val stateCopy = state.deepCopy()
            val account = stateCopy.accounts[serviceId]
            if (account != null) {
                val transferBalance = operands.filterIsInstance<AccumulationOperand.Transfer>()
                    .sumOf { it.transfer.amount }
                stateCopy.accounts[serviceId] = account.copy(
                    info = account.info.copy(
                        balance = account.info.balance + transferBalance
                    )
                )
            }
            stateCopy
        } else {
            state
        }

        return AccumulationOneResult(
            postState = finalState,
            deferredTransfers = emptyList(),
            yield = null,
            gasUsed = 0L,
            provisions = emptySet()
        )
    }

    companion object {
        const val MAX_SERVICE_CODE_SIZE = 4 * 1024 * 1024 // 4MB

        /**
         * Simple Blake2b-256 hash.
         */
        private fun blake2b256(data: ByteArray): ByteArray {
            val digest = org.bouncycastle.jcajce.provider.digest.Blake2b.Blake2b256()
            digest.update(data, 0, data.size)
            return digest.digest()
        }

        /**
         * Encode accumulate arguments in JAM format.
         */
        private fun encodeAccumulateArguments(timeslot: Long, serviceId: Long, operandCount: Int): ByteArray {
            val buffer = java.nio.ByteBuffer.allocate(24) // 3 * 8 bytes
            buffer.order(java.nio.ByteOrder.LITTLE_ENDIAN)
            buffer.putLong(serviceId)
            buffer.putLong(timeslot)
            buffer.putLong(operandCount.toLong())
            return buffer.array()
        }

        private fun wrapAsPvm(code: ByteArray): ProgramParts {
            return ProgramParts(
                isaKind = InstructionSetKind.JamV1,
                is64Bit = true,
                roDataSize = code.size.toUInt(),
                roData = ArcBytes.fromStatic(code),
                codeAndJumpTable = ArcBytes.fromStatic(ByteArray(0))
            )
        }

        /**
         * Extract code blob from preimage data.
         * Preimage format: metaLength (Gray Paper natural number) + metadata + codeBlob
         *
         * Natural number encoding: count leading zeros of inverted first byte to determine
         * how many additional bytes to read, then combine with masked bits.
         */
        private fun extractCodeBlob(preimage: ByteArray): ByteArray? {
            if (preimage.isEmpty()) return null

            val firstByte = preimage[0].toInt() and 0xFF

            if (firstByte == 0) {
                // Metadata length is 0, code starts at byte 1
                return preimage.copyOfRange(1, preimage.size)
            }

            // Count leading zeros of inverted byte
            val inverted = firstByte.inv() and 0xFF
            var byteLength = 0
            for (i in 0 until 8) {
                if ((inverted and (0x80 shr i)) != 0) break
                byteLength++
            }

            if (preimage.size < 1 + byteLength) return null

            // Read additional bytes (little-endian)
            var res: Long = 0
            for (i in 0 until byteLength) {
                res = res or ((preimage[1 + i].toLong() and 0xFF) shl (8 * i))
            }

            // Mask for top bits from first byte
            val mask = (1 shl (8 - byteLength)) - 1
            val topBits = firstByte and mask

            val metaLength = (res + (topBits.toLong() shl (8 * byteLength))).toInt()
            val metaLengthSize = 1 + byteLength
            val codeStart = metaLengthSize + metaLength

            if (codeStart > preimage.size) return null

            return preimage.copyOfRange(codeStart, preimage.size)
        }
    }
}

/**
 * Execute sequential accumulation
 * Processes work reports sequentially, respecting gas budget.
 */
fun accumulateSequential(
    gasLimit: Long,
    deferredTransfers: List<DeferredTransfer>,
    reports: List<WorkReport>,
    partialState: PartialState,
    freeGas: Map<Long, Long>,
    executor: AccumulationExecutor,
    timeslot: Long,
    entropy: JamByteArray,
    config: AccumulationConfig
): AccumulationSeqResult {
    if (reports.isEmpty() && deferredTransfers.isEmpty() && freeGas.isEmpty()) {
        return AccumulationSeqResult(
            reportsAccumulated = 0,
            postState = partialState,
            outputs = emptyMap(),
            gasUsed = emptyList()
        )
    }

    // Calculate how many reports can fit in gas budget
    var totalGas = 0L
    var reportsToProcess = 0
    for (report in reports) {
        val reportGas = report.results.sumOf { it.accumulateGas }
        if (totalGas + reportGas <= gasLimit) {
            totalGas += reportGas
            reportsToProcess++
        } else {
            break
        }
    }

    // Execute parallel accumulation for this batch
    val (newState, newTransfers, outputs, gasUsedList) = accumulateParallel(
        partialState = partialState,
        deferredTransfers = deferredTransfers,
        reports = reports.take(reportsToProcess),
        freeGas = freeGas,
        executor = executor,
        timeslot = timeslot,
        entropy = entropy,
        config = config
    )

    // Recursively process remaining reports with updated gas budget and transfers
    val remainingGas = gasLimit - gasUsedList.sumOf { it.second } + deferredTransfers.sumOf { it.gasLimit }

    if (reportsToProcess < reports.size && remainingGas > 0) {
        val recursiveResult = accumulateSequential(
            gasLimit = remainingGas,
            deferredTransfers = newTransfers,
            reports = reports.drop(reportsToProcess),
            partialState = newState,
            freeGas = emptyMap(), // Free gas only applies to first iteration
            executor = executor,
            timeslot = timeslot,
            entropy = entropy,
            config = config
        )

        return AccumulationSeqResult(
            reportsAccumulated = reportsToProcess + recursiveResult.reportsAccumulated,
            postState = recursiveResult.postState,
            outputs = outputs + recursiveResult.outputs,
            gasUsed = gasUsedList + recursiveResult.gasUsed
        )
    }

    return AccumulationSeqResult(
        reportsAccumulated = reportsToProcess,
        postState = newState,
        outputs = outputs,
        gasUsed = gasUsedList
    )
}

/**
 * Execute parallel accumulation
 * Aggregates work items per service and executes in parallel.
 */
fun accumulateParallel(
    partialState: PartialState,
    deferredTransfers: List<DeferredTransfer>,
    reports: List<WorkReport>,
    freeGas: Map<Long, Long>,
    executor: AccumulationExecutor,
    timeslot: Long,
    entropy: JamByteArray,
    config: AccumulationConfig
): AccumulationParResult {
    // Collect all services that need accumulation
    val services = mutableSetOf<Long>()

    // Services from work reports
    reports.forEach { report ->
        report.results.forEach { result ->
            services.add(result.serviceId)
        }
    }

    // Services from free gas
    services.addAll(freeGas.keys)

    // Services from transfers
    deferredTransfers.forEach { transfer ->
        services.add(transfer.destination)
    }

    // Execute each service
    val allOutputs = mutableMapOf<Long, JamByteArray>()
    val allGasUsed = mutableListOf<Pair<Long, Long>>()
    val allTransfers = mutableListOf<DeferredTransfer>()
    var currentState = partialState

    for (serviceId in services.sorted()) {
        // Build operands for this service
        val operands = mutableListOf<AccumulationOperand>()

        // Add transfers destined for this service
        deferredTransfers.filter { it.destination == serviceId }.forEach {
            operands.add(AccumulationOperand.Transfer(it))
        }

        // Add work items for this service
        val tuples = extractOperandTuples(reports, serviceId)
        tuples.forEach {
            operands.add(AccumulationOperand.WorkItem(it))
        }

        // Calculate gas limit
        val gasLimit = calculateServiceGasLimit(reports, deferredTransfers, freeGas, serviceId)

        // Execute
        val result = executor.executeService(
            partialState = currentState,
            timeslot = timeslot,
            serviceId = serviceId,
            gasLimit = gasLimit,
            entropy = entropy,
            operands = operands
        )

        // Merge results
        currentState = result.postState
        allGasUsed.add(Pair(serviceId, result.gasUsed))
        allTransfers.addAll(result.deferredTransfers)
        result.yield?.let { allOutputs[serviceId] = it }
    }

    return AccumulationParResult(
        postState = currentState,
        deferredTransfers = allTransfers,
        outputs = allOutputs,
        gasUsed = allGasUsed
    )
}

/**
 * Calculate the initial index for nextAccountIndex using Blake2b256.
 * Returns the first 4 bytes as a UInt32 (little endian).
 */
private fun calculateInitialIndex(serviceId: Long, entropy: JamByteArray, timeslot: Long): Long {
    // Encode: serviceId (compact natural), entropy (32 bytes), timeslot (compact natural)
    // JAM encoding uses compact/variable-width natural numbers
    val encodedServiceId = encodeGrayPaperNatural(serviceId)
    val encodedTimeslot = encodeGrayPaperNatural(timeslot)
    val data = encodedServiceId +
        entropy.bytes +
        encodedTimeslot
    println(
        "[CALC-INDEX] serviceId=$serviceId (${encodedServiceId.joinToString("") { "%02x".format(it) }}), timeslot=$timeslot (${
            encodedTimeslot.joinToString(
                ""
            ) { "%02x".format(it) }
        }), entropy=${entropy.bytes.take(8).joinToString("") { "%02x".format(it) }}..."
    )

    // Hash with Blake2b-256
    val digest = Blake2bDigest(256)
    digest.update(data, 0, data.size)
    val hash = ByteArray(32)
    digest.doFinal(hash, 0)

    println("[CALC-INDEX] inputData(${data.size} bytes)=${data.joinToString("") { "%02x".format(it) }}")
    println("[CALC-INDEX] hashResult=${hash.joinToString("") { "%02x".format(it) }}")

    // Take first 4 bytes as little-endian UInt32
    val result = (hash[0].toLong() and 0xFF) or
        ((hash[1].toLong() and 0xFF) shl 8) or
        ((hash[2].toLong() and 0xFF) shl 16) or
        ((hash[3].toLong() and 0xFF) shl 24)
    println("[CALC-INDEX] UInt32 result=$result (0x${result.toString(16)})")
    return result
}

/**
 * Encode a non-negative integer using Gray Paper natural number format.
 */
private fun encodeGrayPaperNatural(value: Long): ByteArray {
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

/**
 * Find the first available service index starting from a candidate.
 */
private fun findAvailableServiceIndex(
    candidate: Long,
    minPublicServiceIndex: Long,
    accounts: Map<Long, ServiceAccount>
): Long {
    var i = candidate
    val s = minPublicServiceIndex
    val right = (UInt.MAX_VALUE - s.toUInt() - 255u).toLong()

    // Loop until we find an unused service index
    while (accounts.containsKey(i)) {
        val left = (i - s + 1)
        i = s + (left % right)
    }
    return i
}
