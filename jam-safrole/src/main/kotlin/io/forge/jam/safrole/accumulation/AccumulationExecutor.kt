package io.forge.jam.safrole.accumulation

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.WorkReport
import io.forge.jam.core.encodeCompactInteger
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.ArcBytes
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.program.ProgramParts
import io.forge.jam.pvm.program.Reg

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
            ?: return createEmptyResult(partialState, serviceId, timeslot)

        // Use the account's codeHash
        val codeHash = account.info.codeHash

        val code = account.preimages[codeHash]?.bytes
        if (code == null) {
            return createEmptyResult(partialState, serviceId, timeslot)
        }

        if (code.size > MAX_SERVICE_CODE_SIZE) {
            return createEmptyResult(partialState, serviceId, timeslot)
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

        // Create accumulation context with dual state
        val context = AccumulationContext(
            x = postTransferState.deepCopy(),
            y = postTransferState.deepCopy(),
            serviceIndex = serviceId,
            timeslot = timeslot,
            entropy = entropy
        )

        // Execute PVM
        val (exitReason, gasUsed) = executePvm(context, code, gasLimit, operands)
        println("[EXEC-DONE] service=$serviceId, exitReason=$exitReason, gasUsed=$gasUsed")

        // Collapse state based on exit reason
        val finalState = context.collapse(exitReason)

        // Update last_accumulation_slot for this service
        val serviceAccount = finalState.accounts[serviceId]
        if (serviceAccount != null) {
            finalState.accounts[serviceId] = serviceAccount.copy(
                info = serviceAccount.info.copy(lastAccumulationSlot = timeslot)
            )
        }

        return AccumulationOneResult(
            postState = finalState,
            deferredTransfers = context.deferredTransfers.toList(),
            yield = context.yield,  // Captured from YIELD host call
            gasUsed = gasUsed,
            provisions = context.provisions.toSet()
        )
    }

    /**
     * Execute PVM code with host call handling.
     */
    private fun executePvm(
        context: AccumulationContext,
        code: ByteArray,
        gasLimit: Long,
        operands: List<AccumulationOperand>
    ): Pair<ExitReason, Long> {
        val module = getOrCompileModule(code)
        if (module == null) {
            return Pair(ExitReason.INVALID_CODE, 0L)
        }

        // Manually instantiate with step tracing
        val interpreted = InterpretedInstance.newFromModule(module, true)
        val backend = InstanceBackend.Interpreted(interpreted)
        val instance = RawInstance(module, backend, null)

        // Set up host call handler
        val hostCalls = AccumulationHostCalls(context, operands, config)

        // Set initial gas
        instance.setGas(gasLimit)
        val initialGas = gasLimit

        val entryPointPc = io.forge.jam.pvm.program.ProgramCounter(5u)

        // Encode input as 3 SCALE-compact integers: timeslot, serviceIndex, itemCount
        val inputData = java.io.ByteArrayOutputStream().use { stream ->
            stream.write(encodeCompactInteger(context.timeslot))
            stream.write(encodeCompactInteger(context.serviceIndex))
            stream.write(encodeCompactInteger(operands.size.toLong()))
            stream.toByteArray()
        }

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
                    hostCalls.dispatch(interrupt.value, instance)
                }

                is InterruptKind.Segfault -> {
                    val segfault = interrupt as InterruptKind.Segfault
                    println(
                        "[SEGFAULT] service=${context.serviceIndex}, pageAddress=0x${
                            segfault.fault.pageAddress.toString(
                                16
                            )
                        }, pageSize=${segfault.fault.pageSize}"
                    )
                    exitReason = ExitReason.PAGE_FAULT
                    break
                }

                InterruptKind.Step -> {
                    // Continue for step tracing
                }

                null -> {
                    exitReason = ExitReason.PANIC
                    break
                }
            }
        }

        val gasUsed = initialGas - instance.gas()

        if (exitReason == ExitReason.PANIC) {
            throw RuntimeException("PVM Panic! Host calls: $hostCallCount")
        }

        return Pair(exitReason, gasUsed)
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
                moduleConfig.setStepTracing(true)
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
     */
    private fun createEmptyResult(
        state: PartialState,
        serviceId: Long? = null,
        timeslot: Long? = null
    ): AccumulationOneResult {
        val finalState = if (serviceId != null && timeslot != null) {
            val stateCopy = state.deepCopy()
            val account = stateCopy.accounts[serviceId]
            if (account != null) {
                stateCopy.accounts[serviceId] = account.copy(
                    info = account.info.copy(lastAccumulationSlot = timeslot)
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
