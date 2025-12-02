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
        println("[EXEC] executeService called for service $serviceId")
        val account = partialState.accounts[serviceId]
        if (account == null) {
            println("[EXEC] Account $serviceId not found!")
            return createEmptyResult(partialState)
        }

        val codeHash = account.info.codeHash
        println("[EXEC] Service $serviceId code_hash=${codeHash.toHex().take(40)}...")
        println("[EXEC] Service $serviceId preimages count=${account.preimages.size}")
        val code = account.preimages[codeHash]?.bytes
        if (code == null) {
            println("[EXEC] Code not found in preimages for service $serviceId!")
            return createEmptyResult(partialState)
        }
        println("[EXEC] Code found for service $serviceId, size=${code.size}")

        if (code.size > MAX_SERVICE_CODE_SIZE) {
            println("[EXEC] Service $serviceId code too large!")
            return createEmptyResult(partialState)
        }
        println("[EXEC] Service $serviceId about to execute PVM")

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
            yield = null, // TODO: Capture yield from PVM execution
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
        println("[EXEC-PVM] executePvm called for service ${context.serviceIndex}, codeSize=${code.size}, gasLimit=$gasLimit")
        val module = getOrCompileModule(code)
        if (module == null) {
            println("[EXEC-PVM] Module compilation failed for service ${context.serviceIndex}!")
            return Pair(ExitReason.PANIC, 0L)
        }
        println("[EXEC-PVM] Module compiled for service ${context.serviceIndex}")

        // val instance = module.instantiate().getOrNull()
        // Manually instantiate with step tracing
        val interpreted = InterpretedInstance.newFromModule(module, true)
        val backend = InstanceBackend.Interpreted(interpreted)
        val instance = RawInstance(module, backend, null)

        if (instance == null) {
            return Pair(ExitReason.PANIC, 0L)
        }

        // Set up host call handler
        val hostCalls = AccumulationHostCalls(context, operands, config)

        // Set initial gas
        instance.setGas(gasLimit)
        val initialGas = gasLimit

        val entryPointPc = io.forge.jam.pvm.program.ProgramCounter(5u)

        // Encode operands

        operands.forEachIndexed { index, op ->
            if (op is AccumulationOperand.WorkItem) {
                val gasLimit = op.operand.gasLimit
                op.operand.authTrace.bytes.size
                op.operand.result.ok?.bytes?.size ?: -1
                // Log encoded gasLimit
                val encodedGas = encodeCompactInteger(gasLimit)
                encodedGas.joinToString("") { "%02x".format(it) }
            }
        }

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

        val writeResult = instance.writeMemory(INPUT_ADDR, inputData, isExternal = true)
        if (writeResult.isFailure) {
        } else {
        }

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

        println("[PVM] Starting execution for service ${context.serviceIndex}")

        // Execute until completion
        var exitReason = ExitReason.HALT
        var hostCallCount = 0
        while (true) {
            val result = instance.run()
            if (result.isFailure) {
                val e = result.exceptionOrNull()
                println("[PVM] Service ${context.serviceIndex} run() failed: ${e?.message}")
                exitReason = ExitReason.PANIC
                break
            }

            when (val interrupt = result.getOrNull()) {
                InterruptKind.Finished -> {
                    println("[PVM] Service ${context.serviceIndex} FINISHED, hostCalls=$hostCallCount")
                    exitReason = ExitReason.HALT
                    break
                }

                InterruptKind.Panic -> {
                    println("[PVM] Service ${context.serviceIndex} PANIC, hostCalls=$hostCallCount")
                    exitReason = ExitReason.PANIC
                    break
                }

                InterruptKind.NotEnoughGas -> {
                    println("[PVM] Service ${context.serviceIndex} OUT_OF_GAS, hostCalls=$hostCallCount")
                    exitReason = ExitReason.OUT_OF_GAS
                    break
                }

                is InterruptKind.Ecalli -> {
                    hostCallCount++
                    hostCalls.dispatch(interrupt.value, instance)
                }

                is InterruptKind.Segfault -> {
                    println("[PVM] Service ${context.serviceIndex} SEGFAULT, hostCalls=$hostCallCount")
                    exitReason = ExitReason.PAGE_FAULT
                    break
                }

                InterruptKind.Step -> {
                    // Continue for step tracing
                }

                null -> {
                    println("[PVM] Service ${context.serviceIndex} null interrupt, hostCalls=$hostCallCount")
                    exitReason = ExitReason.PANIC
                    break
                }
            }
        }

        val gasUsed = initialGas - instance.gas()
        println("[PVM] Service ${context.serviceIndex} done, gasUsed=$gasUsed, exitReason=$exitReason")

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
        println("[COMPILE] Compiling module with codeHash=${codeHash.toHex().take(20)}..., size=${code.size}")

        return moduleCache.getOrPut(codeHash) {
            try {
                var partsResult = ProgramParts.fromGenericBytes(ArcBytes.fromStatic(code))
                println("[COMPILE] Generic format result: ${partsResult.isSuccess}")
                if (partsResult.isFailure) {
                    partsResult = ProgramParts.fromJamBytes(ArcBytes.fromStatic(code))
                    println("[COMPILE] JAM format result: ${partsResult.isSuccess}")
                }
                if (partsResult.isFailure) {
                    partsResult = ProgramParts.fromBytes(ArcBytes.fromStatic(code))
                    println("[COMPILE] PolkaVM format result: ${partsResult.isSuccess}")
                }
                if (partsResult.isFailure) {
                    println("[COMPILE] All format parsing failed! Error: ${partsResult.exceptionOrNull()}")
                    return null
                }
                val parts = partsResult.getOrNull() ?: return null
                if (parts.stackSize < 65536u) {
                    parts.stackSize = 65536u
                }

                val actualRwLen =
                    if (parts.actualRwDataLen > 0u) parts.actualRwDataLen else parts.rwData.toByteArray().size.toUInt()
                parts.rwDataSize - actualRwLen

                val blobResult = ProgramBlob.fromParts(parts)
                if (blobResult.isFailure) {
                    return null
                }
                val blob = blobResult.getOrNull() ?: return null

                val moduleConfig = ModuleConfig.new(dynamicPaging = false)
                moduleConfig.setGasMetering(GasMeteringKind.Sync)
                moduleConfig.setPageSize(4096u)
                moduleConfig.setStepTracing(true)
                moduleConfig.setAuxDataSize(16908288u)

                Module.fromBlob(engine, moduleConfig, blob).getOrNull()
                    ?: return null
            } catch (e: Exception) {
                return null
            }
        }
    }

    /**
     * Create empty result when service code cannot be executed.
     */
    private fun createEmptyResult(state: PartialState): AccumulationOneResult {
        return AccumulationOneResult(
            postState = state,
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
