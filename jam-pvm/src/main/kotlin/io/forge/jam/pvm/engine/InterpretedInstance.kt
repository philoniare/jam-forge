package io.forge.jam.pvm.engine

import io.forge.jam.pvm.PvmLogger
import io.forge.jam.pvm.RawHandlers
import io.forge.jam.pvm.engine.GasVisitor.Companion.trapCost
import io.forge.jam.pvm.program.Compiler
import io.forge.jam.pvm.program.ProgramCounter
import io.forge.jam.pvm.program.Reg

typealias Handler = (visitor: Visitor) -> Target?

/**
 * Internal implementation of an interpreted VM instance
 */
class InterpretedInstance private constructor(
    val module: Module,
    val basicMemory: BasicMemory,
    val dynamicMemory: DynamicMemory,
    val regs: ULongArray,
    var programCounter: ProgramCounter,
    var programCounterValid: Boolean,
    var nextProgramCounter: ProgramCounter?,
    var nextProgramCounterChanged: Boolean,
    var cycleCounter: ULong,
    var gas: Long,
    val compiledOffsetForBlock: FlatMap<NonZeroUInt>,
    val compiledHandlers: MutableList<Handler>,
    val compiledArgs: MutableList<Args>,
    var compiledOffset: UInt,
    var interrupt: InterruptKind,
    val stepTracing: Boolean
) {

    fun emit(handler: Handler, args: Args, name: String) {
        logger.debug("Emitting handler: $name")
        compiledHandlers.add(handler)
        compiledArgs.add(args)
    }

    companion object {
        private const val TARGET_OUT_OF_RANGE = 0u
        private val logger = PvmLogger(InterpretedInstance::class.java)

        /**
         * Creates a new instance from a module
         */
        fun newFromModule(module: Module, forceStepTracing: Boolean): InterpretedInstance {
            val stepTracing = module.isStepTracing() || forceStepTracing

            return InterpretedInstance(
                module = module,
                basicMemory = BasicMemory.new(),
                dynamicMemory = DynamicMemory.new(),
                regs = ULongArray(Reg.ALL.size) { 0u },
                programCounter = ProgramCounter(UInt.MAX_VALUE),
                programCounterValid = false,
                nextProgramCounter = null,
                nextProgramCounterChanged = true,
                cycleCounter = 0u,
                gas = 0L,
                compiledOffsetForBlock = FlatMap.new(module.codeLen() + 1u),
                compiledHandlers = mutableListOf(),
                compiledArgs = mutableListOf(),
                compiledOffset = 0u,
                interrupt = InterruptKind.Finished,
                stepTracing = stepTracing
            ).apply {
                initializeModule()
            }
        }
    }

    fun reg(reg: Reg): ULong {
        var value = regs[reg.toIndex()]
        if (!module.blob().is64Bit) {
            value = value and 0xFFFFFFFFu
        }
        return value
    }

    fun setReg(reg: Reg, value: ULong) {
        regs[reg.toIndex()] = if (!module.blob().is64Bit) {
            Cast(Cast(value).ulongTruncateToU32())
                .uintToSigned()
                .let { Cast(it) }
                .intToI64SignExtend()
                .let { Cast(it) }
                .longToUnsigned()
        } else {
            value
        }
    }

    fun gas(): Long = gas

    fun programCounter(): ProgramCounter? =
        if (!programCounterValid) null else programCounter

    fun nextProgramCounter(): ProgramCounter? = nextProgramCounter

    fun setNextProgramCounter(pc: ProgramCounter) {
        programCounterValid = false
        nextProgramCounter = pc
        nextProgramCounterChanged = true
    }

    fun nextNativeProgramCounter(): ULong? = null

    fun heapSize(): UInt =
        if (!module.isDynamicPaging()) {
            basicMemory.heapSize()
        } else {
            TODO("Dynamic paging heap size not implemented")
        }

    fun sbrk(size: UInt): UInt? =
        if (!module.isDynamicPaging()) {
            basicMemory.sbrk(module, size)
        } else {
            TODO("Dynamic paging sbrk not implemented")
        }

    fun pid(): UInt? = null

    fun resetMemory(): Result<Unit> {
        if (!module.isDynamicPaging()) {
            basicMemory.reset(module)
        } else {
            dynamicMemory.clear()
        }
        return Result.success(Unit)
    }

    private fun initializeModule() {
        if (module.gasMetering() != null) {
            gas = 0L
        }

        if (!module.isDynamicPaging()) {
            basicMemory.forceReset(module)
        } else {
            dynamicMemory.clear()
        }

        compileOutOfRangeStub()
    }

    fun compileOutOfRangeStub() {
        if (stepTracing) {
            emit(RawHandlers.stepOutOfRange, Args.stepOutOfRange(), "step_out_of_range")
        }
        val gasCost = if (module.gasMetering() != null) {
            trapCost()
        } else {
            0u
        }

        emit(RawHandlers.stepOutOfRange, Args.outOfRange(gasCost), "out_of_range")
    }

    fun run(): Result<InterruptKind> = runCatching {
        runImpl()
    }

    private fun unpackTarget(value: NonZeroUInt): Pair<Boolean, Target> {
        val rawValue = value.value
        val isJumpTargetValid = (rawValue shr 31) == 1u
        val target = ((rawValue shl 1) shr 1)
        return Pair(isJumpTargetValid, target)
    }

    fun resolveJump(programCounter: ProgramCounter): Target? {
        val compiledOffset = compiledOffsetForBlock.get(programCounter.value)

        if (compiledOffset != null) {
            val (isJumpTargetValid, target) = unpackTarget(compiledOffset)
            if (!isJumpTargetValid) {
                return null
            }
            return target
        }

        if (!module.isJumpTargetValid(programCounter)) {
            return null
        }


        val finalTarget = compileBlock(programCounter)
        logger.debug("Resolved jump target: $finalTarget")
        return finalTarget
    }

    fun resolveFallthrough(programCounter: ProgramCounter): Target? {
        // First try to get from compiled offset block
        compiledOffsetForBlock.get(programCounter.value)?.let { compiledOffset ->
            val (isJumpTargetValid, target) = unpackTarget(compiledOffset)
            if (!isJumpTargetValid) {
                return null
            }
            return target
        }

        // If not found, compile block
        return compileBlock(programCounter)
    }

    fun resolveArbitraryJump(programCounter: ProgramCounter): Target? {
        compiledOffsetForBlock.get(programCounter.value)?.let { compiledOffset ->
            val (_, target) = unpackTarget(compiledOffset)
            return target
        }

        val basicBlockOffset = module.findStartOfBasicBlock(programCounter)

        compileBlock(basicBlockOffset)

        // Get the compiled offset and unpack target
        return compiledOffsetForBlock.get(programCounter.value)?.let { compiledOffset ->
            unpackTarget(compiledOffset).second
        }
    }

    fun packTarget(index: UInt, isJumpTargetValid: Boolean): NonZeroUInt {
        var index = Cast(index).uintAssertAlwaysFitsInU32()
        if (isJumpTargetValid) {
            index = index or (1u shl 31)
        }
        return NonZeroUInt(index)
    }

    private fun compileBlock(programCounter: ProgramCounter?): Target? {
        if (programCounter?.value!! > module.codeLen()) {
            logger.debug("Program counter out of range: ${programCounter.value}")
            return null
        }

        logger.debug("Compiled handlers: ${compiledHandlers.size}. ${programCounter}")
        val origin = if (compiledHandlers.size > UInt.MAX_VALUE.toLong()) {
            throw IllegalStateException("internal compiled program counter overflow: the program is too big!")
        } else {
            compiledHandlers.size.toUInt()
        }

        logger.debug("Compiling block: ${origin}")

        val gasVisitor = GasVisitor()
        var chargeGasIndex: Pair<ProgramCounter, Int>? = null
        var isJumpTargetValid = module.isJumpTargetValid(programCounter)

        var i = 0
        for (instruction in module.instructionsBoundedAt(programCounter)) {
            i++
            logger.debug("Instruction kind: ${instruction.kind}, ${instruction.offset.value}, ${instruction.nextOffset.value}")

            compiledOffsetForBlock.insert(
                instruction.offset.value,
                packTarget(compiledHandlers.size.toUInt(), isJumpTargetValid)
            )

            isJumpTargetValid = false

            if (stepTracing) {
                logger.debug("  [${compiledHandlers.size}]: ${instruction.offset}: step")
                emit(RawHandlers.step, Args.step(instruction.offset), "step")
            }

            if (module.gasMetering() != null) {
                if (chargeGasIndex == null) {
                    logger.debug("  [${compiledHandlers.size}]: ${instruction.offset}: charge_gas")
                    chargeGasIndex = Pair(instruction.offset, compiledHandlers.size)
                    emit(RawHandlers.chargeGas, Args.chargeGas(instruction.offset, 0u), "charge_gas")
                }
                instruction.kind.visit(gasVisitor)
            }

            logger.debug("  [${compiledHandlers.size}]: ${instruction.offset}: ${instruction.kind}")
            val originalLength = compiledHandlers.size

            instruction.kind.visit(
                Compiler(
                    programCounter = instruction.offset,
                    nextProgramCounter = instruction.nextOffset,
                    compiledHandlers = compiledHandlers,
                    compiledArgs = compiledArgs,
                    module = module,
                    instance = this
                )
            )

            logger.debug("Impossible: ${compiledHandlers.size}, ${originalLength}")
            assert(compiledHandlers.size > originalLength) {
                "Handler size must increase after instruction visit"
            }

            if (instruction.kind.opcode().startsNewBasicBlock()) {
                break
            }
        }
        logger.debug("Processed instructions: ${i}")

        chargeGasIndex?.let { (programCounter, index) ->
            val gasCost = gasVisitor.takeBlockCost()!!
            compiledArgs[index] = Args.chargeGas(programCounter, gasCost)
        }

        // Convert origin back to size type for comparison, like Rust does
        logger.debug("Compiled block: ${origin} -> ${compiledHandlers.size}")
        if (compiledHandlers.size == origin.toInt()) {
            logger.debug("Exiting early")
            return null
        }

        return origin
    }

    fun eachPage(
        module: Module,
        address: UInt,
        length: UInt,
        callback: (pageAddress: UInt, pageOffset: UInt, bufferOffset: UInt, length: Int) -> Unit
    ) {
        val pageSize = module.memoryMap().pageSize
        val pageAddressLo = module.roundToPageSizeDown(address)
        val pageAddressHi = module.roundToPageSizeDown(address + (length - 1u))
        eachPageImpl(pageSize, pageAddressLo, pageAddressHi, address, length, callback)
    }

    private fun eachPageImpl(
        pageSize: UInt, pageAddressLo: UInt, pageAddressHi: UInt, address: UInt, length: UInt,
        callback: (pageAddress: UInt, pageOffset: UInt, bufferOffset: UInt, length: Int) -> Unit
    ) {
        val pageSizeInt = Cast(pageSize).uintToU64()
        val lengthInt = Cast(length).uintToU64()
        val initialPageOffset = Cast(address).uintToU64() - Cast(pageAddressLo).uintToU64()
        val initialChunkLength = minOf(lengthInt, pageSizeInt - initialPageOffset)
        callback(pageAddressLo, initialPageOffset.toUInt(), 0u, initialChunkLength.toInt())

        if (pageAddressLo == pageAddressHi) {
            return
        }

        var pageAddressLoLong = Cast(pageAddressLo).uintToU64()
        val pageAddressHiLong = Cast(pageAddressHi).uintToU64()
        pageAddressLoLong += Cast(pageSize).uintToU64()
        var bufferOffset = initialChunkLength

        while (pageAddressLoLong < pageAddressHiLong) {
            callback(
                Cast(pageAddressLoLong).ulongAssertAlwaysFitsInU32(),
                0u,
                bufferOffset.toUInt(),
                pageSizeInt.toInt()
            )
            bufferOffset += pageSizeInt
            pageAddressLoLong += Cast(pageSize).uintToU64()
        }

        callback(
            Cast(pageAddressLoLong).ulongAssertAlwaysFitsInU32(),
            0u,
            bufferOffset.toUInt(),
            (lengthInt - bufferOffset).toInt()
        )
    }

    fun readMemoryInto(address: UInt, buffer: ByteArray): Result<ByteArray> {
        return try {
            if (!module.isDynamicPaging()) {
                // Handle basic memory case
                val length = Cast(buffer.size.toUInt()).uintAssertAlwaysFitsInU32()
                val slice = basicMemory.getMemorySlice(module, address, length)
                    ?: return Result.failure(
                        MemoryAccessError.outOfRangeAccess(
                            address = address,
                            length = Cast(buffer.size.toUInt()).uintToU64()
                        )
                    )

                slice.copyInto(buffer)
                Result.success(buffer)
            } else {
                // Handle dynamic paging case
                eachPage(
                    module = module,
                    address = address,
                    length = Cast(buffer.size.toUInt()).uintAssertAlwaysFitsInU32()
                ) { pageAddress, pageOffset, bufferOffset, length ->
                    // Validate offsets and lengths
                    require(bufferOffset + length.toUInt() <= buffer.size.toUInt()) {
                        "Buffer offset out of bounds"
                    }
                    require(pageOffset + length.toUInt() <= Cast(module.memoryMap().pageSize).uintToU64()) {
                        "Page offset out of bounds"
                    }

                    val page = dynamicMemory.pages[pageAddress]
                    if (page != null) {
                        page.copyInto(
                            destination = buffer,
                            destinationOffset = bufferOffset.toInt(),
                            startIndex = pageOffset.toInt(),
                            endIndex = (pageOffset + length.toUInt()).toInt()
                        )
                    } else {
                        buffer.fill(0, bufferOffset.toInt(), (bufferOffset + length.toUInt()).toInt())
                    }
                }
                Result.success(buffer)
            }
        } catch (e: Exception) {
            Result.failure(MemoryAccessError.error(PvmError.fromDisplay(e.message ?: "Unknown error")))
        }
    }

    fun writeMemory(address: UInt, data: ByteArray): Result<Unit> {
        return try {
            if (!module.isDynamicPaging()) {
                // Handle basic memory case
                val length = Cast(data.size.toUInt()).uintAssertAlwaysFitsInU32()
                val slice = basicMemory.getMemorySliceMut(module, address, length)
                    ?: return Result.failure(
                        MemoryAccessError.outOfRangeAccess(
                            address = address,
                            length = Cast(data.size.toUInt()).uintToU64()
                        )
                    )

                data.forEachIndexed { index, byte ->
                    slice[index] = byte.toUByte()
                }
            } else {
                // Handle dynamic paging case
                val pageSize = module.memoryMap().pageSize
                eachPage(
                    module = module,
                    address = address,
                    length = Cast(data.size.toUInt()).uintAssertAlwaysFitsInU32()
                ) { pageAddress, pageOffset, bufferOffset, length ->
                    val page = dynamicMemory.pages.getOrPut(pageAddress) {
                        ByteArray(pageSize.toInt()) // empty page of page_size
                    }

                    data.copyInto(
                        destination = page,
                        destinationOffset = pageOffset.toInt(),
                        startIndex = bufferOffset.toInt(),
                        endIndex = (bufferOffset + length.toUInt()).toInt()
                    )
                }
            }
            Result.success(Unit)
        } catch (e: Exception) {
            Result.failure(MemoryAccessError.error(PvmError.fromDisplay(e.message ?: "Unknown error")))
        }
    }

    private fun runImpl(): InterruptKind {
        if (!module.isDynamicPaging()) {
            basicMemory.markDirty()
        }

        logger.debug("Running impl: ${nextProgramCounterChanged}")
        if (nextProgramCounterChanged) {
            val pc = nextProgramCounter
            nextProgramCounter = null
            val programCounter =
                pc ?: throw IllegalStateException("Failed to run: next program counter is not set")

            this.programCounter = programCounter
            logger.debug("PC: ${programCounter.value}")
            this.compiledOffset = resolveArbitraryJump(programCounter)
                ?: TARGET_OUT_OF_RANGE
            logger.debug("Resolved jump: ${compiledOffset}")
            logger.debug("Starting execution at: ${programCounter.value} [$compiledOffset]")
        } else {
            logger.debug("Implicitly resuming at: ${programCounter.value} [$compiledOffset]")
        }

        var offset = compiledOffset
        while (true) {
            cycleCounter++

            val handler = compiledHandlers[offset.toInt()]
            logger.debug("Executing handler at: [$offset], cycle counter: $cycleCounter")

            val visitor = Visitor(this)
            val nextOffset = handler(visitor)
            logger.debug("Next offset: $nextOffset")
            when (nextOffset) {
                null -> return interrupt
                else -> {
                    offset = nextOffset
                    this.compiledOffset = offset
                }
            }
        }
    }
}
