package io.forge.jam.pvm.engine

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

    private fun emit(handler: Handler, args: Args) {
        compiledHandlers.add(handler)
        compiledArgs.add(args)
    }

    companion object {
        private const val TARGET_OUT_OF_RANGE = 0u

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
                // +1 for one implicit out-of-bounds trap
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
        if (!module.isStrict()) {
            value = value and 0xFFFFFFFFu
        }
        return value
    }

    fun setReg(reg: Reg, value: ULong) {
        regs[reg.toIndex()] = if (!module.blob().is64Bit) {
            Cast(value).truncateToU32()
                .let { Cast(it) }
                .toSigned()
                .let { Cast(it) }
                .toI64SignExtend()
                .let { Cast(it) }
                .toUnsigned()
        } else {
            value
        }
    }

    fun gas(): Long = gas

    fun setGas(newGas: Long) {
        gas = newGas
    }

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
            emit(RawHandlers.stepOutOfRange, Args.stepOutOfRange())
        }
        val gasCost = if (module.gasMetering() != null) {
            trapCost()
        } else {
            0u
        }

        emit(RawHandlers.stepOutOfRange, Args.outOfRange(gasCost))
    }

    fun run(): Result<InterruptKind> = runCatching {
        runImpl(false)
    }

    private fun unpackTarget(value: NonZeroUInt): Pair<Boolean, Target> {
        val rawValue = value.value
        val isJumpTargetValid = (rawValue shr 31) == 1u
        val target = ((rawValue shl 1) shr 1)
        return Pair(isJumpTargetValid, target)
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
        var index = Cast(index).assertAlwaysFitsInU32()
        if (isJumpTargetValid) {
            index = index or (1u shl 31)
        }
        return NonZeroUInt(index)
    }

    fun compileBlock(programCounter: ProgramCounter?): Target? {
        if (programCounter?.value!! > module.codeLen()) {
            return null
        }

        var origin = try {
            compiledHandlers.size.toUInt()
        } catch (e: Exception) {
            throw IllegalStateException("Failed to compile block: ${e.message}")
        }

        val gasVisitor = GasVisitor()
        var chargeGasIndex: Pair<ProgramCounter, Int>? = null
        var isJumpTargetValid = module.isJumpTargetValid(programCounter)

        for (instruction in module.instructionsBoundedAt(programCounter)) {
            compiledOffsetForBlock.insert(
                instruction.offset.value, packTarget(
                    compiledHandlers.size.toUInt(),
                    isJumpTargetValid
                )
            )

            isJumpTargetValid = false

            if (module.gasMetering() != null) {
                if (chargeGasIndex == null) {
                    chargeGasIndex = instruction.offset to compiledHandlers.size
                }
                instruction.kind.visit(gasVisitor)
            }

            // Debug assertions equivalent
            val originalLength = compiledHandlers.size

            instruction.kind.visit(
                Compiler(
                    programCounter = instruction.offset,
                    nextProgramCounter = instruction.nextOffset,
                    compiledHandlers = compiledHandlers,
                    compiledArgs = compiledArgs,
                    module = module
                )
            )

            // Debug assertions equivalent
            assert(compiledHandlers.size > originalLength) {
                "Handler size must increase after instruction visit"
            }

            if (instruction.kind.opcode().startsNewBasicBlock()) {
                break
            }
        }

        chargeGasIndex?.let { (programCounter, index) ->
            val gasCost = gasVisitor.takeBlockCost()
                ?: throw IllegalStateException("No gas cost available")
            compiledArgs[index] = Args.chargeGas(programCounter, gasCost)
        }

        if (compiledHandlers.size == origin.toInt()) {
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
        val pageSizeInt = Cast(pageSize).toUSize()
        val lengthInt = Cast(length).toUSize()
        val initialPageOffset = Cast(address).toUSize() - Cast(pageAddressLo).toUSize()
        val initialChunkLength = minOf(lengthInt, pageSizeInt - initialPageOffset)
        callback(pageAddressLo, initialPageOffset.toUInt(), 0u, initialChunkLength.toInt())

        if (pageAddressLo == pageAddressHi) {
            return
        }

        var pageAddressLoLong = Cast(pageAddressLo).toU64()
        val pageAddressHiLong = Cast(pageAddressHi).toU64()
        pageAddressLoLong += Cast(pageSize).toU64()
        var bufferOffset = initialChunkLength

        while (pageAddressLoLong < pageAddressHiLong) {
            callback(
                Cast(pageAddressLoLong).assertAlwaysFitsInU32(),
                0u,
                bufferOffset.toUInt(),
                pageSizeInt.toInt()
            )
            bufferOffset += pageSizeInt
            pageAddressLoLong += Cast(pageSize).toU64()
        }

        callback(
            Cast(pageAddressLoLong).assertAlwaysFitsInU32(),
            0u,
            bufferOffset.toUInt(),
            (lengthInt - bufferOffset).toInt()
        )
    }

    fun readMemoryInto(address: UInt, buffer: ByteArray): Result<ByteArray> {
        return try {
            if (!module.isDynamicPaging()) {
                // Handle basic memory case
                val length = Cast(buffer.size.toUInt()).assertAlwaysFitsInU32()
                val slice = basicMemory.getMemorySlice(module, address, length)
                    ?: return Result.failure(
                        MemoryAccessError.outOfRangeAccess(
                            address = address,
                            length = Cast(buffer.size.toUInt()).toU64()
                        )
                    )

                slice.copyInto(buffer)
                Result.success(buffer)
            } else {
                // Handle dynamic paging case
                eachPage(
                    module = module,
                    address = address,
                    length = Cast(buffer.size.toUInt()).assertAlwaysFitsInU32()
                ) { pageAddress, pageOffset, bufferOffset, length ->
                    // Validate offsets and lengths
                    require(bufferOffset + length.toUInt() <= buffer.size.toUInt()) {
                        "Buffer offset out of bounds"
                    }
                    require(pageOffset + length.toUInt() <= Cast(module.memoryMap().pageSize).toUSize()) {
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

    private fun runImpl(debug: Boolean): InterruptKind {
        if (!module.isDynamicPaging()) {
            basicMemory.markDirty()
        }

        if (nextProgramCounterChanged) {
            val programCounter =
                nextProgramCounter ?: throw IllegalStateException("Failed to run: next program counter is not set")

            this.programCounter = programCounter
            this.compiledOffset = resolveArbitraryJump(programCounter)!!

        }

        var offset = compiledOffset
        while (true) {
            if (debug) {
                cycleCounter++
            }

            val handler = compiledHandlers[offset.toInt()]
            val visitor = Visitor(this)
            when (val nextOffset = handler(visitor)) {
                null -> return interrupt
                else -> {
                    offset = nextOffset
                    compiledOffset = offset
                }
            }
        }
    }

}
