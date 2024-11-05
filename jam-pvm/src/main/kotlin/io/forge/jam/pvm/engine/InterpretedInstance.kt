package io.forge.jam.pvm.engine

import io.forge.jam.pvm.program.ProgramCounter
import io.forge.jam.pvm.program.Reg

typealias Handler = (visitor: Visitor) -> Target?

/**
 * Internal implementation of an interpreted VM instance
 */
class InterpretedInstance private constructor(
    val module: Module,
    private val basicMemory: BasicMemory,
    private val dynamicMemory: DynamicMemory,
    val regs: ULongArray,
    var programCounter: ProgramCounter,
    var programCounterValid: Boolean,
    var nextProgramCounter: ProgramCounter?,
    private var nextProgramCounterChanged: Boolean,
    private var cycleCounter: ULong,
    private var gas: Long,
    private val compiledOffsetForBlock: FlatMap<NonZeroUInt>,
    private val compiledHandlers: MutableList<Handler>,
    private val compiledArgs: MutableList<Args>,
    var compiledOffset: UInt,
    var interrupt: InterruptKind,
    private val stepTracing: Boolean
) {
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

    fun resetMemory() {
        if (!module.isDynamicPaging()) {
            basicMemory.reset(module)
        } else {
            dynamicMemory.clear()
        }
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

    fun run(): Result<InterruptKind> = runCatching {
        runImpl(false)
    }

    private fun unpackTarget(value: NonZeroUInt): Pair<Boolean, Target> {
        val rawValue = value.value
        val isJumpTargetValid = (rawValue shr 31) == 1u
        val target = ((rawValue shl 1) shr 1)
        return Pair(isJumpTargetValid, target)
    }

    fun resolveArbitraryJump(programCounter: ProgramCounter): Target {
        compiledOffsetForBlock.get(programCounter.value)?.let { compiledOffset ->
            val (_, target) = unpackTarget(compiledOffset)
            return target
        }

        val basicBlockOffset = module.findStartOfBasicBlock(programCounter)?.also { offset ->
        } ?: run {
            return null
        }

        compileBlock(basicBlockOffset) ?: return null

        // Get the compiled offset and unpack target
        return compiledOffsetForBlock.get(programCounter.value)?.let { compiledOffset ->
            unpackTarget(compiledOffset).second
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
            this.compiledOffset = resolveArbitraryJump(programCounter, debug)

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
