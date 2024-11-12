package io.forge.jam.pvm.engine

import io.forge.jam.pvm.Abi
import io.forge.jam.pvm.PvmLogger
import io.forge.jam.pvm.program.*
import java.util.concurrent.atomic.AtomicReference
import kotlin.math.log2

data class ModulePrivate(
    val engineState: AtomicReference<EngineState>?,
    val crosscheck: Boolean,
    val blob: ProgramBlob,
    val compiledModule: CompiledModuleKind,
    val interpretedModule: InterpretedModule?,
    val memoryMap: Abi.MemoryMap,
    val gasMetering: GasMeteringKind?,
    val isStrict: Boolean,
    val stepTracing: Boolean,
    val dynamicPaging: Boolean,
    val pageSizeMask: UInt,
    val pageShift: UInt,
    val instructionSet: RuntimeInstructionSet,
)

class Module private constructor(private var state: AtomicReference<ModulePrivate?>) {
    companion object {
        private const val RESPONSE_WAIT_TIMEOUT = 60000L
        private val logger = PvmLogger(Module::class.java)

        @JvmStatic
        fun fromBlob(engine: Engine, config: ModuleConfig, blob: ProgramBlob): Result<Module> = runCatching {
            if (config.dynamicPaging && !engine.allowDynamicPaging) {
                throw IllegalStateException("Dynamic paging not enabled; use Config.setAllowDynamicPaging to enable it")
            }

            // Validate memory configuration early
            Abi.MemoryMapBuilder.new(config.pageSize)
                .roDataSize(blob.roDataSize)
                .rwDataSize(blob.rwDataSize)
                .stackSize(blob.stackSize).build().getOrThrow()

            val init = GuestInit(
                pageSize = config.pageSize,
                roData = blob.roData.toByteArray(),
                rwData = blob.rwData.toByteArray(),
                roDataSize = blob.roDataSize,
                rwDataSize = blob.rwDataSize,
                stackSize = blob.stackSize,
                auxDataSize = config.auxDataSize
            )

            val instructionSet = RuntimeInstructionSet(
                allowSbrk = config.allowSbrk,
                is64Bit = blob.is64Bit
            )

            // Compilation and module setup
            val interpretedModule = if (engine.interpreterEnabled) {
                InterpretedModule.new(init).getOrNull()
            } else null

            val memoryMap = init.memoryMap().getOrThrow()

            // RO data starts at 0 by convention
            logger.debug(
                """Memory map: RO data: 0x${memoryMap.roDataRange().first.toString(16).padStart(8, '0')}..0x${
                    memoryMap.roDataRange().last.toString(16).padStart(8, '0')
                } (${blob.roData.asRef().size}/${memoryMap.roDataRange().endInclusive - memoryMap.roDataRange().start} bytes, non-zero until 0x${
                    (memoryMap.roDataRange().start + blob.roData.toByteArray().size.toUInt()).toString(16)
                        .padStart(8, '0')
                })""".trimIndent()
            )

            logger.debug(
                """Memory map: RW data: 0x${
                    memoryMap.rwDataAddress.toString(16).padStart(8, '0')
                }..0x${
                    (memoryMap.rwDataAddress + memoryMap.rwDataSize).toString(16).padStart(8, '0')
                } (${blob.rwData.toByteArray().size}/${memoryMap.rwDataSize} bytes, non-zero until 0x${
                    (memoryMap.rwDataAddress.toUInt() + blob.rwData.toByteArray().size.toUInt()).toString(
                        16
                    ).padStart(8, '0')
                })
""".trimIndent()
            )

            logger.debug(
                """
    Memory map:   Stack: 0x${
                    (memoryMap.stackAddressHigh - memoryMap.stackSize).toString(16).padStart(8, '0')
                }..0x${
                    memoryMap.stackAddressHigh.toString(16).padStart(8, '0')
                } (${blob.stackSize}/${memoryMap.stackSize} bytes)
""".trimIndent()
            )

            logger.debug(
                """
    Memory map:     Aux: 0x${
                    memoryMap.auxDataAddress.toString(16).padStart(8, '0')
                }..0x${
                    (memoryMap.auxDataAddress + memoryMap.auxDataSize).toString(16).padStart(8, '0')
                } (${config.auxDataSize}/${memoryMap.auxDataSize} bytes requested)
""".trimIndent()
            )

            val pageShift = log2(memoryMap.pageSize.toFloat()).toUInt()
            val pageSizeMask = ((1u shl pageShift.toInt()) - 1u)

            val modulePrivate = ModulePrivate(
                engineState = engine.state.let { AtomicReference(it) },
                crosscheck = engine.crosscheck,
                blob = blob,
                compiledModule = CompiledModuleKind.Unavailable,
                interpretedModule = interpretedModule,
                memoryMap = memoryMap,
                gasMetering = config.gasMetering,
                isStrict = config.isStrict,
                stepTracing = config.stepTracing,
                dynamicPaging = config.dynamicPaging,
                pageSizeMask = pageSizeMask,
                pageShift = pageShift,
                instructionSet = instructionSet
            )

            Module(AtomicReference(modulePrivate))
        }
    }

    /**
     * Creates a new instance of the module.
     * @return Result containing a RawInstance if successful, or an error if failed
     */
    fun instantiate(): Result<RawInstance> = runCatching {
        val state = state.get() ?: throw IllegalStateException("Failed to instantiate module: empty module")

        val backend = InstanceBackend.Interpreted(
            InterpretedInstance.newFromModule(this, false)
        )

        // Create crosscheck instance if needed
        val crosscheckInstance = if (state.crosscheck && backend !is InstanceBackend.Interpreted) {
            InterpretedInstance.newFromModule(this, true)
        } else null

        RawInstance(
            module = this,
            backend = backend,
            crosscheckInstance = crosscheckInstance
        )
    }

    fun state(): ModulePrivate = state.get() ?: throw IllegalStateException("Module not initialized")

    fun isStrict(): Boolean = state().isStrict

    fun interpretedModule(): InterpretedModule? = state().interpretedModule

    fun memoryMap(): Abi.MemoryMap = state().memoryMap

    fun isStepTracing(): Boolean = state().stepTracing

    fun isDynamicPaging(): Boolean = state().dynamicPaging

    fun blob(): ProgramBlob = state().blob

    fun codeLen(): UInt = state().blob.code.asRef().size.toUInt()

    fun isJumpTargetValid(offset: ProgramCounter): Boolean =
        state().blob.isJumpTargetValid(state().instructionSet, offset)

    fun gasMetering(): GasMeteringKind? = state().gasMetering

    fun jumpTable() = state().blob.jumpTable()

    fun findStartOfBasicBlock(offset: ProgramCounter): ProgramCounter? {
        return Program.findStartOfBasicBlock(
            state().instructionSet,
            state().blob.code.toByteArray(),
            state().blob.bitmask.toByteArray(),
            offset.value
        )?.let { ProgramCounter(it) }
    }

    fun instructionsBoundedAt(offset: ProgramCounter): Instructions<RuntimeInstructionSet> {
        return state().blob.instructionsBoundedAt(state().instructionSet, offset)
    }

    fun roundToPageSizeDown(value: UInt): UInt {
        return value and state().pageSizeMask.inv()
    }

    fun defaultSp(): ULong {
        return state().memoryMap.stackAddressHigh.toULong()
    }
}
