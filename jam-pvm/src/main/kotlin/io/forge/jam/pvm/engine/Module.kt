package io.forge.jam.pvm.engine

import io.forge.jam.pvm.Abi
import io.forge.jam.pvm.program.ProgramBlob
import java.util.concurrent.atomic.AtomicReference

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
    val moduleKey: ModuleKey? = null
)

class Module private constructor(private var state: AtomicReference<ModulePrivate?>)
