package io.forge.jam.pvm.engine

data class EngineState(
    val sandboxGlobal: GlobalStateKind? = null,
    val moduleCache: ModuleCache? = null,
) {
    companion object {
        fun new(
            moduleCache: ModuleCache? = null
        ): EngineState {
            return EngineState(
                sandboxGlobal = null,
                moduleCache = moduleCache
            )
        }
    }
}
