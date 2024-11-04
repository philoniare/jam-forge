package io.forge.jam.pvm.engine

data class Engine(
    val selectedSandbox: SandboxKind?,
    val interpreterEnabled: Boolean,
    val crosscheck: Boolean,
    val state: EngineState,
    val allowDynamicPaging: Boolean
) {
    companion object {
        fun new(config: Config): Result<Engine> = runCatching {
            val crosscheck = config.crosscheck

            // TODO: Add module caching
            val engineState = EngineState(
                sandboxGlobal = null,
                moduleCache = null,
            )

            Engine(
                selectedSandbox = SandboxKind.Generic,
                interpreterEnabled = true,
                crosscheck = crosscheck,
                state = EngineState.new(moduleCache = null),
                allowDynamicPaging = config.allowDynamicPaging
            )
        }
    }
}
