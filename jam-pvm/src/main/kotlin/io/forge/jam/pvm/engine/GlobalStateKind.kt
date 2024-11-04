package io.forge.jam.pvm.engine

sealed class GlobalStateKind {
    data class Generic(val state: GenericGlobalState) : GlobalStateKind()

    companion object {
        fun new(kind: SandboxKind, config: Config): Result<GlobalStateKind> = runCatching {
            when (kind) {
                SandboxKind.Generic -> {
                    if (isGenericSandboxEnabled) {
                        Generic(
                            GenericGlobalState.new(config)
                                .getOrElse { error ->
                                    throw IllegalStateException("failed to initialize generic sandbox: $error")
                                }
                        )
                    } else {
                        throw IllegalStateException("generic sandbox is not enabled")
                    }
                }

                else -> throw IllegalStateException("unsupported sandbox kind: $kind")
            }
        }

        private val isGenericSandboxEnabled = true
    }
}

class GenericGlobalState private constructor() {
    companion object {
        fun new(config: Config): Result<GenericGlobalState> = runCatching {
            GenericGlobalState()
        }
    }
}
