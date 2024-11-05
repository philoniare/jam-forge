package io.forge.jam.pvm.engine

sealed class CompiledModuleKind {
    object Unavailable : CompiledModuleKind()
    data class Generic(val module: GenericModule) : CompiledModuleKind()
}
