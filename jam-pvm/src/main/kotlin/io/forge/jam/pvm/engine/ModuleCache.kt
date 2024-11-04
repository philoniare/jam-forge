package io.forge.jam.pvm.engine

data class ModuleCache(
    val enabled: Boolean = false,
) {
    companion object {
        fun new(): ModuleCache = ModuleCache()
    }
}
