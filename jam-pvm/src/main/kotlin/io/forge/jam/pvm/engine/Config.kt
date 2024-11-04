package io.forge.jam.pvm.engine

data class Config(
    val crosscheck: Boolean = false,
    val allowExperimental: Boolean = false,
    val allowDynamicPaging: Boolean = false,
    val workerCount: UInt = 0u,
    val cacheEnabled: Boolean = false,
    val lruCacheSize: UInt = 0u
) {
    companion object {
        @JvmStatic
        fun new(): Config = Config()
    }
}
