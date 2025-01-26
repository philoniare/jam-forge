package io.forge.jam.pvm.engine

class ModuleConfig private constructor(
    var pageSize: UInt = 0x1000u,
    var gasMetering: GasMeteringKind? = null,
    var isStrict: Boolean = false,
    var stepTracing: Boolean = false,
    var dynamicPaging: Boolean = false,
    var auxDataSize: UInt = 0u,
    var allowSbrk: Boolean = true,
    private var cacheByHash: Boolean = false
) : Cloneable {

    /**
     * Sets the page size used for the module.
     *
     * Default: `4096` (4k)
     */
    fun setPageSize(pageSize: UInt): ModuleConfig = apply {
        this.pageSize = pageSize
    }

    /**
     * Sets the size of the auxiliary data region.
     *
     * Default: `0`
     */
    fun setAuxDataSize(auxDataSize: UInt): ModuleConfig = apply {
        this.auxDataSize = auxDataSize
    }

    /**
     * Sets the type of gas metering to enable for this module.
     *
     * Default: `None`
     */
    fun setGasMetering(kind: GasMeteringKind?): ModuleConfig = apply {
        this.gasMetering = kind
    }

    /**
     * Sets whether dynamic paging is enabled.
     *
     * [Config.allowDynamicPaging] also needs to be `true` for dynamic paging to be enabled.
     *
     * Default: `false`
     */
    fun setDynamicPaging(value: Boolean): ModuleConfig = apply {
        this.dynamicPaging = value
    }

    /**
     * Sets whether step tracing is enabled.
     *
     * When enabled [InterruptKind.Step] will be returned by [RawInstance.run]
     * for each executed instruction.
     *
     * Should only be used for debugging.
     *
     * Default: `false`
     */
    fun setStepTracing(enabled: Boolean): ModuleConfig = apply {
        this.stepTracing = enabled
    }

    /**
     * Sets the strict mode. When disabled it's guaranteed that the semantics
     * of lazy execution match the semantics of eager execution.
     *
     * Should only be used for debugging.
     *
     * Default: `false`
     */
    fun setStrict(isStrict: Boolean): ModuleConfig = apply {
        this.isStrict = isStrict
    }

    /**
     * Sets whether sbrk instruction is allowed.
     *
     * When enabled sbrk instruction is not allowed it will lead to a panic, otherwise
     * sbrk instruction is emulated.
     *
     * Default: `true`
     */
    fun setAllowSbrk(enabled: Boolean): ModuleConfig = apply {
        this.allowSbrk = enabled
    }

    /**
     * Returns whether the module will be cached by hash.
     */
    fun getCacheByHash(): Boolean = cacheByHash

    /**
     * Sets whether the module will be cached by hash.
     *
     * This introduces extra overhead as every time a module compilation is triggered the hash
     * of the program must be calculated, and in general it is faster to recompile a module
     * from scratch rather than compile its hash.
     *
     * Default: `true`
     */
    fun setCacheByHash(enabled: Boolean): ModuleConfig = apply {
        this.cacheByHash = enabled
    }

    public override fun clone(): ModuleConfig = ModuleConfig(
        pageSize = pageSize,
        gasMetering = gasMetering,
        isStrict = isStrict,
        stepTracing = stepTracing,
        dynamicPaging = dynamicPaging,
        auxDataSize = auxDataSize,
        allowSbrk = allowSbrk,
        cacheByHash = cacheByHash
    )

    companion object {
        /**
         * Creates a new default module configuration.
         */
        fun new(): ModuleConfig = ModuleConfig()
    }
}
