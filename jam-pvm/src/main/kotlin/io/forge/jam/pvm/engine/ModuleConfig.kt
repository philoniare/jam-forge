package io.forge.jam.pvm.engine

class ModuleConfig private constructor(
    private var pageSize: UInt = 0x1000u,
    private var gasMeteringKind: GasMeteringKind? = null,
    private var isStrict: Boolean = false,
    private var stepTracing: Boolean = false,
    private var dynamicPaging: Boolean = false,
    private var auxDataSize: UInt = 0u,
    private var allowSbrk: Boolean = true,
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
     * Returns the size of the auxiliary data region.
     */
    fun getAuxDataSize(): UInt = auxDataSize

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
        this.gasMeteringKind = kind
    }

    /**
     * Returns whether dynamic paging is enabled.
     */
    fun getDynamicPaging(): Boolean = dynamicPaging

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
     * When enabled sbrk instruction is not allowed it will lead to a trap, otherwise
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
        gasMeteringKind = gasMeteringKind,
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
