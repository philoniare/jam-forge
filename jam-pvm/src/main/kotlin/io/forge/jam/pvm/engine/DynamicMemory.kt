package io.forge.jam.pvm.engine

import java.util.*

/**
 * Legacy DynamicMemory wrapper for backward compatibility.
 * Internally delegates to GeneralMemory for actual memory operations.
 */
class DynamicMemory private constructor(
    val pages: TreeMap<UInt, ByteArray>,
    private val generalMemory: GeneralMemory
) {
    companion object {
        /**
         * Creates a new instance of DynamicMemory.
         */
        @JvmStatic
        fun new(): DynamicMemory = DynamicMemory(
            pages = TreeMap(),
            generalMemory = GeneralMemory.new()
        )
    }

    /**
     * The underlying GeneralMemory instance.
     */
    fun generalMemory(): GeneralMemory = generalMemory

    /**
     * Clears all allocated pages.
     */
    fun clear() {
        pages.clear()
        generalMemory.clear()
    }

    /**
     * Gets or creates a page at the given address.
     */
    fun getOrCreatePage(pageAddress: UInt, pageSize: UInt): ByteArray {
        return pages.getOrPut(pageAddress) {
            ByteArray(pageSize.toInt())
        }
    }
}
