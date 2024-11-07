package io.forge.jam.pvm.engine

import java.util.*

/**
 * Internal implementation of dynamic memory management using page mapping
 */
class DynamicMemory private constructor(
    val pages: TreeMap<UInt, ByteArray>
) {
    companion object {
        /**
         * Creates a new instance of DynamicMemory
         */
        @JvmStatic
        fun new(): DynamicMemory = DynamicMemory(TreeMap())
    }

    /**
     * Clears all allocated pages
     */
    fun clear() {
        pages.clear()
    }
}
