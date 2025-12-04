package io.forge.jam.safrole.traces

import io.forge.jam.core.JamByteArray
import java.util.TreeMap

/**
 * Manages state storage for trace validation.
 * Stores key-value pairs and computes state roots.
 */
class StateManager {
    // Using TreeMap for deterministic ordering of keys
    private val storage: TreeMap<ByteArrayWrapper, JamByteArray> = TreeMap()

    /**
     * Loads state from a RawState object.
     */
    fun loadFromRawState(rawState: RawState) {
        storage.clear()
        for (kv in rawState.keyvals) {
            storage[ByteArrayWrapper(kv.key.bytes)] = kv.value
        }
    }

    /**
     * Gets the current state root.
     */
    fun getStateRoot(): JamByteArray {
        // For now, return the stored state root
        // Full implementation would compute Merkle-Patricia trie root
        return JamByteArray(ByteArray(32))
    }

    /**
     * Gets a value by key.
     */
    fun get(key: ByteArray): JamByteArray? {
        return storage[ByteArrayWrapper(key)]
    }

    /**
     * Gets a value by key.
     */
    fun get(key: JamByteArray): JamByteArray? {
        return get(key.bytes)
    }

    /**
     * Sets a value for a key.
     */
    fun set(key: ByteArray, value: JamByteArray) {
        storage[ByteArrayWrapper(key)] = value
    }

    /**
     * Sets a value for a key.
     */
    fun set(key: JamByteArray, value: JamByteArray) {
        set(key.bytes, value)
    }

    /**
     * Removes a key from storage.
     */
    fun remove(key: ByteArray) {
        storage.remove(ByteArrayWrapper(key))
    }

    /**
     * Gets all key-value pairs as a list.
     */
    fun getAllKeyValues(): List<KeyValue> {
        return storage.map { (key, value) ->
            KeyValue(JamByteArray(key.data), value)
        }
    }

    /**
     * Clears all storage.
     */
    fun clear() {
        storage.clear()
    }

    /**
     * Returns the number of entries in storage.
     */
    fun size(): Int = storage.size

    /**
     * Checks if storage contains a key.
     */
    fun containsKey(key: ByteArray): Boolean {
        return storage.containsKey(ByteArrayWrapper(key))
    }

    /**
     * Wrapper class for ByteArray to use as map key with proper equals/hashCode.
     */
    private class ByteArrayWrapper(val data: ByteArray) : Comparable<ByteArrayWrapper> {
        override fun equals(other: Any?): Boolean {
            if (this === other) return true
            if (other !is ByteArrayWrapper) return false
            return data.contentEquals(other.data)
        }

        override fun hashCode(): Int = data.contentHashCode()

        override fun compareTo(other: ByteArrayWrapper): Int {
            val minLen = minOf(data.size, other.data.size)
            for (i in 0 until minLen) {
                val cmp = (data[i].toInt() and 0xFF) - (other.data[i].toInt() and 0xFF)
                if (cmp != 0) return cmp
            }
            return data.size - other.data.size
        }
    }
}
