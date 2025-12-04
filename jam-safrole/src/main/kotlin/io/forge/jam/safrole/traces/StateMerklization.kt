package io.forge.jam.safrole.traces

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.blakeHash

/**
 * State Merklization function implementing the JAM protocol state trie.
 *
 * This is a binary Merkle tree where:
 * - Keys are 31 bytes (248 bits)
 * - At each level, keys are split based on bit at position i
 * - Leaves are encoded differently based on value size (embedded vs regular)
 */
object StateMerklization {

    private val ZERO_HASH = JamByteArray(ByteArray(32))

    /**
     * Compute state root from key-value pairs.
     *
     * @param kv Map of 31-byte keys to variable-length values
     * @param bitIndex Current bit position for tree split (0-247)
     * @return 32-byte state root hash
     */
    fun stateMerklize(kv: Map<JamByteArray, JamByteArray>, bitIndex: Int = 0): JamByteArray {
        // Empty case - return zero hash
        if (kv.isEmpty()) {
            return ZERO_HASH
        }

        // Single leaf case
        if (kv.size == 1) {
            val entry = kv.entries.first()
            return JamByteArray(blakeHash(leaf(entry.key, entry.value)))
        }

        // Split keys based on bit at position bitIndex
        val left = mutableMapOf<JamByteArray, JamByteArray>()
        val right = mutableMapOf<JamByteArray, JamByteArray>()

        for ((key, value) in kv) {
            if (getBit(key.bytes, bitIndex)) {
                right[key] = value
            } else {
                left[key] = value
            }
        }

        // Recurse and combine
        val leftHash = stateMerklize(left, bitIndex + 1)
        val rightHash = stateMerklize(right, bitIndex + 1)

        return JamByteArray(blakeHash(branch(leftHash, rightHash)))
    }

    /**
     * Convenience method that takes a List<KeyValue> instead of Map.
     */
    fun stateMerklize(keyvals: List<KeyValue>): JamByteArray {
        val map = mutableMapOf<JamByteArray, JamByteArray>()
        for (kv in keyvals) {
            map[kv.key] = kv.value
        }
        return stateMerklize(map)
    }

    /**
     * Create branch node (64 bytes).
     * First byte has high bit cleared to indicate branch.
     */
    private fun branch(left: JamByteArray, right: JamByteArray): ByteArray {
        val data = ByteArray(64)
        // Copy left hash, clearing high bit of first byte
        System.arraycopy(left.bytes, 0, data, 0, 32)
        data[0] = (data[0].toInt() and 0x7F).toByte() // Clear high bit
        // Copy right hash
        System.arraycopy(right.bytes, 0, data, 32, 32)
        return data
    }

    /**
     * Create leaf node (64 bytes).
     * Uses embedded format for values <= 32 bytes, regular format otherwise.
     */
    private fun leaf(key: JamByteArray, value: JamByteArray): ByteArray {
        return if (value.size <= 32) {
            embeddedLeaf(key, value)
        } else {
            regularLeaf(key, value)
        }
    }

    /**
     * Embedded leaf format (value <= 32 bytes):
     * Byte 0: 0x80 | size (high bit set, size in lower 6 bits)
     * Bytes 1-31: key (31 bytes)
     * Bytes 32-63: value + zero padding
     */
    private fun embeddedLeaf(key: JamByteArray, value: JamByteArray): ByteArray {
        require(key.size == 31) { "Key must be 31 bytes" }
        require(value.size <= 32) { "Value must be <= 32 bytes for embedded leaf" }

        val data = ByteArray(64)
        // First byte: 0x80 | size
        data[0] = (0x80 or value.size).toByte()
        // Key (31 bytes)
        System.arraycopy(key.bytes, 0, data, 1, 31)
        // Value + padding
        System.arraycopy(value.bytes, 0, data, 32, value.size)
        // Remaining bytes are already 0 (zero padding)
        return data
    }

    /**
     * Regular leaf format (value > 32 bytes):
     * Byte 0: 0xC0 (high 2 bits set)
     * Bytes 1-31: key (31 bytes)
     * Bytes 32-63: blake2b(value)
     */
    private fun regularLeaf(key: JamByteArray, value: JamByteArray): ByteArray {
        require(key.size == 31) { "Key must be 31 bytes" }

        val data = ByteArray(64)
        // First byte: 0xC0
        data[0] = 0xC0.toByte()
        // Key (31 bytes)
        System.arraycopy(key.bytes, 0, data, 1, 31)
        // Hash of value
        val valueHash = blakeHash(value.bytes)
        System.arraycopy(valueHash, 0, data, 32, 32)
        return data
    }

    /**
     * Get bit at position i from data.
     * Bit 0 is the MSB of byte 0, bit 7 is the LSB of byte 0, etc.
     */
    private fun getBit(data: ByteArray, i: Int): Boolean {
        val byteIndex = i / 8
        if (byteIndex >= data.size) {
            return false
        }
        val bitIndex = 7 - (i % 8) // MSB first
        return (data[byteIndex].toInt() and (1 shl bitIndex)) != 0
    }
}
