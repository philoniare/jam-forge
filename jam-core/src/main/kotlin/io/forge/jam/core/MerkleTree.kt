package io.forge.jam.core

import org.bouncycastle.crypto.digests.KeccakDigest
import org.bouncycastle.jcajce.provider.digest.Blake2b

// Blake2b-256 hash function
fun blakeHash(data: ByteArray): ByteArray {
    val digest = Blake2b.Blake2b256()
    digest.update(data, 0, data.size)
    return digest.digest()
}

fun keccakHash(data: ByteArray): ByteArray {
    val digest = KeccakDigest(256)
    val output = ByteArray(32)
    digest.update(data, 0, data.size)
    digest.doFinal(output, 0)
    return output
}

/**
 * GP (286): Branch function
 * Creates a fork node by combining two child nodes in a Patricia Merkle Trie.
 *
 * @param l The left child node (32 bytes)
 * @param r The right child node (32 bytes)
 * @return Combined fork node (64 bytes)
 * @throws IllegalArgumentException if inputs are not 32 bytes
 */
fun fork(l: JamByteArray, r: JamByteArray): JamByteArray {
    require(l.size == 32) { "Left child must be 32 bytes" }
    require(r.size == 32) { "Right child must be 32 bytes" }

    val head = (l[0].toInt() and 0xfe).toByte()
    val bytes = ByteArray(64).apply {
        this[0] = head
        l.copyInto(this, 1, 1, 32)
        r.copyInto(this, 32, 0, 32)
    }

    return JamByteArray(bytes)
}

/**
 * GP (287): Leaf function
 * Creates a leaf node in a Patricia Merkle Trie.
 * For values <= 32 bytes: embeds the value directly with its length in the header
 * For values > 32 bytes: stores the hash of the value
 *
 * @param k The key (must be at least 1 byte)
 * @param v The value to store
 * @return Leaf node (64 bytes)
 * @throws IllegalArgumentException if key is empty
 */
fun leaf(k: JamByteArray, v: JamByteArray): JamByteArray {
    require(!k.isEmpty()) { "Key cannot be empty" }
    val bytes = ByteArray(64).apply {
        if (v.size <= 32) {
            // Set bit pattern 01 and encode length in upper 6 bits
            this[0] = (0b01 or (v.size shl 2)).toByte()

            // Copy key except last byte
            k.copyInto(this, 1, 0, k.size - 1)

            // Copy value and pad with zeros
            v.copyInto(this, 32, 0, v.size)
            // Note: remaining bytes are already 0 from ByteArray initialization
        } else {
            // Set bit pattern 11 for hashed values
            this[0] = 0b11.toByte()

            // Copy key except last byte
            k.copyInto(this, 1, 0, k.size - 1)

            // Hash the value and copy to last 32 bytes
            val hash = blakeHash(v.bytes)
            hash.copyInto(this, 32, 0, 32)
        }
    }

    return JamByteArray(bytes)
}


// Bit extraction function
fun bit(k: JamByteArray, i: Int): Boolean {
    return (k.bytes[i shr 3].toInt() and (1 shl (i and 7))) != 0
}

// GP (289): Merkle function
fun merkle(kvMap: Map<JamByteArray, JamByteArray>, i: Int = 0): JamByteArray {
    if (kvMap.isEmpty()) {
        return JamByteArray(ByteArray(32) { 0 })
    }

    val encoded: JamByteArray = if (kvMap.size == 1) {
        val entry = kvMap.entries.first()
        leaf(entry.key, entry.value)
    } else {
        val l = mutableMapOf<JamByteArray, JamByteArray>()
        val r = mutableMapOf<JamByteArray, JamByteArray>()

        for ((k, v) in kvMap) {
            if (bit(k, i)) {
                r[k] = v
            } else {
                l[k] = v
            }
        }
        fork(merkle(l, i + 1), merkle(r, i + 1))
    }
    require(encoded.size == 64) { "Encoded length must be 64 bytes" }
    return JamByteArray(blakeHash(encoded.bytes))
}
