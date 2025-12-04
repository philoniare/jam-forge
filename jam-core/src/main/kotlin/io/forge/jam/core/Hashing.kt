package io.forge.jam.core

import org.bouncycastle.crypto.digests.KeccakDigest
import org.bouncycastle.jcajce.provider.digest.Blake2b

/**
 * Blake2b-256 hash function.
 */
fun blakeHash(data: ByteArray): ByteArray {
    val digest = Blake2b.Blake2b256()
    digest.update(data, 0, data.size)
    return digest.digest()
}

/**
 * Keccak-256 hash function.
 */
fun keccakHash(data: ByteArray): ByteArray {
    val digest = KeccakDigest(256)
    val output = ByteArray(32)
    digest.update(data, 0, data.size)
    digest.doFinal(output, 0)
    return output
}
