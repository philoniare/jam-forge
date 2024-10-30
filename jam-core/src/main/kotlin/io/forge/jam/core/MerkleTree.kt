import org.bouncycastle.jcajce.provider.digest.Blake2b

// Blake2b-256 hash function
fun blakeHash(data: ByteArray): ByteArray {
    val digest = Blake2b.Blake2b256()
    digest.update(data, 0, data.size)
    return digest.digest()
}

// GP (286): Branch function
fun branch(l: ByteArray, r: ByteArray): ByteArray {
    require(l.size == 32) { "Left child must be 32 bytes" }
    require(r.size == 32) { "Right child must be 32 bytes" }
    val head = (l[0].toInt() and 0xfe).toByte()
    return byteArrayOf(head) + l.sliceArray(1 until l.size) + r
}

// GP (287): Leaf function
fun leaf(k: ByteArray, v: ByteArray): ByteArray {
    return if (v.size <= 32) {
        val head = (0b01 or (v.size shl 2)).toByte()
        byteArrayOf(head) + k.sliceArray(0 until k.size - 1) + v + ByteArray(32 - v.size)
    } else {
        val head = 0b11.toByte()
        byteArrayOf(head) + k.sliceArray(0 until k.size - 1) + blakeHash(v)
    }
}

// Bit extraction function
fun bit(k: ByteArray, i: Int): Boolean {
    return (k[i shr 3].toInt() and (1 shl (i and 7))) != 0
}

// GP (289): Merkle function
fun merkle(kvs: List<Pair<ByteArray, ByteArray>>, i: Int = 0): ByteArray {
    if (kvs.isEmpty()) {
        return ByteArray(32) { 0 }
    }
    val encoded = if (kvs.size == 1) {
        leaf(kvs[0].first, kvs[0].second)
    } else {
        val l = mutableListOf<Pair<ByteArray, ByteArray>>()
        val r = mutableListOf<Pair<ByteArray, ByteArray>>()
        for ((k, v) in kvs) {
            if (bit(k, i)) {
                r.add(k to v)
            } else {
                l.add(k to v)
            }
        }
        branch(merkle(l, i + 1), merkle(r, i + 1))
    }
    require(encoded.size == 64) { "Encoded length must be 64 bytes" }
    return blakeHash(encoded)
}
