package io.forge.jam.core

fun jamShuffleList(list: List<Int>, entropy: JamByteArray): List<Int> {
    val entropyNumbers = computeQ(entropy, list.size)
    return computeShuffleEq329(list, entropyNumbers)
}

fun jamComputeShuffle(size: Int, entropy: JamByteArray): List<Int> {
    if (size == 0) return emptyList()

    val sequence = (0 until size).toList()
    val entropyNumbers = computeQ(entropy, size)
    return computeShuffleEq329(sequence, entropyNumbers)
}

private fun computeShuffleEq329(sequence: List<Int>, entropyNumbers: List<Int>): List<Int> {
    if (sequence.isEmpty()) return emptyList()

    val currentSize = sequence.size
    val index = (entropyNumbers[0].toUInt() % currentSize.toUInt()).toInt()
    val head = sequence[index]

    val sequencePost = sequence.toMutableList()
    sequencePost[index] = sequencePost[currentSize - 1]

    return listOf(head) + computeShuffleEq329(
        sequencePost.subList(0, currentSize - 1),
        entropyNumbers.subList(1, entropyNumbers.size)
    )
}

private fun computeQ(hash: JamByteArray, length: Int): List<Int> {
    return (0 until length).map { i ->
        val counter = i / 8
        val preimage = ByteArray(hash.size + 4).apply {
            hash.copyInto(this)
            counter.toLeBytes().copyInto(this, hash.size)
        }

        val hashed = blakeHash(preimage)
        val offset = (4 * i) % 32
        hashed.sliceArray(offset until offset + 4).fromLeBytesUInt().toInt()
    }
}

private fun Int.toLeBytes(size: Int = 4): ByteArray = ByteArray(size) { i ->
    ((this shr (8 * i)) and 0xFF).toByte()
}

/**
 * Convert bytes to UInt using little-endian format
 */
private fun ByteArray.fromLeBytesUInt(): UInt {
    require(size == 4) { "Need exactly 4 bytes to convert to UInt" }
    return (this[0].toUByte().toUInt()) or
        (this[1].toUByte().toUInt() shl 8) or
        (this[2].toUByte().toUInt() shl 16) or
        (this[3].toUByte().toUInt() shl 24)
}
