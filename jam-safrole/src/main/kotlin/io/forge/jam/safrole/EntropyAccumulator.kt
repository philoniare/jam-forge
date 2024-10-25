package io.forge.jam.safrole

import blake2b256

data class EntropyAccumulator(
    var current: ByteArray = ByteArray(32),
    var lastEpoch: ByteArray = ByteArray(32),
    var twoEpochsAgo: ByteArray = ByteArray(32),
    var threeEpochsAgo: ByteArray = ByteArray(32)
) {
    // Accumulate VRF output into the current entropy
    fun accumulateVrf(vrfOutput: ByteArray) {
        current = blake2b256(current + vrfOutput)
    }

    // Rotate entropy accumulators on epoch transition
    fun rotate() {
        threeEpochsAgo = twoEpochsAgo
        twoEpochsAgo = lastEpoch
        lastEpoch = current
        current = ByteArray(32) // Reset current entropy
    }
}
