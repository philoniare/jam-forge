package io.forge.jam.safrole.historical

import io.forge.jam.core.JamByteArray
import org.bouncycastle.crypto.digests.KeccakDigest

class HistoryTransition {
    companion object {
        const val BLOCK_HISTORY_LIMIT = 8
        const val MAX_CORES = 341
        val ZERO_HASH = ByteArray(32) { 0 }

        // MMR node prefix as
        private const val MMR_NODE_PREFIX = "\$mmr_node"
    }

    fun stf(input: HistoricalInput, preState: HistoricalState): HistoricalState {
        validateInputs(input)

        val updatedPreState = if (preState.beta.isNotEmpty()) {
            val lastBlock = preState.beta.last().copy(
                stateRoot = input.parentStateRoot
            )
            preState.copy(
                beta = preState.beta.dropLast(1) + lastBlock,
            )
        } else {
            preState
        }

        val newMmr = appendToMmr(
            if (updatedPreState.beta.isEmpty()) {
                HistoricalMmr(peaks = listOf())
            } else {
                updatedPreState.beta.last().mmr
            },
            input.accumulateRoot
        )
        val newBlock = HistoricalBeta(
            hash = input.headerHash,
            mmr = newMmr,
            stateRoot = JamByteArray(ZERO_HASH),
            reported = input.workPackages
        )

        val newBeta = (updatedPreState.beta + newBlock).takeLast(BLOCK_HISTORY_LIMIT)
        return HistoricalState(beta = newBeta)
    }

    private fun validateInputs(input: HistoricalInput) {
        require(input.headerHash.size == 32) { "Header hash must be 32 bytes" }
        require(input.parentStateRoot.size == 32) { "Parent state root must be 32 bytes" }
        require(input.accumulateRoot.size == 32) { "Accumulate root must be 32 bytes" }
        require(input.workPackages.size <= MAX_CORES) { "Too many work packages" }
        input.workPackages.forEachIndexed { index, pkg ->
            require(pkg.hash.size == 32) { "Work package $index must be 32 bytes" }
        }
    }

    /**
     * Append a new peak to the MMR following the mountain range rules.
     * Each peak represents a perfect binary tree of size 2^i where i is the index.
     * When merging, we combine adjacent peaks of the same size and push the result up.
     */
    private fun appendToMmr(previousMmr: HistoricalMmr, newPeak: JamByteArray): HistoricalMmr {
        val peaks = previousMmr.peaks.toMutableList()
        appendRecursive(peaks, newPeak, 0)
        return HistoricalMmr(peaks = peaks)
    }

    private fun appendRecursive(peaks: MutableList<JamByteArray?>, data: JamByteArray, index: Int) {
        when {
            // If we're beyond current peaks size, add new peak
            index >= peaks.size -> {
                peaks.add(data)
            }
            // If current position has a peak, merge and go up
            peaks[index] != null -> {
                val current = peaks[index]!!
                peaks[index] = null
                // Simply concatenate and hash the peaks
                val merged = keccakHash(current + data)
                appendRecursive(peaks, merged, index + 1)
            }
            // If current position is empty (null), place peak here
            else -> {
                peaks[index] = data
            }
        }
    }

    private fun keccakHash(data: JamByteArray): JamByteArray {
        val digest = KeccakDigest(256)
        val output = ByteArray(32)
        digest.update(data.bytes, 0, data.size)
        digest.doFinal(output, 0)
        return JamByteArray(output)
    }
}
