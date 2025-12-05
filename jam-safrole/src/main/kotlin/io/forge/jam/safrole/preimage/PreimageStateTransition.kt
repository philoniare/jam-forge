package io.forge.jam.safrole.preimage

import io.forge.jam.core.blakeHash
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.safrole.traces.StateKeys

class PreimageStateTransition {
    fun transition(
        input: PreimageInput,
        preState: PreimageState
    ): Pair<PreimageState, PreimageOutput> {
        // First check if all preimages are needed (account exists, lookup entry exists with empty value)
        for (submission in input.preimages) {
            val account = preState.accounts.find { it.id == submission.requester }
                ?: return Pair(preState, PreimageOutput(err = PreimageErrorCode.PREIMAGE_UNNEEDED))

            val hash = blakeHash(submission.blob.bytes)
            val length = submission.blob.size.toLong()

            // Verify this preimage was solicited and not already available
            // First check lookupMeta (typed state)
            val historyEntry = account.data.lookupMeta.find { historyItem ->
                historyItem.key.hash.bytes.contentEquals(hash) &&
                    historyItem.key.length == length &&
                    historyItem.value.isEmpty()
            }

            // If not found in lookupMeta, check raw keyvals
            val isSolicited = if (historyEntry != null) {
                true
            } else {
                // Compute the expected state key and check raw keyvals
                val stateKey = StateKeys.servicePreimageInfoKey(
                    submission.requester.toInt(),
                    length.toInt(),
                    JamByteArray(hash)
                )
                val rawValue = input.rawServiceDataByStateKey.entries.find { (k, _) ->
                    k.bytes.contentEquals(stateKey.bytes)
                }?.value

                if (rawValue != null) {
                    // Check if value is empty timestamp list (compact(0) = 0x00)
                    val (timestampCount, _) = decodeCompactInteger(rawValue.bytes, 0)
                    timestampCount == 0L
                } else {
                    false
                }
            }

            if (!isSolicited) {
                return Pair(preState, PreimageOutput(err = PreimageErrorCode.PREIMAGE_UNNEEDED))
            }
        }

        // Then validate that preimages are sorted and unique by (requester, hash)
        if (!arePreimagesSortedAndUnique(input.preimages)) {
            return Pair(preState, PreimageOutput(err = PreimageErrorCode.PREIMAGES_NOT_SORTED_UNIQUE))
        }

        val postState = preState.copy(
            accounts = preState.accounts.map { it.copy() }
        )

        for (submission in input.preimages) {
            val account = postState.accounts.find { it.id == submission.requester }!!
            val hash = blakeHash(submission.blob.bytes)
            val length = submission.blob.size.toLong()

            // Check if we need to add a new lookupMeta entry (from raw keyvals)
            val existsInLookupMeta = account.data.lookupMeta.any { item ->
                item.key.hash.bytes.contentEquals(hash) && item.key.length == length
            }

            val baseHistory = if (existsInLookupMeta) {
                account.data.lookupMeta
            } else {
                // Add a new entry for this preimage from raw keyvals
                account.data.lookupMeta + PreimageHistory(
                    key = PreimageHistoryKey(hash = JamByteArray(hash), length = length),
                    value = emptyList()
                )
            }

            // Update history and preimages
            val updatedHistory = baseHistory.map { item ->
                if (item.key.hash.bytes.contentEquals(hash) &&
                    item.key.length == length
                ) {
                    item.copy(value = listOf(input.slot))
                } else {
                    item
                }
            }.sortedBy { it.key.hash.toHex() }

            val updatedPreimages = (account.data.preimages + PreimageHash(
                hash = JamByteArray(hash),
                blob = submission.blob
            )).sortedBy { it.hash.toHex() }

            // Update account state
            account.data.lookupMeta = updatedHistory
            account.data.preimages = updatedPreimages
        }

        return Pair(postState, PreimageOutput(ok = null))
    }

    /**
     * Check that preimages are sorted by (requester, hash) and unique.
     * Sorting is ascending by requester ID, then by blake2b hash of the blob.
     */
    private fun arePreimagesSortedAndUnique(preimages: List<PreimageExtrinsic>): Boolean {
        if (preimages.size <= 1) return true

        var prevRequester: Long? = null
        var prevHash: ByteArray? = null

        for (submission in preimages) {
            val currentHash = blakeHash(submission.blob.bytes)

            if (prevRequester != null && prevHash != null) {
                val comparison = comparePreimages(prevRequester, prevHash, submission.requester, currentHash)
                // Must be strictly less than (sorted and unique means no duplicates)
                if (comparison >= 0) {
                    return false
                }
            }

            prevRequester = submission.requester
            prevHash = currentHash
        }

        return true
    }

    /**
     * Compare two preimages by (requester, hash).
     * Returns negative if first < second, 0 if equal, positive if first > second.
     */
    private fun comparePreimages(
        requester1: Long,
        hash1: ByteArray,
        requester2: Long,
        hash2: ByteArray
    ): Int {
        // First compare by requester
        val requesterComparison = requester1.compareTo(requester2)
        if (requesterComparison != 0) {
            return requesterComparison
        }

        // Then compare by hash (lexicographically)
        for (i in hash1.indices) {
            val byte1 = hash1[i].toInt() and 0xFF
            val byte2 = hash2[i].toInt() and 0xFF
            if (byte1 != byte2) {
                return byte1 - byte2
            }
        }
        return 0
    }
}
