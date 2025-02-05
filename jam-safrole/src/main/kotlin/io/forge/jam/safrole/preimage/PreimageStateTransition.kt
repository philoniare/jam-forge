package io.forge.jam.safrole.preimage

import blakeHash
import io.forge.jam.core.JamByteArray

class PreimageStateTransition {
    fun transition(
        input: PreimageInput,
        preState: PreimageState
    ): Pair<PreimageState, PreimageOutput> {
        for (submission in input.preimages) {
            val account = preState.accounts.find { it.id == submission.requester }
                ?: return Pair(preState, PreimageOutput(err = PreimageErrorCode.PREIMAGE_UNNEEDED))

            val hash = blakeHash(submission.blob.bytes)
            val length = submission.blob.size.toLong()

            // Verify this preimage was solicited and not already available
            val historyEntry = account.data.lookupMeta.find { historyItem ->
                historyItem.key.hash.bytes.contentEquals(hash) &&
                    historyItem.key.length == length &&
                    historyItem.value.isEmpty()
            } ?: return Pair(preState, PreimageOutput(err = PreimageErrorCode.PREIMAGE_UNNEEDED))
        }

        val postState = preState.copy(
            accounts = preState.accounts.map { it.copy() }
        )

        for (submission in input.preimages) {
            val account = postState.accounts.find { it.id == submission.requester }!!
            val hash = blakeHash(submission.blob.bytes)
            val length = submission.blob.size.toLong()

            // Update history and preimages
            val updatedHistory = account.data.lookupMeta.map { item ->
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
}
