package io.forge.jam.safrole.stats

private const val SLOTS_PER_EPOCH = 600

class StatStateTransition {
    fun transition(
        input: StatInput,
        preState: StatState
    ): Pair<StatState, StatOutput?> {
        val postState = preState.copy()
        val preEpoch = preState.tau / SLOTS_PER_EPOCH
        val postEpoch = input.slot / SLOTS_PER_EPOCH
        if (postEpoch > preEpoch) {
            // Rotate stats on epoch transition
            postState.pi.last = postState.pi.current
            postState.pi.current = List(postState.pi.current.size) { StatCount() }
        }

        // Update current epoch stats for block author
        postState.pi.current[input.authorIndex.toInt()].blocks++

        // Update tickets
        input.extrinsic.tickets.forEach { _ ->
            postState.pi.current[input.authorIndex.toInt()].tickets++
        }

        // Update preimages
        input.extrinsic.preimages.forEach { preimage ->
            postState.pi.current[input.authorIndex.toInt()].preImages++
            postState.pi.current[input.authorIndex.toInt()].preImagesSize += preimage.blob.size
        }

        // Update guarantees
        input.extrinsic.guarantees.forEach { guarantee ->
            guarantee.signatures.forEach { sig ->
                postState.pi.current[sig.validatorIndex.toInt()].guarantees++
            }
        }

        // Update assurances
        input.extrinsic.assurances.forEach { assurance ->
            postState.pi.current[assurance.validatorIndex.toInt()].assurances++
        }

        return Pair(postState, null)
    }
}
