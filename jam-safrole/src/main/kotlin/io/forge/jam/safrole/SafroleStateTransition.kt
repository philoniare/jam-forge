package io.forge.jam.safrole

import io.forge.jam.core.EpochMark
import io.forge.jam.core.Extrinsic
import io.forge.jam.core.JamErrorCode
import io.forge.jam.vrfs.RustLibrary
import io.forge.jam.vrfs.use
import org.bouncycastle.crypto.digests.Blake2bDigest

object SafroleStateTransition {
    const val EPOCH_LENGTH: Long = 12
    const val TICKET_CUTOFF: Long = 500

    fun transition(
        input: SafroleInput,
        preState: SafroleState
    ): Pair<SafroleState, SafroleOutput> {
        try {
            val postState = preState.copy()

            // 1. Validate slot transition (eq. 42)
            if (input.slot <= preState.tau) {
                return Pair(postState, SafroleOutput(err = JamErrorCode.BAD_SLOT))
            }

            // 2. Calculate epoch data (eq. 47)
            val prevEpoch = preState.tau / EPOCH_LENGTH
            val prevPhase = preState.tau % EPOCH_LENGTH
            val newEpoch = input.slot / EPOCH_LENGTH
            val newPhase = input.slot % EPOCH_LENGTH

            println("Debug: prevEpoch=$prevEpoch, newEpoch=$newEpoch")
            println("Debug: preState.tau=${preState.tau}, input.slot=${input.slot}")
            println("Debug: EPOCH_LENGTH=$EPOCH_LENGTH")

            // Create mutable post state
            var epochMark: EpochMark? = null
            var ticketsMark: List<TicketBody>? = null

            // 3. Process entropy accumulation (eq. 67)
            postState.eta[0] = blake2b256(preState.eta[0] + input.entropy)

            println("Print 0")
            // 4. Handle epoch transition if needed
            if (newEpoch > prevEpoch) {
                println("Print 1")
                val transitionResult = handleEpochTransition(postState, preState, prevPhase)
                epochMark = transitionResult.first
                ticketsMark = transitionResult.second
            }

            // 5. Process ticket submissions if any (eq. 74-80)
            if (input.extrinsic.isNotEmpty()) {
                val ticketResult = processExtrinsics(postState, input.extrinsic, newPhase)
                if (ticketResult != null) {
                    return Pair(postState, SafroleOutput(err = ticketResult))
                }
            }

            // 6. Update timeslot
            postState.tau = input.slot

            return Pair(
                postState, SafroleOutput(
                    ok = OutputMarks(
                        epochMark = epochMark,
                        ticketsMark = ticketsMark
                    )
                )
            )
        } catch (e: Exception) {
            return Pair(preState, SafroleOutput(err = JamErrorCode.RESERVED))
        }
    }

    private fun handleEpochTransition(
        postState: SafroleState,
        preState: SafroleState,
        prevPhase: Long,
    ): Pair<EpochMark?, List<TicketBody>?> {
        // 5.1. Rotate entropy values (eq. 68)
        postState.eta[3] = preState.eta[2]
        postState.eta[2] = preState.eta[1]
        postState.eta[1] = preState.eta[0]

        // 5.2. Rotate validator sets (eq. 58)
        postState.lambda = preState.kappa
        postState.kappa = preState.gammaK

        // 5.3. Load new pending validators
        postState.gammaK = preState.iota

        // 5.4. Generate new ring root
        postState.gammaZ = generateRingRoot(postState.gammaK)


        // 5.5. Generate epoch mark (eq. 72)
        val epochMark = EpochMark(
            entropy = postState.eta[1],
            validators = postState.gammaK.map { it.bandersnatch }
        )
        var ticketsMark: List<TicketBody>? = null

        // 5.6. Determine sealing sequence (eq. 69)
        if (prevPhase >= TICKET_CUTOFF && preState.gammaA.size == EPOCH_LENGTH.toInt()) {
            // Use accumulated tickets
            val ticketSequence = transformTicketsSequence(preState.gammaA)
            postState.gammaS = TicketsOrKeys.fromTickets(ticketSequence)

            // Generate tickets mark (eq. 73)
            ticketsMark = ticketSequence
        } else {
            // Use fallback sequence (eq. 71)
            val fallbackKeys = generateFallbackSequence(
                postState.eta[2],
                postState.kappa
            )
            postState.gammaS = TicketsOrKeys.fromKeys(fallbackKeys)
        }

        // 5.7. Clear ticket accumulator
        postState.gammaA = emptyList()

        return Pair(epochMark, ticketsMark)
    }

    private fun processExtrinsics(
        postState: SafroleState,
        extrinsics: List<Extrinsic>,
        phase: Long
    ): JamErrorCode? {
        // Skip if in epoch tail
        if (phase >= TICKET_CUTOFF) {
            return JamErrorCode.UNEXPECTED_TICKET
        }

        val newTickets = mutableListOf<TicketBody>()

        for (extrinsic in extrinsics) {
            for (ticket in extrinsic.tickets) {
                // Verify ring VRF proof
                if (!verifyRingProof(ticket.signature, postState.gammaZ, postState.eta[2], ticket.attempt)) {
                    return JamErrorCode.BAD_TICKET_PROOF
                }

                val ticketBody = TicketBody(
                    id = extractVrfOutput(ticket.signature),
                    attempt = ticket.attempt
                )

                // Check uniqueness (eq. 78)
                if (postState.gammaA.any { it.id.contentEquals(ticketBody.id) } ||
                    newTickets.any { it.id.contentEquals(ticketBody.id) }) {
                    return JamErrorCode.DUPLICATE_TICKET
                }

                newTickets.add(ticketBody)
            }
        }


        // Verify ordering
        if (!isOrderedByIdentifier(newTickets)) {
            return JamErrorCode.BAD_TICKET_ORDER
        }

        // Update accumulator with new tickets (eq. 79)
        if (newTickets.isNotEmpty()) {
            postState.gammaA = (postState.gammaA + newTickets)
                .sortedWith(Comparator { a, b -> a.id.compareTo(b.id) })
                .take(EPOCH_LENGTH.toInt())
        }

        return null
    }

    /**
     * Implements equation 77 from the gray paper.
     * Verifies that tickets are ordered by their identifier (VRF output).
     *
     * @param tickets The list of tickets to check ordering for
     * @return true if tickets are properly ordered by identifier, false otherwise
     */
    private fun isOrderedByIdentifier(tickets: List<TicketBody>): Boolean {
        if (tickets.size <= 1) return true

        // Compare each pair of adjacent tickets
        for (i in 0 until tickets.size - 1) {
            val current = tickets[i].id
            val next = tickets[i + 1].id

            // Compare byte arrays lexicographically
            // Must be strictly increasing (no equality allowed as per eq. 77)
            val comparison = current.compareTo(next)
            if (comparison >= 0) {  // If current is greater than or equal to next
                return false
            }
        }

        return true
    }

    /**
     * Helper extension function to compare ByteArrays lexicographically
     */
    private fun ByteArray.compareTo(other: ByteArray): Int {
        val len = minOf(this.size, other.size)
        for (i in 0 until len) {
            val diff = (this[i].toInt() and 0xFF) - (other[i].toInt() and 0xFF)
            if (diff != 0) {
                return diff
            }
        }
        return this.size - other.size
    }

    // Cryptographic helper functions
    private fun blake2b256(data: ByteArray): ByteArray {
        val digest = Blake2bDigest(256)
        digest.update(data, 0, data.size)
        val hash = ByteArray(32)
        digest.doFinal(hash, 0)
        return hash
    }

    private fun generateRingRoot(validators: List<ValidatorKey>): ByteArray {
        // Implement Bandersnatch ring root generation
        var bandersnatchKeys = validators.map { it.bandersnatch }
        RustLibrary.use(bandersnatchKeys.size, 0) { (_, verifierPtr) ->

        }
        TODO()
    }

    private fun verifyRingProof(
        proof: ByteArray,
        ringRoot: ByteArray,
        entropy: ByteArray,
        entryIndex: Long
    ): Boolean {
        // Implement Ring VRF proof verification
        TODO()
    }

    private fun extractVrfOutput(proof: ByteArray): ByteArray {
        // Extract VRF output from proof
        TODO()
    }

    private fun generateFallbackSequence(
        entropy: ByteArray,
        validators: List<ValidatorKey>
    ): List<ByteArray> {
        // Implement fallback sequence generation (eq. 71)
        TODO()
    }

    /**
     * Implements equation 70 from the gray paper:
     * Z: ⟦C⟧_E → ⟦C⟧_E
     * s ↦ [s_0, s_|s|-1, s_1, s_|s|-2, ...]
     *
     * This function takes the accumulated tickets and transforms them into the sealing sequence
     * by arranging them in an outside-in pattern:
     * - First element
     * - Last element
     * - Second element
     * - Second to last element
     * etc.
     */
    private fun transformTicketsSequence(tickets: List<TicketBody>): List<TicketBody> {
        val result = ArrayList<TicketBody>(tickets.size)
        var left = 0
        var right = tickets.size - 1

        while (left <= right) {
            // Add left element
            result.add(tickets[left])

            // Add right element if different from left
            if (left != right) {
                result.add(tickets[right])
            }

            left++
            right--
        }

        return result
    }
}
