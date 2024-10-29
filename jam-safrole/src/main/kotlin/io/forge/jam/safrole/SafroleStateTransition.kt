package io.forge.jam.safrole

import io.forge.jam.core.EpochMark
import io.forge.jam.core.JamErrorCode
import io.forge.jam.core.TicketEnvelope
import io.forge.jam.core.toHex
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

            // Validate slot transition (eq. 42)
            if (input.slot <= preState.tau) {
                return Pair(postState, SafroleOutput(err = JamErrorCode.BAD_SLOT))
            }

            // Calculate epoch data (eq. 47)
            val prevEpoch = preState.tau / EPOCH_LENGTH
            val prevPhase = preState.tau % EPOCH_LENGTH
            val newEpoch = input.slot / EPOCH_LENGTH
            val newPhase = input.slot % EPOCH_LENGTH

            // Create mutable post state
            var epochMark: EpochMark? = null
            var ticketsMark: List<TicketBody>? = null


            // Handle epoch transition if needed
            if (newEpoch > prevEpoch) {
                val transitionResult = handleEpochTransition(postState, preState, prevPhase, preState.eta[0].clone())
                epochMark = transitionResult.first
                ticketsMark = transitionResult.second
            }

            // Process entropy accumulation (eq. 67)
            postState.eta[0] = blake2b256(preState.eta[0] + input.entropy)

            // Process ticket submissions if any (eq. 74-80)
            if (input.extrinsic.isNotEmpty()) {
                val ticketResult = processExtrinsics(postState, input.extrinsic, newPhase)
                if (ticketResult != null) {
                    return Pair(postState, SafroleOutput(err = ticketResult))
                }
            }

            // Update timeslot
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
        originalEta0: ByteArray
    ): Pair<EpochMark?, List<TicketBody>?> {
        // 5.1. Rotate entropy values (eq. 68)
        postState.eta[3] = preState.eta[2]
        postState.eta[2] = preState.eta[1]
        postState.eta[1] = originalEta0

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
        tickets: List<TicketEnvelope>,
        phase: Long
    ): JamErrorCode? {
        // Skip if in epoch tail
        if (phase >= TICKET_CUTOFF) {
            return JamErrorCode.UNEXPECTED_TICKET
        }

        val newTickets = mutableListOf<TicketBody>()

        for (ticket in tickets) {
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
//        RustLibrary.use(bandersnatchKeys.size, 0) { (_, verifierPtr) ->
//
//        }
        return ByteArray(0)
    }

    private fun verifyRingProof(
        proof: ByteArray,
        ringRoot: ByteArray,
        entropy: ByteArray,
        entryIndex: Long
    ): Boolean {
        // Implement Ring VRF proof verification
        println("proof: ${proof.toHex()}")
        println("RingRoot: ${ringRoot.toHex()}")
        println("entropy: ${entropy.toHex()}")
        println("entryIndex: ${entryIndex}")
        return true
    }

    private fun extractVrfOutput(proof: ByteArray): ByteArray {
        // Extract VRF output from proof
        return byteArrayOf(0)
    }

    private fun generateFallbackSequence(
        entropy: ByteArray,
        validators: List<ValidatorKey>
    ): List<ByteArray> {
        val result = ArrayList<ByteArray>(EPOCH_LENGTH.toInt())
        val bandersnatchKeys = validators.map { it.bandersnatch }

        for (i in 0 until EPOCH_LENGTH.toInt()) {
            // Little-endian encode the index i
            val indexBytes = ByteArray(4)
            indexBytes[0] = (i and 0xFF).toByte()
            indexBytes[1] = ((i shr 8) and 0xFF).toByte()
            indexBytes[2] = ((i shr 16) and 0xFF).toByte()
            indexBytes[3] = ((i shr 24) and 0xFF).toByte()

            // Generate slot-specific entropy
            val input = entropy + indexBytes
            val hashOutput = blake2b256(input)
            val selectionBytes = hashOutput.copyOfRange(0, 4)

            // Convert to unsigned integer
            val unsignedValue = (selectionBytes[0].toLong() and 0xFF) or
                ((selectionBytes[1].toLong() and 0xFF) shl 8) or
                ((selectionBytes[2].toLong() and 0xFF) shl 16) or
                ((selectionBytes[3].toLong() and 0xFF) shl 24)

            val index = (unsignedValue % validators.size).toInt()

            result.add(bandersnatchKeys[index])
        }

        return result
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
