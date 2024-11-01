package io.forge.jam.safrole

import io.forge.jam.core.*
import io.forge.jam.vrfs.BandersnatchWrapper
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer

private const val JAM_VALID = "jam_valid"
private const val JAM_INVALID = "jam_invalid"
private const val JAM_GUARANTEE = "jam_guarantee"

class SafroleStateTransition(private val config: SafroleConfig) {
    private val bandersnatchWrapper: BandersnatchWrapper = BandersnatchWrapper(config.ringSize)

    fun transition(
        input: SafroleInput,
        preState: SafroleState
    ): Pair<SafroleState, SafroleOutput> {
        try {
            val postState = preState.copy()

            // Create mutable post state
            var epochMark: EpochMark? = null
            var ticketsMark: List<TicketBody>? = null
            var offendersMark: List<ByteArray>? = null

            // Process disputes without slot advancement
            if (input.disputes != null) {
                offendersMark = processDisputes(input.disputes, postState)
                // If only processing disputes, return immediately
                if (input.slot == preState.tau) {
                    return Pair(postState, SafroleOutput(ok = OutputMarks(offendersMark = offendersMark)))
                }
            }

            // Validate slot transition (eq. 42)
            if (input.slot <= preState.tau) {
                return Pair(postState, SafroleOutput(err = JamErrorCode.BAD_SLOT))
            }

            // Calculate epoch data (eq. 47)
            val prevEpoch = preState.tau / config.epochLength
            val prevPhase = preState.tau % config.epochLength
            val newEpoch = input.slot / config.epochLength
            val newPhase = input.slot % config.epochLength


            // Check for tickets_mark condition
            // Same epoch AND crossing cutoff point Y and accumulator is full
            if (newEpoch == prevEpoch &&
                prevPhase < config.ticketCutoff &&
                newPhase >= config.ticketCutoff &&
                postState.gammaA.size.toLong() == config.epochLength
            ) {
                ticketsMark = transformTicketsSequence(postState.gammaA)
            }

            // Handle epoch transition if needed
            if (newEpoch > prevEpoch) {
                epochMark =
                    handleEpochTransition(
                        input,
                        postState,
                        preState,
                        newEpoch,
                        prevEpoch,
                        prevPhase,
                        preState.eta[0].clone()
                    )
            }

            // Process ticket submissions if any (eq. 74-80)
            if (input.extrinsic.isNotEmpty()) {
                val ticketResult = processExtrinsics(postState, input.extrinsic, newPhase)
                if (ticketResult != null) {
                    return Pair(postState, SafroleOutput(err = ticketResult))
                }
            }

            // Process entropy accumulation (eq. 67)
            postState.eta[0] = blake2b256(preState.eta[0] + input.entropy)

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
            println("Error: ${e.message}")
            return Pair(preState, SafroleOutput(err = JamErrorCode.RESERVED))
        }
    }

    private fun processDisputes(dispute: Dispute, postState: SafroleState): List<ByteArray> {
        // Track new offenders for the mark
        val offendersMark = mutableListOf<ByteArray>()
        // Initialize post_state variables
        if (postState.psi == null) {
            postState.psi = Psi(
                psiG = mutableListOf(),
                psiB = mutableListOf(),
                psiW = mutableListOf(),
                psiO = mutableListOf()
            )
        }

        if (postState.rho == null) {
            postState.rho = MutableList(341) { null }
        }

        // Process verdicts
        for (verdict in dispute.verdicts) {
            val reportHash = verdict.target
            val epochAge = verdict.age

            // Count positive votes
            val positiveVotes = verdict.votes.count { it.vote }

            // Get validator set based on age
            val validatorSet = if (epochAge == 0L) {
                postState.kappa
            } else {
                postState.lambda
            }

            // Verify all vote signatures
            for (vote in verdict.votes) {
                val validator = validatorSet[vote.index.toInt()]
                val message = if (vote.vote) {
                    JAM_VALID.toByteArray() + reportHash
                } else {
                    JAM_INVALID.toByteArray() + reportHash
                }

                verifyEd25519Signature(
                    publicKey = validator.ed25519,
                    message = message,
                    signature = vote.signature
                )
            }

            // Calculate vote thresholds
            val numValidators = validatorSet.size
            val superMajority = (2 * numValidators / 3) + 1
            val oneThird = numValidators / 3

            // Update PSI sets based on vote count
            when (positiveVotes) {
                superMajority -> postState.psi!!.psiG.add(reportHash)
                0 -> postState.psi!!.psiB.add(reportHash)
                oneThird -> postState.psi!!.psiW.add(reportHash)
            }
        }

        // Process culprits
        for (culprit in dispute.culprits) {
            val message = JAM_GUARANTEE.toByteArray() + culprit.target

            // Verify culprit signature
            verifyEd25519Signature(
                publicKey = culprit.key,
                message = message,
                signature = culprit.signature
            )

            val targetInPsiB = postState.psi!!.psiB.any { it.contentEquals(culprit.target) }
            val keyInPsiO = postState.psi!!.psiO.any { it.contentEquals(culprit.key) }

            // Add culprit if report is bad and validator not already punished
            if (targetInPsiB && !keyInPsiO) {
                postState.psi!!.psiO.add(culprit.key)
                offendersMark.add(culprit.key)
            }
        }

        // Process faults
        for (fault in dispute.faults) {
            val message = if (fault.vote) {
                JAM_VALID.toByteArray() + fault.target
            } else {
                JAM_INVALID.toByteArray() + fault.target
            }

            // Verify fault signature
            verifyEd25519Signature(
                publicKey = fault.key,
                message = message,
                signature = fault.signature
            )

            // Check if vote conflicts with verdict
            val isBad = fault.target in postState.psi!!.psiB
            val isGood = fault.target in postState.psi!!.psiG

            // Add fault if vote conflicts and validator not already punished
            if ((isBad != isGood) == fault.vote && fault.key !in postState.psi!!.psiO) {
                postState.psi!!.psiO.add(fault.key)
                println("Adding fault: ${fault.key.toHex()}")
                offendersMark.add(fault.key)
            }
        }

        // Clear invalid reports from rho state
        for (i in postState.rho!!.indices) {
            val report = postState.rho!![i] ?: continue

            val reportHash = blake2b256(report.dummyWorkReport)

            // Find matching verdict if any
            val verdict = dispute.verdicts.find { it.target.contentEquals(reportHash) }

            // Clear report if judged invalid/uncertain
            if (verdict != null) {
                val positiveVotes = verdict.votes.count { it.vote }
                val superMajority = (2 * postState.kappa.size / 3) + 1

                if (positiveVotes < superMajority) {
                    postState.rho!![i] = null
                }
            }
        }
        return offendersMark
    }

    private fun verifyEd25519Signature(
        publicKey: ByteArray,
        message: ByteArray,
        signature: ByteArray
    ) {
        // Validate input lengths
        if (publicKey.size != 32) {
            throw IllegalArgumentException("Ed25519 public key must be 32 bytes")
        }
        if (signature.size != 64) {
            throw IllegalArgumentException("Ed25519 signature must be 64 bytes")
        }
        if (message.isEmpty()) {
            throw IllegalArgumentException("Message cannot be empty")
        }

        try {
            // Create Ed25519 public key parameters
            val pubKeyParams = Ed25519PublicKeyParameters(publicKey, 0)

            // Create and initialize the signer in verification mode
            val signer = Ed25519Signer()
            signer.init(false, pubKeyParams)

            // Add message data
            signer.update(message, 0, message.size)

            // Verify the signature
            if (!signer.verifySignature(signature)) {
                throw IllegalArgumentException("Invalid Ed25519 signature")
            }
        } catch (e: Exception) {
            // Catch any BouncyCastle errors and convert to IllegalArgumentException
            throw IllegalArgumentException("Ed25519 verification failed: ${e.message}")
        }
    }

    private fun handleEpochTransition(
        input: SafroleInput,
        postState: SafroleState,
        preState: SafroleState,
        newEpoch: Long,
        prevEpoch: Long,
        prevPhase: Long,
        originalEta0: ByteArray
    ): EpochMark {
        // 5.1. Rotate entropy values (eq. 68)
        postState.eta[3] = preState.eta[2]
        postState.eta[2] = preState.eta[1]
        postState.eta[1] = originalEta0

        // 5.2. Rotate validator sets (eq. 58)
        postState.lambda = preState.kappa
        postState.kappa = preState.gammaK

        // 5.3. Load new pending validators
        postState.gammaK = preState.iota.map { validator ->
            if (input.postOffenders?.any { offender -> offender.contentEquals(validator.ed25519) } == true) {
                // Replace offender's entire validator key with zeros
                ValidatorKey(
                    bandersnatch = ByteArray(32) { 0 },
                    ed25519 = ByteArray(32) { 0 },
                    bls = ByteArray(144) { 0 },
                    metadata = ByteArray(128) { 0 }
                )
            } else {
                validator
            }
        }

        // 5.4. Generate new ring root
        postState.gammaZ = generateRingRoot(postState.gammaK)

        // 5.5. Generate epoch mark (eq. 72)
        val epochMark = EpochMark(
            entropy = postState.eta[1],
            validators = postState.gammaK.map { it.bandersnatch }
        )

        // 5.6. Determine sealing sequence (eq. 69)
        if (
            newEpoch == prevEpoch + 1 && // Moving to next epoch only
            prevPhase >= config.ticketCutoff && // Previous phase cutoff
            preState.gammaA.size == config.epochLength.toInt() // Accumulator full
        ) {
            // Use accumulated tickets
            val ticketSequence = transformTicketsSequence(preState.gammaA)
            postState.gammaS = TicketsOrKeys.fromTickets(ticketSequence)
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

        return epochMark
    }

    private fun processExtrinsics(
        postState: SafroleState,
        tickets: List<TicketEnvelope>,
        phase: Long
    ): JamErrorCode? {
        // Skip if in epoch tail
        if (phase >= config.ticketCutoff) {
            return JamErrorCode.UNEXPECTED_TICKET
        }

        val newTickets = mutableListOf<TicketBody>()

        for (ticket in tickets) {
            // Check for valid attempt value
            if (ticket.attempt > 1) {
                return JamErrorCode.BAD_TICKET_ATTEMPT
            }


            // Verify ring VRF proof
            val ticketId = verifyRingProof(ticket.signature, postState.gammaZ, postState.eta[2], ticket.attempt)
            if (ticketId.all { it == 0.toByte() }) {
                return JamErrorCode.BAD_TICKET_PROOF
            }

            val ticketBody = TicketBody(
                id = ticketId,
                attempt = ticket.attempt
            )

            // Check uniqueness (eq. 78)
            if (postState.gammaA.any { it.id.contentEquals(ticketBody.id) }) {
                return JamErrorCode.DUPLICATE_TICKET
            }

            newTickets.add(ticketBody)
        }


        // Verify ordering
        if (!hasStrictlyIncreasingIdentifiers(newTickets)) {
            return JamErrorCode.BAD_TICKET_ORDER
        }

        // Update accumulator with new tickets (eq. 79)
        if (newTickets.isNotEmpty()) {
            postState.gammaA = (postState.gammaA + newTickets)
                .sortedWith(Comparator { a, b -> a.id.compareTo(b.id) })
                .take(config.epochLength.toInt())
        }

        return null
    }

    /**
     * Implements equation 77 from gray paper.
     * Verifies that tickets are strictly ordered by their VRF output identifier
     * with no duplicates allowed.
     *
     * @param tickets The list of tickets to check ordering for
     * @return true if tickets are in strict ascending order by id, false otherwise
     */
    private fun hasStrictlyIncreasingIdentifiers(tickets: List<TicketBody>): Boolean {
        if (tickets.size <= 1) return true

        // Compare each pair of adjacent tickets
        for (i in 0 until tickets.size - 1) {
            val current = tickets[i].id
            val next = tickets[i + 1].id

            // Must be strictly increasing (eq. 77: [x_y __ x ∈ n])
            if (current.compareTo(next) >= 0) {
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
        var bandersnatchKeys = validators.map { it.bandersnatch }
        return bandersnatchWrapper.generateRingRoot(bandersnatchKeys, config.ringSize) ?: ByteArray(0)
    }

    private fun verifyRingProof(
        proof: ByteArray,
        ringRoot: ByteArray,
        entropy: ByteArray,
        entryIndex: Long
    ): ByteArray {
        // Implement Ring VRF proof verification
        return bandersnatchWrapper.verifyRingProof(entropy, entryIndex, proof, ringRoot, config.ringSize)
    }


    private fun generateFallbackSequence(
        entropy: ByteArray,
        validators: List<ValidatorKey>
    ): List<ByteArray> {
        val result = ArrayList<ByteArray>(config.epochLength.toInt())
        val bandersnatchKeys = validators.map { it.bandersnatch }

        for (i in 0 until config.epochLength.toInt()) {
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
