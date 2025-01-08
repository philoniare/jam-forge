package io.forge.jam.safrole.safrole

import io.forge.jam.core.*
import io.forge.jam.safrole.*
import io.forge.jam.vrfs.BandersnatchWrapper
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer

private const val JAM_VALID = "jam_valid"
private const val JAM_INVALID = "jam_invalid"
private const val JAM_GUARANTEE = "jam_guarantee"


class SafroleStateTransition(private val config: SafroleConfig) {
    private val bandersnatchWrapper: BandersnatchWrapper = BandersnatchWrapper(config.ringSize)

    private fun validateJudgementAge(
        verdict: Verdict,
        currentEpoch: Long
    ): SafroleErrorCode? {
        // Verdicts must use either:
        // - Current validator set (κ) if age == currentEpoch
        // - Previous validator set (λ) if age == currentEpoch - 1
        val validAges = setOf(currentEpoch, currentEpoch - 1)

        if (!validAges.contains(verdict.age)) {
            return SafroleErrorCode.BAD_JUDGEMENT_AGE
        }

        return null
    }

    private fun validateVoteDistribution(verdict: Verdict, validatorSet: List<ValidatorKey>): SafroleErrorCode? {
        val positiveVotes = verdict.votes.count { it.vote }
        val totalVotes = verdict.votes.size

        // Only allow:
        // - 0 (all negative) for invalid reports
        // - 1/3 for uncertain/"wonky" reports
        // - 2/3 + 1 for valid reports
        val validThresholds = setOf(
            0,
            config.oneThird,
            config.superMajority
        )

        if (!validThresholds.contains(positiveVotes)) {
            return SafroleErrorCode.BAD_VOTE_SPLIT
        }
        return null
    }

    fun transition(
        input: SafroleInput,
        preState: SafroleState
    ): Pair<SafroleState, SafroleOutput> {
        try {
            val postState = preState.deepCopy()

            // Create mutable post state
            var epochMark: EpochMark? = null
            var ticketsMark: List<TicketBody>? = null
            var offendersMark: List<JamByteArray>? = null

            // Process disputes without slot advancement
            if (input.disputes != null) {
                val (offendersList, error) = processDisputes(input.disputes, postState)
                offendersMark = offendersList

                if (error != null) {
                    return Pair(preState, SafroleOutput(err = error))
                }

                // If only processing disputes, return immediately
                if (input.slot == null) {
                    return Pair(postState, SafroleOutput(ok = OutputMarks(offendersMark = offendersMark)))
                }
            }

            // Validate slot transition (eq. 42)
            if (input.slot!! <= preState.tau) {
                return Pair(postState, SafroleOutput(err = SafroleErrorCode.BAD_SLOT))
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
            return Pair(preState, SafroleOutput(err = SafroleErrorCode.RESERVED))
        }
    }

    fun validateDisputes(
        disputes: Dispute,
        state: SafroleState,
    ): SafroleErrorCode? {
        val currentEpoch = state.tau / config.epochLength
        // 1. First validate signatures
        // This comes from equation 101 which requires valid signatures:
        // "s ∈ E_k ⟨X_G ⌢ r⟩"
        var isOrderValid = true
        val culprits = disputes.culprits
        if (culprits.isNotEmpty()) {
            for (i in 0 until culprits.size) {
                val culprit = culprits[i]
                val message = JamByteArray(JAM_GUARANTEE.toByteArray()) + culprit.target

                // Verify culprit signature
                if (!verifyEd25519Signature(
                        publicKey = culprit.key,
                        message = message,
                        signature = culprit.signature
                    )
                ) {
                    return SafroleErrorCode.BAD_SIGNATURE
                }
                // From equation 104: "c = [k __ {r, k, s} ∈ c]"
                if (i < culprits.size - 1) {
                    if (culprits[i].key.compareUnsigned(culprits[i + 1].key) >= 0) {
                        isOrderValid = false
                    }
                }


                if (state.psi?.offenders?.any { it.contentEquals(culprit.key) } == true) {
                    return SafroleErrorCode.OFFENDER_ALREADY_REPORTED
                }
            }
        }

        // 2. Validate culprits ordering
        if (!isOrderValid) {
            return SafroleErrorCode.CULPRITS_NOT_SORTED_UNIQUE
        }

        for (i in 0 until disputes.verdicts.size) {
            val verdict = disputes.verdicts[i]
            val ageError = validateJudgementAge(verdict, currentEpoch)
            if (ageError != null) {
                return ageError
            }

            val target = verdict.target
            val currentEpochIndex = state.tau / config.epochLength
            val validatorSet = if (verdict.age == currentEpochIndex) {
                state.kappa
            } else {
                state.lambda
            }

            val voteError = validateVoteDistribution(verdict, validatorSet)
            if (voteError != null) {
                return voteError
            }

            val positiveVotes = verdict.votes.count { it.vote }
            val totalVotes = verdict.votes.size
            if (positiveVotes == totalVotes &&
                positiveVotes >= config.superMajority &&
                disputes.faults.isEmpty()
            ) {
                // If we have all positive votes but no faults registered against
                // validators who may have incorrectly judged it invalid
                return SafroleErrorCode.NOT_ENOUGH_FAULTS
            }

            if (positiveVotes >= config.superMajority) {
                val hasFault = disputes.faults.any { fault ->
                    fault.target.contentEquals(target)
                }
                if (!hasFault) {
                    return SafroleErrorCode.NOT_ENOUGH_FAULTS
                }
            }

            // Validate
            val isGoodVerdict = positiveVotes >= config.superMajority
            val matchingFaults = disputes.faults.filter { it.target.contentEquals(target) }
            for (fault in matchingFaults) {
                // For a good verdict (supermajority positive votes),
                // faults must have vote=false to be valid
                // For a bad verdict (not supermajority positive),
                // faults must have vote=true to be valid
                if (fault.vote == isGoodVerdict) {
                    return SafroleErrorCode.FAULT_VERDICT_WRONG
                }
            }

            // Verify all vote signatures
            for (i in 0 until verdict.votes.size - 1) {
                val vote = verdict.votes[i]
                val validator = validatorSet[vote.index.toInt()]
                val message = if (vote.vote) {
                    JamByteArray(JAM_VALID.toByteArray()) + target
                } else {
                    JamByteArray(JAM_INVALID.toByteArray()) + target
                }

                if (!verifyEd25519Signature(
                        publicKey = validator.ed25519,
                        message = message,
                        signature = vote.signature
                    )
                ) {
                    return SafroleErrorCode.BAD_SIGNATURE
                }

                if (i < verdict.votes.size - 1) {
                    if (verdict.votes[i].index >= verdict.votes[i + 1].index) {
                        return SafroleErrorCode.JUDGEMENTS_NOT_SORTED_UNIQUE
                    }
                }
            }

            // Validate if any verdict target has already been judged
            if (state.psi?.good?.contains(target) == true ||
                state.psi?.bad?.contains(target) == true ||
                state.psi?.wonky?.contains(target) == true
            ) {
                return SafroleErrorCode.ALREADY_JUDGED
            }

            // Validate enough culprits for all-negative verdicts
            val negativeVotes = verdict.votes.count { !it.vote }
            if (negativeVotes == verdict.votes.size) {
                if (disputes.culprits.size < 2) {
                    return SafroleErrorCode.NOT_ENOUGH_CULPRITS
                }
            }

            if (i < disputes.verdicts.size - 1) {
                val current = disputes.verdicts[i].target
                val next = disputes.verdicts[i + 1].target
                if (current.compareUnsigned(next) >= 0) {
                    return SafroleErrorCode.VERDICTS_NOT_SORTED_UNIQUE
                }
            }
        }

        for (i in 0 until disputes.faults.size) {
            val fault = disputes.faults[i]
            if (i < disputes.faults.size - 1) {
                val currentKey = disputes.faults[i].key
                val nextKey = disputes.faults[i + 1].key

                if (currentKey.compareUnsigned(nextKey) >= 0) {
                    return SafroleErrorCode.FAULTS_NOT_SORTED_UNIQUE
                }
            }
            val message = if (fault.vote) {
                JamByteArray(JAM_VALID.toByteArray()) + fault.target
            } else {
                JamByteArray(JAM_INVALID.toByteArray()) + fault.target
            }

            if (!verifyEd25519Signature(
                    publicKey = fault.key,
                    message = message,
                    signature = fault.signature
                )
            ) {
                return SafroleErrorCode.BAD_SIGNATURE
            }

            if (state.psi?.offenders?.any { it.contentEquals(fault.key) } == true) {
                return SafroleErrorCode.OFFENDER_ALREADY_REPORTED
            }
        }

        return null
    }

    private fun processDisputes(
        dispute: Dispute,
        postState: SafroleState
    ): Pair<List<JamByteArray>, SafroleErrorCode?> {
        // Track new offenders for the mark
        val offendersMark = mutableListOf<JamByteArray>()
        // Validate culprits
        val errorResult = validateDisputes(dispute, postState)
        if (errorResult != null) {
            return Pair(offendersMark, errorResult)
        }

        // Initialize post_state variables
        if (postState.psi == null) {
            postState.psi = Psi(
                good = JamByteArrayList(),
                bad = JamByteArrayList(),
                wonky = JamByteArrayList(),
                offenders = JamByteArrayList()
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

            // Calculate vote thresholds
            val numValidators = validatorSet.size
            val superMajority = (2 * numValidators / 3) + 1
            val oneThird = numValidators / 3

            // Update PSI sets based on vote count
            when (positiveVotes) {
                superMajority -> postState.psi!!.good.add(reportHash)
                0 -> postState.psi!!.bad.add(reportHash)
                oneThird -> postState.psi!!.wonky.add(reportHash)
            }
        }

        // Verify that all culprit targets are marked as bad before processing them
        for (culprit in dispute.culprits) {
            if (culprit.target !in postState.psi!!.bad) {
                return Pair(emptyList(), SafroleErrorCode.CULPRITS_VERDICT_NOT_BAD)
            }
        }

        // Process culprits
        for (culprit in dispute.culprits) {
            val targetInPsiB = postState.psi!!.bad.any { it.contentEquals(culprit.target) }
            val keyInPsiO = postState.psi!!.offenders.any { it.contentEquals(culprit.key) }

            // Add culprit if report is bad and validator not already punished
            if (targetInPsiB && !keyInPsiO) {
                postState.psi!!.offenders.add(culprit.key)
                offendersMark.add(culprit.key)
            }
        }

        // Process faults
        for (fault in dispute.faults) {
            val isBad = fault.target in postState.psi!!.bad
            val isGood = fault.target in postState.psi!!.good

            // A fault exists if:
            // - Report is in psiG but validator voted false
            // - Report is in psiB but validator voted true
            val voteConflicts = (isGood && !fault.vote) || (isBad && fault.vote)
            val notPunished = fault.key !in postState.psi!!.offenders

            if (voteConflicts && notPunished) {
                postState.psi!!.offenders.add(fault.key)
                offendersMark.add(fault.key)
            }
        }

        // Clear invalid reports from rho state
        for (i in postState.rho!!.indices) {
            val report = postState.rho!![i] ?: continue

            val reportHash = blake2b256(JamByteArray(report.report.encode()))
            println(
                "Encoded: ${
                    report.report.encode().toHex()
                } ReportHash: ${reportHash.toHex()}. Bad: ${postState.psi!!.bad.joinToString { it.toHex() }}"
            )

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

            if (postState.psi!!.bad.any { it.contentEquals(reportHash) } ||
                postState.psi!!.wonky.any { it.contentEquals(reportHash) }) {
                postState.rho!![i] = null
            }
        }
        return Pair(offendersMark, null)
    }

    private fun verifyEd25519Signature(
        publicKey: JamByteArray,
        message: JamByteArray,
        signature: JamByteArray
    ): Boolean {
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
            val pubKeyParams = Ed25519PublicKeyParameters(publicKey.bytes, 0)

            // Create and initialize the signer in verification mode
            val signer = Ed25519Signer()
            signer.init(false, pubKeyParams)

            // Add message data
            signer.update(message.bytes, 0, message.size)

            // Verify the signature
            return signer.verifySignature(signature.bytes)
        } catch (e: Exception) {
            return true
        }
    }

    private fun handleEpochTransition(
        input: SafroleInput,
        postState: SafroleState,
        preState: SafroleState,
        newEpoch: Long,
        prevEpoch: Long,
        prevPhase: Long,
        originalEta0: JamByteArray
    ): EpochMark {
        // 5.1. Rotate entropy values (eq. 68)
        postState.eta[3] = preState.eta[2]
        postState.eta[2] = preState.eta[1]
        postState.eta[1] = originalEta0

        // 5.2. Rotate validator sets (eq. 58)
        postState.lambda = preState.kappa
        postState.kappa = preState.gammaK

        // 5.3. Load new pending validators
        println("Checking validators, ${preState.postOffenders}")
        postState.gammaK = preState.iota.map { validator ->
            val isOffender = preState.postOffenders?.any { offender ->
                println("Comparing offender: ${offender.toHex()} with validator ed25519: ${validator.ed25519.toHex()}")
                offender.contentEquals(validator.ed25519)
            } == true

            if (isOffender) {
                println("Found offender match - zeroing validator")
                ValidatorKey(
                    bandersnatch = JamByteArray(ByteArray(32) { 0 }),
                    ed25519 = JamByteArray(ByteArray(32) { 0 }),
                    bls = JamByteArray(ByteArray(144) { 0 }),
                    metadata = JamByteArray(ByteArray(128) { 0 })
                )
            } else {
                validator
            }
        }

        // 5.4. Generate new ring root
        postState.gammaZ = JamByteArray(generateRingRoot(postState.gammaK))

        // 5.5. Generate epoch mark (eq. 72)
        val epochMark = EpochMark(
            entropy = postState.eta[1],
            ticketsEntropy = postState.eta[2],
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
    ): SafroleErrorCode? {
        // Skip if in epoch tail
        if (phase >= config.ticketCutoff) {
            return SafroleErrorCode.UNEXPECTED_TICKET
        }

        val newTickets = mutableListOf<TicketBody>()

        for (ticket in tickets) {
            // Check for valid attempt value
            if (ticket.attempt > 1) {
                return SafroleErrorCode.BAD_TICKET_ATTEMPT
            }


            // Verify ring VRF proof
            val ticketId =
                verifyRingProof(ticket.signature.bytes, postState.gammaZ.bytes, postState.eta[2].bytes, ticket.attempt)
            if (ticketId.all { it == 0.toByte() }) {
                return SafroleErrorCode.BAD_TICKET_PROOF
            }

            val ticketBody = TicketBody(
                id = JamByteArray(ticketId),
                attempt = ticket.attempt
            )

            // Check uniqueness (eq. 78)
            if (postState.gammaA.any { it.id.contentEquals(ticketBody.id) }) {
                return SafroleErrorCode.DUPLICATE_TICKET
            }

            newTickets.add(ticketBody)
        }


        // Verify ordering
        if (!hasStrictlyIncreasingIdentifiers(newTickets)) {
            return SafroleErrorCode.BAD_TICKET_ORDER
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
    private fun blake2b256(data: JamByteArray): JamByteArray {
        val digest = Blake2bDigest(256)
        digest.update(data.bytes, 0, data.size)
        val hash = ByteArray(32)
        digest.doFinal(hash, 0)
        return JamByteArray(hash)
    }

    private fun generateRingRoot(validators: List<ValidatorKey>): ByteArray {
        val bandersnatchKeys = validators.map { it.bandersnatch.bytes }
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
        entropy: JamByteArray,
        validators: List<ValidatorKey>
    ): List<JamByteArray> {
        val result = ArrayList<JamByteArray>(config.epochLength.toInt())
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
