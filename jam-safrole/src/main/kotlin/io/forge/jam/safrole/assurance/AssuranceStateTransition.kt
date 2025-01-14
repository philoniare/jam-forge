package io.forge.jam.safrole.assurance

import blakeHash
import io.forge.jam.core.AssuranceExtrinsic
import io.forge.jam.core.WorkReport
import io.forge.jam.safrole.ValidatorKey
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer

fun ByteArray.isBitSet(position: Int): Boolean {
    val byteIndex = position / 8
    val bitIndex = position % 8
    if (byteIndex >= this.size) return false
    return (this[byteIndex].toInt() and (1 shl bitIndex)) != 0
}

class AssuranceStateTransition(private val assuranceConfig: AssuranceConfig) {
    private val JAM_AVAILABLE_PREFIX = "jam_available"
    private fun log(message: String) {
        println("[AssuranceStateTransition] $message")
    }

    private fun verifyAssuranceSignature(
        assurance: AssuranceExtrinsic,
        validatorKey: ValidatorKey
    ): Boolean {
        return try {
            // First create combined data and hash it
            val serializedData = assurance.anchor.bytes + assurance.bitfield.bytes
            val dataHash = blakeHash(serializedData)

            // Create final message by prepending context
            val signatureMessage = JAM_AVAILABLE_PREFIX.toByteArray() + dataHash

            // Verify using Ed25519
            val publicKey = Ed25519PublicKeyParameters(validatorKey.ed25519.bytes, 0)
            val signer = Ed25519Signer()
            signer.init(false, publicKey)
            signer.update(signatureMessage, 0, signatureMessage.size)

            signer.verifySignature(assurance.signature.bytes)
        } catch (e: Exception) {
            false
        }
    }

    private fun isReportTimedOut(timeout: Int, currentSlot: Int): Boolean {
        return currentSlot >= timeout + assuranceConfig.TIMEOUT_PERIOD
    }

    private fun handleTimeouts(state: AssuranceState, currentSlot: Int): AssuranceState {
        val newAssignments = state.availAssignments.mapIndexed { index, assignment ->
            if (assignment != null && isReportTimedOut(assignment.timeout.toInt(), currentSlot)) {
                null
            } else {
                assignment
            }
        }
        return state.copy(availAssignments = newAssignments)
    }

    private fun validateSortedAndUniqueValidators(assurances: List<AssuranceExtrinsic>): Boolean {
        return assurances.zipWithNext().all { (curr, next) ->
            curr.validatorIndex < next.validatorIndex
        }
    }

    private fun validateAssurances(input: AssuranceInput, state: AssuranceState): AssuranceErrorCode? {
        // Early return if no assurances
        if (input.assurances.isEmpty()) {
            return null
        }

        // Validate each assurance
        for (assurance in input.assurances) {
            // Check validator index bounds
            if (assurance.validatorIndex >= state.currValidators.size) {
                return AssuranceErrorCode.BAD_VALIDATOR_INDEX
            }

            // Check parent block hash matches
            if (assurance.anchor != input.parent) {
                return AssuranceErrorCode.BAD_ATTESTATION_PARENT
            }

            // Verify signature
            if (!verifyAssuranceSignature(assurance, state.currValidators[assurance.validatorIndex])) {
                return AssuranceErrorCode.BAD_SIGNATURE
            }
        }

        // Check for sorted and unique validator indices
        if (!validateSortedAndUniqueValidators(input.assurances)) {
            return AssuranceErrorCode.NOT_SORTED_OR_UNIQUE_ASSURERS
        }

        return null
    }

    private fun processAvailableReports(availableCores: Set<Int>, state: AssuranceState): List<WorkReport> {
        return availableCores.sorted().mapNotNull { coreIndex ->
            state.availAssignments.getOrNull(coreIndex)?.report
        }
    }

    private fun findAvailableCores(input: AssuranceInput, state: AssuranceState): Set<Int> {
        val validatorCount = state.currValidators.size
        val requiredAssurances = (validatorCount * 2) / 3

        // For each core, count how many validators have assured it
        return input.assurances.fold(mutableMapOf<Int, Int>()) { counts, assurance ->
            val bitfieldBytes = assurance.bitfield.bytes
            for (coreIndex in state.availAssignments.indices) {
                if (bitfieldBytes.isBitSet(coreIndex)) {
                    counts[coreIndex] = (counts[coreIndex] ?: 0) + 1
                }
            }
            counts
        }.filterValues { count ->
            count > requiredAssurances
        }.keys
    }

    private fun updateStateWithAvailableReports(
        state: AssuranceState,
        availableReports: List<WorkReport>
    ): AssuranceState {
        // Create new availability assignments list with available reports removed
        val newAssignments = state.availAssignments.mapIndexed { index, assignment ->
            if (assignment?.report in availableReports) null else assignment
        }

        return state.copy(availAssignments = newAssignments)
    }

    private fun validateCoreEngagement(input: AssuranceInput, state: AssuranceState): Boolean {
        return input.assurances.all { assurance ->
            val bitfieldBytes = assurance.bitfield.bytes

            state.availAssignments.indices.all { coreIndex ->
                val isSet = bitfieldBytes.isBitSet(coreIndex)
                val isEngaged = state.availAssignments.getOrNull(coreIndex)?.report != null

                if (isSet && !isEngaged) {
                    return@all false
                }
                true
            }
        }
    }

    fun transition(
        input: AssuranceInput,
        preState: AssuranceState
    ): Pair<AssuranceState, AssuranceOutput> {
        val postState = handleTimeouts(preState, input.slot.toInt())

        if (!validateCoreEngagement(input, preState)) {
            return Pair(postState, AssuranceOutput(err = AssuranceErrorCode.CORE_NOT_ENGAGED))
        }

        validateAssurances(input, preState)?.let { error ->
            return Pair(preState.copy(), AssuranceOutput(err = error))
        }

        val availableCores = findAvailableCores(input, preState)
        val availableReports = processAvailableReports(availableCores, preState)

        return if (availableReports.isEmpty()) {
            Pair(postState, AssuranceOutput(ok = AssuranceOutputMarks(emptyList())))
        } else {
            Pair(
                updateStateWithAvailableReports(postState, availableReports),
                AssuranceOutput(ok = AssuranceOutputMarks(availableReports))
            )
        }
    }
}
