package io.forge.jam.safrole.report

import blakeHash
import io.forge.jam.core.GuaranteeExtrinsic
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.WorkReport
import io.forge.jam.safrole.AvailabilityAssignment
import io.forge.jam.safrole.ValidatorKey
import io.forge.jam.safrole.historical.HistoricalBeta
import keccakHash
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer


class ReportStateTransition(private val config: ReportStateConfig) {

    private fun calculatePeak(peaks: List<JamByteArray>): ByteArray {
        return when {
            peaks.isEmpty() -> ByteArray(32) { 0 }
            peaks.size == 1 -> peaks[0].bytes
            else -> {
                val subPeaks = peaks.dropLast(1)
                val lastPeak = peaks.last()
                val prefix = "node".toByteArray()
                val recursiveResult = calculatePeak(subPeaks)
                keccakHash(prefix + recursiveResult + lastPeak.bytes)
            }
        }
    }

    /**
     * Validates the Beefy root against the MMR peaks.
     * This would need to implement the actual MMR validation logic.
     */
    private fun validateBeefyRootAgainstMmrPeaks(
        beefyRoot: JamByteArray,
        mmrPeaks: List<JamByteArray?>
    ): Boolean {
        if (mmrPeaks.isEmpty()) {
            return false
        }

        val nonNullPeaks = mmrPeaks.filterNotNull()
        val root = calculatePeak(nonNullPeaks)
        return root.contentEquals(beefyRoot.bytes)
    }

    /**
     * Validates that guarantee extrinsics have valid anchors that are recent enough.
     *
     *
     * @param guarantees List of guarantees to validate
     * @param recentBlocks Recent block history (β)
     * @param currentSlot Current timeslot
     * @return ReportErrorCode if validation fails, null if successful
     */
    fun validateAnchor(
        guarantees: List<GuaranteeExtrinsic>,
        recentBlocks: List<HistoricalBeta>,
        currentSlot: Long
    ): ReportErrorCode? {
        guarantees.forEach { guarantee ->
            val context = guarantee.report.context

            // Validate lookup anchor is not too old
            if (currentSlot - context.lookupAnchorSlot > config.MAX_LOOKUP_ANCHOR_AGE) {
                return ReportErrorCode.ANCHOR_NOT_RECENT
            }

            // Find anchor block and lookup anchor block
            val lookupAnchorBlock = recentBlocks.find { it.headerHash.contentEquals(context.lookupAnchor) }
            if (lookupAnchorBlock == null) {
                return ReportErrorCode.ANCHOR_NOT_RECENT
            }

            val anchorBlock = recentBlocks.find { it.headerHash.contentEquals(context.anchor) }
            if (anchorBlock == null) {
                return ReportErrorCode.ANCHOR_NOT_RECENT
            }

            // Verify state root matches
            if (!anchorBlock.stateRoot.contentEquals(context.stateRoot)) {
                return ReportErrorCode.BAD_STATE_ROOT
            }

            // Verify MMR peaks
            val beefyRoot = context.beefyRoot
            val mmrPeaks = anchorBlock.mmr.peaks

            if (!validateBeefyRootAgainstMmrPeaks(beefyRoot, mmrPeaks)) {
                return ReportErrorCode.BAD_BEEFY_MMR_ROOT
            }

            // Validate prerequisites exist in recent history
            if (context.prerequisites.isNotEmpty()) {
                val prerequisitesExist = context.prerequisites.all { prerequisite ->
                    recentBlocks.any { block ->
                        block.reported.any { reported ->
                            reported.hash.contentEquals(prerequisite.bytes)
                        }
                    }
                }
                if (!prerequisitesExist) {
                    return ReportErrorCode.DEPENDENCY_MISSING
                }
            }
        }

        return null
    }

    /**
     * Checks if a work package with the given hash has already been reported in recent history.
     * This validates against section 4.8.2 of the JAM protocol specification.
     */
    private fun checkWorkPackageDuplicates(
        packageHash: JamByteArray,
        recentBlocks: List<HistoricalBeta>,
        seenHashes: MutableSet<JamByteArray>
    ): Boolean {
        if (!seenHashes.add(packageHash)) {
            return true
        }

        // Check if package hash exists in any recent block's reported work packages
        return recentBlocks.any { block ->
            block.reported.any { report ->
                report.hash.contentEquals(packageHash)
            }
        }
    }

    /**
     * Validates that guarantee extrinsics have no duplicate packages.
     *
     * @param guarantees List of guarantees to validate
     * @param recentBlocks Recent block history (β)
     * @return ReportErrorCode if validation fails, null if successful
     */
    private fun validateNoDuplicatePackages(
        guarantees: List<GuaranteeExtrinsic>,
        recentBlocks: List<HistoricalBeta>
    ): ReportErrorCode? {
        val seenHashes = mutableSetOf<JamByteArray>()
        // Check each guarantee's work package hash
        guarantees.forEach { guarantee ->
            val packageHash = guarantee.report.packageSpec.hash

            // Check if package exists in recent history
            if (checkWorkPackageDuplicates(packageHash, recentBlocks, seenHashes)) {
                return ReportErrorCode.DUPLICATE_PACKAGE
            }

            // Check if package hash exists in segment root lookups
            guarantee.report.segmentRootLookup.forEach { lookup ->
                if (checkWorkPackageDuplicates(lookup.workPackageHash, recentBlocks, seenHashes)) {
                    return ReportErrorCode.DUPLICATE_PACKAGE
                }
            }
        }

        return null
    }

    /**
     * Validates work report according to JAM protocol specifications.
     * Implements validation rules from sections 11.4 and 14.3 of the protocol.
     */
    fun validateWorkReport(
        workReport: WorkReport,
        guaranteeSlot: Long,
        currentSlot: Long,
        services: List<ServiceItem>,
        authPools: List<List<JamByteArray>>,
        availAssignments: List<AvailabilityAssignment?>
    ): ReportErrorCode? {
        if (guaranteeSlot > currentSlot) {
            return ReportErrorCode.FUTURE_REPORT_SLOT
        }

        val existingAssignment = availAssignments.getOrNull(workReport.coreIndex.toInt())
        if (existingAssignment?.report != null) {
            return ReportErrorCode.CORE_ENGAGED
        }

        // Validate core index is within bounds (eq. 143)
        if (workReport.coreIndex >= config.MAX_CORES) {
            return ReportErrorCode.BAD_CORE_INDEX
        }

        // Validate authorizer is present in auth pool for core (eq. 143)
        val coreAuthPool = authPools.getOrNull(workReport.coreIndex.toInt())
            ?: return ReportErrorCode.CORE_UNAUTHORIZED

        if (!coreAuthPool.any { it.contentEquals(workReport.authorizerHash) }) {
            return ReportErrorCode.CORE_UNAUTHORIZED
        }

        // Validate work results
        for (result in workReport.results) {
            // Validate service exists
            val service = services.find { it.id == result.serviceId } ?: return ReportErrorCode.BAD_SERVICE_ID

            // Validate code hash matches service state (eq. 156)
            if (!result.codeHash.contentEquals(service.info.codeHash)) {
                return ReportErrorCode.BAD_CODE_HASH
            }

            // Validate gas requirements (eq. 144)
            if (result.accumulateGas < service.info.minItemGas) {
                return ReportErrorCode.SERVICE_ITEM_GAS_TOO_LOW
            }
        }

        // Validate total gas limit (eq. 144)
        val totalGas = workReport.results.sumOf { it.accumulateGas }
        if (totalGas > config.MAX_ACCUMULATION_GAS) {
            return ReportErrorCode.WORK_REPORT_GAS_TOO_HIGH
        }

        // Validate dependencies (eq. 150-152)
        val prerequisites = workReport.context.prerequisites
        if (prerequisites.size > config.MAX_DEPENDENCIES) {
            return ReportErrorCode.TOO_MANY_DEPENDENCIES
        }

        // Validate segment root lookups (eq. 153-155)
        if (workReport.segmentRootLookup.size + prerequisites.size > config.MAX_DEPENDENCIES) {
            return ReportErrorCode.TOO_MANY_DEPENDENCIES
        }

        return null
    }

    private fun <T> Array<T>.shuffle(random: RandomGenerator) {
        for (i in size - 1 downTo 1) {
            val j = random.nextInt(i + 1)
            val temp = this[i]
            this[i] = this[j]
            this[j] = temp
        }
    }

    private fun calculateCoreAssignments(
        timeslot: Long,
        validators: List<ValidatorKey>,
        randomness: JamByteArray
    ): List<Int> {
//        validateRotationInput(timeslot, validators)

        // Calculate base assignments - one core per 3 validators
        val validatorsPerCore = validators.size / config.MAX_CORES

        // Create initial sequential assignments
        val baseAssignments = Array(validators.size) { validatorIndex ->
            (validatorIndex * config.MAX_CORES / validators.size).toInt()
        }

        // Create deterministic shuffle
        baseAssignments.shuffle(RandomGenerator(randomness))

        // Calculate rotation
        val rotationIndex = (timeslot / config.ROTATION_PERIOD).toInt()

        // Apply rotation
        return baseAssignments.map { core ->
            Math.floorMod(core + rotationIndex, config.MAX_CORES)
        }
    }

    fun validateGuarantorSignatures(
        guarantee: GuaranteeExtrinsic,
        currValidators: List<ValidatorKey>,
        prevValidators: List<ValidatorKey>,
        currentSlot: Long,
        entropyPool: List<JamByteArray>
    ): ReportErrorCode? {
        // Validate minimum number of signatures
        if (guarantee.signatures.size !in 2..3) {
            return ReportErrorCode.BAD_SIGNATURE
        }

        val reportedCore = guarantee.report.coreIndex.toInt()

        // Calculate current and previous assignments
        val currAssignments = calculateCoreAssignments(currentSlot, currValidators, entropyPool[2])
        val prevAssignments = if ((currentSlot % config.EPOCH_LENGTH) < config.ROTATION_PERIOD) {
            calculateCoreAssignments(currentSlot - config.ROTATION_PERIOD, prevValidators, entropyPool[3])
        } else {
            calculateCoreAssignments(currentSlot - config.ROTATION_PERIOD, currValidators, entropyPool[2])
        }

        // Calculate which set of assignments to use
        val isCurrent = (guarantee.slot / config.ROTATION_PERIOD) == (currentSlot / config.ROTATION_PERIOD)
        val assignments = if (isCurrent) currAssignments else prevAssignments
        val validators = if (isCurrent) currValidators else prevValidators

        // First verify all validator indices are valid
        for (signature in guarantee.signatures) {
            val validatorIndex = signature.validatorIndex.toInt()
            if (validatorIndex < 0 || validatorIndex >= validators.size) {
                return ReportErrorCode.BAD_VALIDATOR_INDEX
            }
        }

        // Verify at least one validator is assigned to the reported core
        var hasValidCoreAssignment = false
        for (signature in guarantee.signatures) {
            val validatorIndex = signature.validatorIndex.toInt()
            val assignedCore = assignments[validatorIndex]

            if (assignedCore == reportedCore) {
                hasValidCoreAssignment = true
            }

            // Validate signature
            val validatorKey = validators[validatorIndex].ed25519
            val signaturePrefix = "jam_guarantee".toByteArray()
            val signatureMessage = signaturePrefix + blakeHash(guarantee.report.encode())

            try {
                val publicKey = Ed25519PublicKeyParameters(validatorKey.bytes, 0)
                val signer = Ed25519Signer()
                signer.init(false, publicKey)
                signer.update(signatureMessage, 0, signatureMessage.size)

                if (!signer.verifySignature(signature.signature.bytes)) {
                    return ReportErrorCode.BAD_SIGNATURE
                }
            } catch (e: Exception) {
                return ReportErrorCode.BAD_SIGNATURE
            }
        }

        // Return BAD_CORE_INDEX only if no validator was assigned to the reported core
        if (!hasValidCoreAssignment) {
            return ReportErrorCode.BAD_CORE_INDEX
        }

        return null
    }

    private class RandomGenerator(private val seed: JamByteArray) {
        private var counter = 0

        fun nextInt(bound: Int): Int {
            // Ensure positive value
            val hash = blakeHash(seed.bytes + counter.toBigInteger().toByteArray())
            counter++
            val value = hash.fold(0L) { acc, byte ->
                (acc * 256 + (byte.toLong() and 0xFF))
            }
            return Math.floorMod(value, bound.toLong()).toInt()
        }
    }

    // Helper extension function for safe modulo
    private fun Long.safeMod(other: Long): Long {
        return Math.floorMod(this, other)
    }

    // Helper extension function for safe modulo with Int
    private fun Long.safeMod(other: Int): Int {
        return Math.floorMod(this, other.toLong()).toInt()
    }


    /**
     * Performs state transition according to JAM protocol specifications.
     * Implements transition rules from sections 11.4 and 11.5.
     */
    fun transition(
        input: ReportInput,
        preState: ReportState
    ): Pair<ReportState, ReportOutput> {
        val postState = preState.deepCopy()
        val currentSlot = input.slot

        // Check for duplicate packages first
        validateNoDuplicatePackages(input.guarantees, preState.recentBlocks)?.let {
            return Pair(postState, ReportOutput(err = it))
        }

        // Validate anchor and context
        validateAnchor(input.guarantees, preState.recentBlocks, currentSlot)?.let {
            return Pair(postState, ReportOutput(err = it))
        }

        // Track cores with pending reports to prevent duplicates
        val pendingReports = mutableListOf<WorkReport>()
        val validGuarantors = mutableListOf<JamByteArray>()
        val reportPackages = mutableListOf<ReportPackage>()

        // Validate guarantees
        for (guarantee in input.guarantees) {
            // Validate the work report
            validateWorkReport(
                guarantee.report,
                guarantee.slot,
                currentSlot,
                preState.services,
                preState.authPools,
                preState.availAssignments
            )?.let {
                return Pair(postState, ReportOutput(err = it))
            }

            validateGuarantorSignatures(
                guarantee,
                preState.currValidators,
                preState.prevValidators,
                currentSlot,
                preState.entropy
            )?.let {
                return Pair(postState, ReportOutput(err = it))
            }

            // Validate core is not already occupied
            if (pendingReports.any { it.coreIndex == guarantee.report.coreIndex }) {
                return Pair(postState, ReportOutput(err = ReportErrorCode.CORE_ENGAGED))
            }

            pendingReports.add(guarantee.report)

            // Add the primary work package hash and exports root
            reportPackages.add(
                ReportPackage(
                    guarantee.report.packageSpec.hash,
                    guarantee.report.packageSpec.exportsRoot
                )
            )

            // Add any segment root lookups
            guarantee.report.segmentRootLookup.forEach { lookup ->
                reportPackages.add(ReportPackage(lookup.workPackageHash, lookup.segmentTreeRoot))
            }

            // Collect valid guarantor Ed25519 keys
            guarantee.signatures.forEach { signature ->
                val validatorKey = if ((currentSlot / config.ROTATION_PERIOD) ==
                    (guarantee.slot / config.ROTATION_PERIOD)
                ) {
                    preState.currValidators[signature.validatorIndex.toInt()].ed25519
                } else {
                    preState.prevValidators[signature.validatorIndex.toInt()].ed25519
                }
                validGuarantors.add(validatorKey)
            }
        }

        // Update state with new reports
        updateStateWithReports(postState, pendingReports, currentSlot)

        val sortedReportPackages = reportPackages.sortedBy { it.workPackageHash.toHex() }
        val sortedGuarantors = validGuarantors.distinct().sortedBy { it.toHex() }

        return Pair(
            postState,
            ReportOutput(
                ok = ReportOutputMarks(
                    reported = sortedReportPackages,
                    reporters = sortedGuarantors
                )
            )
        )
    }

    /**
     * Updates state with new work reports according to section 11.5
     */
    /**
     * Updates state with new work reports according to section 11.5
     */
    private fun updateStateWithReports(
        state: ReportState,
        reports: List<WorkReport>,
        currentSlot: Long
    ) {
        // Create new assignments list with updated values
        val newAssignments = state.availAssignments.toMutableList()

        // Update assignments for each report
        reports.forEach { report ->
            val index = report.coreIndex.toInt()
            if (index < newAssignments.size) {
                newAssignments[index] = AvailabilityAssignment(
                    report = report,
                    timeout = currentSlot
                )
            }
        }

        // Update mutable state fields directly
        state.availAssignments = newAssignments
    }

    private fun List<Long>.isSorted(): Boolean {
        for (i in 0 until size - 1) {
            if (this[i] > this[i + 1]) return false
        }
        return true
    }

    private fun isValidatorAssignedToCore(
        validator: ValidatorKey,
        coreIndex: Long,
        slot: Long
    ): Boolean {
        // Implementation of validator-core assignment logic
        // Based on equations 133-136
        return true // Placeholder
    }
}
