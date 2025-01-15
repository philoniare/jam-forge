package io.forge.jam.safrole.report

import blakeHash
import io.forge.jam.core.*
import io.forge.jam.safrole.AvailabilityAssignment
import io.forge.jam.safrole.ValidatorKey
import io.forge.jam.safrole.historical.HistoricalBeta
import keccakHash
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer

// Extension function to check if a list of validator indices is sorted and contains no duplicates
private fun List<Long>.isSortedAndUnique(): Boolean {
    if (isEmpty()) return true

    var prev = get(0)
    for (i in 1 until size) {
        val curr = get(i)
        if (curr <= prev) {
            return false
        }
        prev = curr
    }
    return true
}

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
        val currentBatchReports = guarantees.associate { guarantee ->
            guarantee.report.packageSpec.hash to guarantee.report.packageSpec.exportsRoot
        }
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
                    // Check in current batch first
                    val existsInCurrentBatch = currentBatchReports.any { (hash, exportsRoot) ->
                        hash.contentEquals(prerequisite.bytes) &&
                            guarantee.report.segmentRootLookup.all { lookup ->
                                !lookup.workPackageHash.contentEquals(prerequisite.bytes) ||
                                    lookup.segmentTreeRoot.contentEquals(exportsRoot)
                            }
                    }

                    // If not in current batch, check recent blocks
                    val existsInHistory = recentBlocks.any { block ->
                        block.reported.any { reported ->
                            reported.hash.contentEquals(prerequisite.bytes) &&
                                guarantee.report.segmentRootLookup.all { lookup ->
                                    !lookup.workPackageHash.contentEquals(prerequisite.bytes) ||
                                        lookup.segmentTreeRoot.contentEquals(reported.exportsRoot)
                                }
                        }
                    }

                    existsInCurrentBatch || existsInHistory
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
        seenHashes: MutableSet<JamByteArray>,
        isLookupHash: Boolean = false
    ): Boolean {
        if (!seenHashes.add(packageHash)) {
            return true
        }

        if (isLookupHash) return false

        // Check if package hash exists in any recent block's reported work packages
        return recentBlocks.any { block ->
            block.reported.any { report ->
                report.hash.contentEquals(packageHash)
            }
        }
    }

    /**
     * Validates that guarantee extrinsics have no duplicate packages.
     */
    private fun validateNoDuplicatePackages(
        guarantees: List<GuaranteeExtrinsic>,
        recentBlocks: List<HistoricalBeta>
    ): ReportErrorCode? {
        val seenHashes = mutableSetOf<String>() // Using String to avoid ByteArray equality issues

        // Check each guarantee's package hash
        for (guarantee in guarantees) {
            val packageHashHex = guarantee.report.packageSpec.hash.toHex()

            // If we've seen this hash before, it's a duplicate
            if (!seenHashes.add(packageHashHex)) {
                return ReportErrorCode.DUPLICATE_PACKAGE
            }

            // Check if package exists in recent history
            if (recentBlocks.any { block ->
                    block.reported.any { report ->
                        report.hash.contentEquals(guarantee.report.packageSpec.hash)
                    }
                }) {
                return ReportErrorCode.DUPLICATE_PACKAGE
            }
        }

        // Validate segment root lookups and prerequisites
        for (guarantee in guarantees) {
            // Check each segment root lookup
            for (lookup in guarantee.report.segmentRootLookup) {
                // Check if the lookup hash exists in current batch
                val lookupHashHex = lookup.workPackageHash.toHex()
                if (seenHashes.contains(lookupHashHex)) {
                    val foundGuarantee = guarantees.find {
                        it.report.packageSpec.hash.toHex() == lookupHashHex
                    }
                    // Verify segment root matches if found in current batch
                    if (foundGuarantee != null &&
                        !lookup.segmentTreeRoot.contentEquals(foundGuarantee.report.packageSpec.exportsRoot)
                    ) {
                        return ReportErrorCode.SEGMENT_ROOT_LOOKUP_INVALID
                    }
                } else {
                    // If not in current batch, check history
                    val foundInHistory = recentBlocks.any { block ->
                        block.reported.any { report ->
                            report.hash.contentEquals(lookup.workPackageHash) &&
                                report.exportsRoot.contentEquals(lookup.segmentTreeRoot)
                        }
                    }
                    if (!foundInHistory) {
                        return ReportErrorCode.SEGMENT_ROOT_LOOKUP_INVALID
                    }
                }
            }

            // Validate prerequisites exist
            for (prerequisite in guarantee.report.context.prerequisites) {
                val prerequisiteHex = prerequisite.toHex()
                val existsInCurrentBatch = seenHashes.contains(prerequisiteHex)
                val existsInHistory = recentBlocks.any { block ->
                    block.reported.any { report ->
                        report.hash.contentEquals(prerequisite)
                    }
                }

                if (!existsInCurrentBatch && !existsInHistory) {
                    return ReportErrorCode.DEPENDENCY_MISSING
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

        val totalOutputSize = workReport.authOutput.bytes.size + workReport.results.sumOf { result ->
            result.result.ok?.bytes?.size ?: 0
        }
        // Maximum size limit WR ≡ 48·2^10 bytes (48 KiB)
        if (totalOutputSize > 48 * 1024) {
            return ReportErrorCode.WORK_REPORT_TOO_BIG
        }

        val totalAccumulateGas = workReport.results.sumOf { it.accumulateGas }
        if (totalAccumulateGas > config.MAX_ACCUMULATION_GAS) {
            return ReportErrorCode.WORK_REPORT_GAS_TOO_HIGH
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
        val segmentLookups = workReport.segmentRootLookup
        val totalDependencies = prerequisites.size + segmentLookups.size
        if (totalDependencies > config.MAX_DEPENDENCIES) {
            return ReportErrorCode.TOO_MANY_DEPENDENCIES
        }

        return null
    }

    private fun calculateCoreAssignments(
        isCurrent: Boolean,
        slot: Long,
        validators: List<ValidatorKey>,
        randomness: JamByteArray
    ): List<Int> {
        // Base assignments grouped in 3s
        val baseAssignments = (0 until validators.size).map { i -> i / 3 }

        // Get shuffled indices
        val shuffledIndices = jamComputeShuffle(validators.size, randomness)

        // Calculate rotation
        val rotationIndex = Math.floorMod(slot, config.ROTATION_PERIOD).toInt()

        // Calculate final assignments with rotation
        return List(validators.size) { validatorIndex ->
            val shuffledPos = shuffledIndices[validatorIndex].toInt()
            val baseCore = baseAssignments[shuffledPos]
            val finalCore = Math.floorMod(baseCore + rotationIndex, config.MAX_CORES)

            // Log assignment calculation
            println(
                """Core assignment calculation for ${if (isCurrent) "current" else "previous"} rotation:
            |Validator: $validatorIndex
            |Shuffled position: $shuffledPos
            |Base core: $baseCore
            |Rotation index: $rotationIndex
            |Final core: $finalCore""".trimMargin()
            )

            finalCore
        }
    }

    private fun verifySignature(
        validatorKey: JamByteArray,
        guarantee: GuaranteeExtrinsic,
        signature: GuaranteeSignature
    ): ReportErrorCode? {
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
        return null
    }

    fun validateGuarantorSignatures(
        guarantee: GuaranteeExtrinsic,
        currValidators: List<ValidatorKey>,
        prevValidators: List<ValidatorKey>,
        currentSlot: Long,
        entropyPool: List<JamByteArray>
    ): ReportErrorCode? {
        val reportRotation = guarantee.slot / config.ROTATION_PERIOD
        val currentRotation = currentSlot / config.ROTATION_PERIOD

        if (reportRotation < currentRotation - 1) {
            return ReportErrorCode.REPORT_EPOCH_BEFORE_LAST
        }

        if (guarantee.signatures.size !in 2..3) {
            return ReportErrorCode.INSUFFICIENT_GUARANTEES
        }

        val validatorIndices = guarantee.signatures.map { it.validatorIndex }

        if (!validatorIndices.isSortedAndUnique()) {
            return ReportErrorCode.NOT_SORTED_OR_UNIQUE_GUARANTORS
        }

        val reportedCore = guarantee.report.coreIndex.toInt()

        // Calculate if we're using current or previous rotation
        val isCurrent = (guarantee.slot / config.ROTATION_PERIOD) == (currentSlot / config.ROTATION_PERIOD)

        // Get the appropriate validator set and entropy
        val (validators, entropy) = if (isCurrent) {
            Pair(currValidators, entropyPool[3])  // Current rotation
        } else {
            Pair(prevValidators, entropyPool[2])  // Previous rotation
        }

        // Calculate assignments for both current and previous rotations
        val currentAssignments = mutableMapOf<Int, MutableList<Int>>()
        val previousAssignments = mutableMapOf<Int, MutableList<Int>>()

        // Calculate and log current assignments with details
        calculateCoreAssignments(true, guarantee.slot, currValidators, entropyPool[3])
            .forEachIndexed { validatorIndex, coreIndex ->
                println("Current Assignment - Validator $validatorIndex -> Core $coreIndex")
                currentAssignments.getOrPut(coreIndex) { mutableListOf() }.add(validatorIndex)
            }

        // Calculate and log previous assignments with details
        calculateCoreAssignments(false, guarantee.slot, prevValidators, entropyPool[2])
            .forEachIndexed { validatorIndex, coreIndex ->
                println("Previous Assignment - Validator $validatorIndex -> Core $coreIndex")
                previousAssignments.getOrPut(coreIndex) { mutableListOf() }.add(validatorIndex)
            }

        // Log complete assignment maps
        println("Complete Current Assignments Map: $currentAssignments")
        println("Complete Previous Assignments Map: $previousAssignments")
        // Track signature counts per core
        val signatureCountsByCore = mutableMapOf<Int, Int>()

        // Verify signatures and track core assignments
        for (signature in guarantee.signatures) {
            val validatorIndex = signature.validatorIndex.toInt()
            if (validatorIndex < 0 || validatorIndex >= validators.size) {
                return ReportErrorCode.BAD_VALIDATOR_INDEX
            }

            // Check both current and previous assignments for this validator
            val isAssignedInCurrent = currentAssignments[reportedCore]?.contains(validatorIndex) == true
            val isAssignedInPrevious = previousAssignments[reportedCore]?.contains(validatorIndex) == true
            println("Assignment Check for validator $validatorIndex:")
            println("  Current Assignment Check - Core $reportedCore contains validator? $isAssignedInCurrent")
            println("  Previous Assignment Check - Core $reportedCore contains validator? $isAssignedInPrevious")

            val validatorKey = if (isCurrent) {
                currValidators[validatorIndex].ed25519
            } else {
                prevValidators[validatorIndex].ed25519
            }

            if (isAssignedInCurrent || isAssignedInPrevious) {
                verifySignature(validatorKey, guarantee, signature)?.let {
                    return it
                }
                // Increment signature count for this core
                signatureCountsByCore[reportedCore] = signatureCountsByCore.getOrDefault(reportedCore, 0) + 1
            }
        }

        // Check if we have at least 2 valid signatures for the reported core
        val validSignaturesForCore = signatureCountsByCore.getOrDefault(reportedCore, 0)
        println("Core $reportedCore validation complete: $validSignaturesForCore valid signatures (${if (isCurrent) "current" else "previous"} rotation)")
        if (validSignaturesForCore < 2) {
            return ReportErrorCode.WRONG_ASSIGNMENT
        }

        return null
    }

    // Helper extension function for safe modulo
    private fun Long.safeMod(other: Long): Long {
        return Math.floorMod(this, other)
    }

    // Helper extension function for safe modulo with Int
    private fun Long.safeMod(other: Int): Int {
        return Math.floorMod(this, other.toLong()).toInt()
    }

    private fun validateGuaranteesOrder(guarantees: List<GuaranteeExtrinsic>): ReportErrorCode? {
        for (i in 0 until guarantees.size - 1) {
            val currentCore = guarantees[i].report.coreIndex
            val nextCore = guarantees[i + 1].report.coreIndex
            if (currentCore >= nextCore) {
                return ReportErrorCode.OUT_OF_ORDER_GUARANTEE
            }
        }
        return null
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

        validateGuaranteesOrder(input.guarantees)?.let {
            return Pair(postState, ReportOutput(err = it))
        }

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
