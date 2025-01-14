package io.forge.jam.safrole.report

import blakeHash
import io.forge.jam.core.GuaranteeExtrinsic
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.WorkReport
import io.forge.jam.core.jamComputeShuffle
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
        println("Calculating assignments for ${validators.size} validators")

        // Base assignments grouped in 3s according to equation 133
        val baseAssignments = (0 until validators.size).map { i ->
            i / 3
        }
        println("Initial base assignments: ${baseAssignments.take(10)}")

        // Get shuffled indices according to equation 134
        val shuffledIndices = jamComputeShuffle(validators.size, randomness)
        println("Shuffled indices: ${shuffledIndices.take(10)}")

        // Calculate rotation according to equation 135
        val rotationIndex = (slot / config.ROTATION_PERIOD).toInt()
        println("Rotation index: $rotationIndex")

        // According to equation 136, for each validator:
        // 1. Use their position after shuffling to get base core assignment
        // 2. Add rotation and wrap around MAX_CORES
        return List(validators.size) { validatorIndex ->
            val shuffledPos = shuffledIndices[validatorIndex].toInt()
            val baseCore = baseAssignments[shuffledPos]
            val finalCore = Math.floorMod(baseCore + rotationIndex, config.MAX_CORES)
            println("Validator $validatorIndex: shuffled to $shuffledPos, base core $baseCore, final core $finalCore")
            finalCore
        }
    }

    fun validateGuarantorSignatures(
        guarantee: GuaranteeExtrinsic,
        currValidators: List<ValidatorKey>,
        prevValidators: List<ValidatorKey>,
        currentSlot: Long,
        entropyPool: List<JamByteArray>
    ): ReportErrorCode? {
        println("Validating guarantor signatures")
        println("Report core: ${guarantee.report.coreIndex}")
        println("Guarantee slot: ${guarantee.slot}")
        println("Current slot: $currentSlot")

        val reportRotation = guarantee.slot / config.ROTATION_PERIOD
        val currentRotation = currentSlot / config.ROTATION_PERIOD

        println("Report Rotation: $reportRotation")
        println("Current Rotation: $currentRotation")

        if (reportRotation < currentRotation - 1) {
            println("ERROR: Report from rotation before last")
            return ReportErrorCode.REPORT_EPOCH_BEFORE_LAST
        }

        if (guarantee.signatures.size !in 2..3) {
            println("ERROR: Invalid signature count: ${guarantee.signatures.size}")
            return ReportErrorCode.INSUFFICIENT_GUARANTEES
        }

        val validatorIndices = guarantee.signatures.map { it.validatorIndex }
        println("Validator Indices: $validatorIndices")

        if (!validatorIndices.isSortedAndUnique()) {
            println("ERROR: Validator indices not sorted or unique")
            return ReportErrorCode.NOT_SORTED_OR_UNIQUE_GUARANTORS
        }

        val reportedCore = guarantee.report.coreIndex.toInt()
        println("Reported Core: $reportedCore")

        // Calculate and log assignments
        val isCurrent = (guarantee.slot / config.ROTATION_PERIOD) == (currentSlot / config.ROTATION_PERIOD)
        println("Using Current Rotation: $isCurrent")

        val assignmentsCurrent = calculateCoreAssignments(
            true,
            guarantee.slot,
            currValidators,
            entropyPool[3]
        )

        val assignmentsPrevious = calculateCoreAssignments(
            false,
            guarantee.slot,
            prevValidators,
            entropyPool[2]
        )

        val assignments = calculateCoreAssignments(
            isCurrent,
            guarantee.slot,
            if (isCurrent) currValidators else prevValidators,
            if (isCurrent) entropyPool[3] else entropyPool[2]
        )
        val validators = if (isCurrent) currValidators else prevValidators

        val coreToValidators = assignments.withIndex()
            .groupBy({ it.value }, { it.index })
        println("Core assignments: ${coreToValidators[reportedCore]}")

        println("Assignments for signatures:")
        guarantee.signatures.forEach { sig ->
            val validatorIndex = sig.validatorIndex.toInt()
            val assignedCore = assignments.getOrNull(validatorIndex)
            println("Validator $validatorIndex assigned to core: $assignedCore")
        }

        println("Checking assignments for core $reportedCore")
        println(
            "Base assignments before rotation: ${
                (0 until validators.size).map { i ->
                    (config.MAX_CORES.toDouble() * i / validators.size).toInt()
                }
            }")

        var matchCount = 0
        // Verify signatures and core assignments
        for (signature in guarantee.signatures) {
            val validatorIndex = signature.validatorIndex.toInt()
            if (validatorIndex < 0 || validatorIndex >= validators.size) {
                println("ERROR: Invalid validator index: $validatorIndex")
                return ReportErrorCode.BAD_VALIDATOR_INDEX
            }

            val assignedCoreInCurrent = if (validatorIndex < currValidators.size)
                assignmentsCurrent[validatorIndex] else -1
            val assignedCoreInPrevious = if (validatorIndex < prevValidators.size)
                assignmentsPrevious[validatorIndex] else -1

            if (assignedCoreInCurrent == reportedCore || assignedCoreInPrevious == reportedCore) {
                matchCount++
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
                    println("ERROR: Invalid signature for validator $validatorIndex")
                    return ReportErrorCode.BAD_SIGNATURE
                }
            } catch (e: Exception) {
                println("ERROR: Signature verification failed with exception: ${e.message}")
                return ReportErrorCode.BAD_SIGNATURE
            }
        }

        if (matchCount < 2) {
            return ReportErrorCode.WRONG_ASSIGNMENT
        }

        println("=== Signature Validation Successful ===")
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
