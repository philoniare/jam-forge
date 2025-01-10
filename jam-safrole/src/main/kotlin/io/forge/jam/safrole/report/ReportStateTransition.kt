package io.forge.jam.safrole.report

import io.forge.jam.core.GuaranteeExtrinsic
import io.forge.jam.core.JamByteArray
import io.forge.jam.core.WorkReport
import io.forge.jam.safrole.AvailabilityAssignment
import io.forge.jam.safrole.ValidatorKey
import io.forge.jam.safrole.historical.HistoricalBeta


class ReportStateTransition(private val config: ReportStateConfig) {

    /**
     * Validates the Beefy root against the MMR peaks.
     * This would need to implement the actual MMR validation logic.
     */
    private fun validateBeefyRootAgainstMmrPeaks(
        beefyRoot: JamByteArray,
        mmrPeaks: List<JamByteArray?>
    ): Boolean {
        // TODO: Implement proper MMR peak validation logic
        return mmrPeaks.any { peak ->
            peak?.contentEquals(beefyRoot) == true
        }
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
            val lookupAnchorBlock = recentBlocks.find { it.hash.contentEquals(context.lookupAnchor) }
            if (lookupAnchorBlock == null) {
                return ReportErrorCode.ANCHOR_NOT_RECENT
            }

            val anchorBlock = recentBlocks.find { it.hash.contentEquals(context.anchor) }
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
     * Validates work report according to JAM protocol specifications.
     * Implements validation rules from sections 11.4 and 14.3 of the protocol.
     */
    fun validateWorkReport(
        workReport: WorkReport,
        services: List<ServiceItem>,
        authPools: List<List<JamByteArray>>,
        currentSlot: Long,
        pendingReports: List<WorkReport>
    ): ReportErrorCode? {
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
            println("ServiceId: ${result.serviceId}")
            // Validate service exists
            val service = services.find { it.id == result.serviceId } ?: continue


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

    /**
     * Performs state transition according to JAM protocol specifications.
     * Implements transition rules from sections 11.4 and 11.5.
     */
    fun transition(
        input: ReportInput,
        preState: ReportState
    ): Pair<ReportState, ReportOutput> {
        val postState = preState.deepCopy()
        val currentSlot = input.slot ?: return Pair(
            postState,
            ReportOutput(err = ReportErrorCode.FUTURE_REPORT_SLOT)
        )

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
                preState.services,
                preState.authPools,
                currentSlot,
                pendingReports
            )?.let {
                return Pair(postState, ReportOutput(err = it))
            }

            // Validate core is not already occupied
            if (pendingReports.any { it.coreIndex == guarantee.report.coreIndex }) {
                return Pair(postState, ReportOutput(err = ReportErrorCode.CORE_ENGAGED))
            }

            // Validate guarantor assignments
//            validateGuarantorAssignments(
//                guarantee,
//                preState.currValidators,
//                preState.prevValidators,
//                currentSlot
//            )?.let {
//                return Pair(postState, ReportOutput(err = it))
//            }

            pendingReports.add(guarantee.report)

            // Create report package from segment root lookups
            val workPackageHashes = mutableListOf<JamByteArray>()
            val segmentTreeRoots = mutableListOf<JamByteArray>()

            // Add the primary work package hash and exports root
            workPackageHashes.add(guarantee.report.packageSpec.hash)
            segmentTreeRoots.add(guarantee.report.packageSpec.exportsRoot)

            // Add any segment root lookups
            guarantee.report.segmentRootLookup.forEach { lookup ->
                workPackageHashes.add(lookup.workPackageHash)
                segmentTreeRoots.add(lookup.segmentTreeRoot)
            }

            reportPackages.add(
                ReportPackage(
                    workPackageHash = workPackageHashes,
                    segment_tree_root = segmentTreeRoots
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

        return Pair(
            postState,
            ReportOutput(
                ok = ReportOutputMarks(
                    reported = reportPackages,
                    reporters = validGuarantors
                )
            )
        )
    }

    /**
     * Updates state with new work reports according to section 11.5
     */
    private fun updateStateWithReports(
        state: ReportState,
        reports: List<WorkReport>,
        currentSlot: Long
    ) {
        // Create new list with updated assignments
        val newAssignments = state.availAssignments.toMutableList()

        reports.forEach { report ->
            val index = report.coreIndex.toInt()
            if (index < newAssignments.size) {
                newAssignments[index] = AvailabilityAssignment(
                    report = report,
                    timeout = currentSlot
                )
            }
        }

        // Create new state with updated assignments
        val updatedState = state.copy(
            availAssignments = newAssignments
        )

        // Update all fields from updatedState back to state
        state.apply {
            ReportState::class.java.declaredFields.forEach { field ->
                field.isAccessible = true
                field.set(this, field.get(updatedState))
            }
        }
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
