package io.forge.jam.safrole.traces

import io.forge.jam.core.*
import io.forge.jam.safrole.accumulation.AccumulationInput
import io.forge.jam.safrole.assurance.AssuranceInput
import io.forge.jam.safrole.authorization.Auth
import io.forge.jam.safrole.authorization.AuthInput
import io.forge.jam.safrole.historical.HistoricalInput
import io.forge.jam.safrole.preimage.PreimageExtrinsic
import io.forge.jam.safrole.preimage.PreimageInput
import io.forge.jam.safrole.safrole.SafroleInput
import io.forge.jam.safrole.safrole.SafroleState
import io.forge.jam.safrole.stats.StatExtrinsic
import io.forge.jam.safrole.stats.StatInput
import io.forge.jam.vrfs.BandersnatchWrapper

/**
 * Extracts STF inputs from block and state.
 * Each STF has specific input requirements derived from the block extrinsic and current state.
 */
object InputExtractor {

    /**
     * Extract SafroleInput from block and state.
     */
    fun extractSafroleInput(block: Block, state: SafroleState): SafroleInput {
        val header = block.header
        val tickets = block.extrinsic.tickets

        // Extract disputes if present
        val disputes = block.extrinsic.disputes.takeIf {
            it.verdicts.isNotEmpty() || it.culprits.isNotEmpty() || it.faults.isNotEmpty()
        }

        // Extract VRF output from header.entropySource (96-byte signature)
        val entropyBytes = header.entropySource.bytes
        val vrfOutput = if (entropyBytes.size >= 96) {
            try {
                val output = BandersnatchWrapper.getIetfVrfOutput(entropyBytes)
                JamByteArray(output)
            } catch (e: Exception) {
                JamByteArray(entropyBytes.copyOfRange(0, 32))
            }
        } else if (entropyBytes.size >= 32) {
            JamByteArray(entropyBytes.copyOfRange(0, 32))
        } else {
            header.entropySource
        }

        return SafroleInput(
            slot = header.slot,
            entropy = vrfOutput,
            extrinsic = tickets,
            disputes = disputes
        )
    }

    /**
     * Extract SafroleInput from block and FullJamState.
     */
    fun extractSafroleInput(block: Block, state: FullJamState): SafroleInput {
        return extractSafroleInput(block, state.toSafroleState())
    }

    /**
     * Extract AssuranceInput from block and state.
     */
    fun extractAssuranceInput(block: Block, state: FullJamState): AssuranceInput {
        return AssuranceInput(
            assurances = block.extrinsic.assurances,
            slot = block.header.slot,
            parent = block.header.parent
        )
    }

    /**
     * Extract AuthInput from consumed authorizers and block.
     * Called after Reports STF to update authorization pools.
     */
    fun extractAuthInput(consumedAuths: List<Auth>, slot: Long): AuthInput {
        return AuthInput(
            slot = slot,
            auths = consumedAuths
        )
    }

    /**
     * Extract AuthInput directly from block guarantees.
     * Each guarantee consumes an authorization for its core.
     */
    fun extractAuthInputFromBlock(block: Block, state: FullJamState): AuthInput {
        val auths = block.extrinsic.guarantees.map { guarantee ->
            Auth(
                core = guarantee.report.coreIndex.toLong(),
                authHash = guarantee.report.authorizerHash
            )
        }
        return AuthInput(
            slot = block.header.slot,
            auths = auths
        )
    }

    /**
     * Extract PreimageInput from block.
     */
    fun extractPreimageInput(
        block: Block,
        slot: Long,
        rawServiceDataByStateKey: Map<JamByteArray, JamByteArray> = emptyMap()
    ): PreimageInput {
        // Convert Preimage to PreimageExtrinsic
        val preimageExtrinsics = block.extrinsic.preimages.map { preimage ->
            PreimageExtrinsic(
                requester = preimage.requester,
                blob = preimage.blob
            )
        }
        return PreimageInput(
            preimages = preimageExtrinsics,
            slot = slot,
            rawServiceDataByStateKey = rawServiceDataByStateKey
        )
    }

    /**
     * Extract StatInput from block.
     */
    fun extractStatInput(block: Block, state: FullJamState): StatInput {
        val extrinsic = block.extrinsic

        // Create StatExtrinsic from block extrinsic
        val statExtrinsic = StatExtrinsic(
            tickets = extrinsic.tickets,
            preimages = extrinsic.preimages,
            guarantees = extrinsic.guarantees,
            assurances = extrinsic.assurances,
            disputes = extrinsic.disputes
        )

        return StatInput(
            slot = block.header.slot,
            authorIndex = block.header.authorIndex.toLong(),
            extrinsic = statExtrinsic
        )
    }

    /**
     * Extract HistoricalInput from block and accumulation result.
     */
    fun extractHistoryInput(
        block: Block,
        accumulateRoot: JamByteArray
    ): HistoricalInput {
        // Compute header hash
        val headerHash = JamByteArray(blakeHash(block.header.encode()))

        // Extract work packages from guarantees, sorted by hash (as per GP spec)
        val workPackages = block.extrinsic.guarantees.map { guarantee ->
            ReportedWorkPackage(
                hash = guarantee.report.packageSpec.hash,
                exportsRoot = guarantee.report.packageSpec.exportsRoot
            )
        }.sortedBy { it.hash.toHex() }

        return HistoricalInput(
            headerHash = headerHash,
            parentStateRoot = block.header.parentStateRoot,
            accumulateRoot = accumulateRoot,
            workPackages = workPackages
        )
    }

    /**
     * Extract AccumulationInput from available reports.
     * Available reports come from the Assurance STF output.
     */
    fun extractAccumulationInput(
        availableReports: List<WorkReport>,
        slot: Long
    ): AccumulationInput {
        return AccumulationInput(
            slot = slot,
            reports = availableReports
        )
    }

    /**
     * Extract guarantees from block for Reports STF.
     */
    fun extractGuarantees(block: Block): List<GuaranteeExtrinsic> {
        return block.extrinsic.guarantees
    }
}
