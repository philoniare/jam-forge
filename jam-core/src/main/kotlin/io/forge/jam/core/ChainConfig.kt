package io.forge.jam.core

/**
 * Chain configuration parameters for different network configurations.
 * For more information refer to https://docs.jamcha.in/basics/chain-spec
 */
data class ChainConfig(
    /** Number of validators in the network */
    val validatorCount: Int,
    /** Number of cores available for parallel execution */
    val coresCount: Int,
    /** Preimage expunge period (D value) */
    val preimageExpungePeriod: Int,
    /** Slot duration in seconds */
    val slotDuration: Int,
    /** Epoch duration in slots */
    val epochLength: Int,
    /** Contest duration in slots */
    val contestDuration: Int,
    /** Number of tickets each validator can submit per epoch */
    val ticketsPerValidator: Int,
    /** Maximum tickets allowed per extrinsic */
    val maxTicketsPerExtrinsic: Int,
    /** Rotation period for validators */
    val rotationPeriod: Int,
    /** Number of erasure coding pieces per segment */
    val numEcPiecesPerSegment: Int,
    /** Maximum gas allowed per block */
    val maxBlockGas: Long,
    /** Maximum gas allowed for refinement */
    val maxRefineGas: Long,
    /** Minimum public service index (S_S in Gray Paper, 2^16 by default) */
    val minPublicServiceIndex: Long = 1L shl 16,
    /** Votes per verdict (2/3 * validators + 1 for supermajority) */
    val votesPerVerdict: Int = (2 * validatorCount / 3) + 1
) {
    companion object {
        /**
         * Tiny configuration for fast testing with 6 validators.
         */
        val TINY = ChainConfig(
            validatorCount = 6,
            coresCount = 2,
            preimageExpungePeriod = 32,
            slotDuration = 6,
            epochLength = 12,
            contestDuration = 10,
            ticketsPerValidator = 3,
            maxTicketsPerExtrinsic = 3,
            rotationPeriod = 4,
            numEcPiecesPerSegment = 1026,
            maxBlockGas = 20_000_000L,
            maxRefineGas = 1_000_000_000L
        )

        /**
         * Full production configuration with 1023 validators.
         * All parameters match the Gray Paper.
         */
        val FULL = ChainConfig(
            validatorCount = 1023,
            coresCount = 341,
            preimageExpungePeriod = 19200,
            slotDuration = 6,
            epochLength = 600,
            contestDuration = 500,
            ticketsPerValidator = 2,
            maxTicketsPerExtrinsic = 16,
            rotationPeriod = 10,
            numEcPiecesPerSegment = 6,
            maxBlockGas = 3_500_000_000L,
            maxRefineGas = 5_000_000_000L
        )
    }
}
