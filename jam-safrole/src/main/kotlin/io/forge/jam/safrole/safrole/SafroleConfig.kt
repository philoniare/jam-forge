package io.forge.jam.safrole.safrole

data class SafroleConfig(
    val epochLength: Long,
    val ticketCutoff: Long,
    val ringSize: Int,
    val validatorCount: Int
) {
    // Compute thresholds based on configured validator count
    val superMajority: Int = (2 * validatorCount / 3) + 1
    val oneThird: Int = validatorCount / 3

    // Set of valid vote thresholds
    val validVoteThresholds = setOf(0, oneThird, superMajority)
}
