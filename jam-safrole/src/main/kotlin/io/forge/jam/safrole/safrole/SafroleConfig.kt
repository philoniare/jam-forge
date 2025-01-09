package io.forge.jam.safrole.safrole

data class SafroleConfig(
    val epochDuration: Long,
    val ticketCutoff: Long,
    val ringSize: Int,
    val validatorCount: Int,
    val maxTicketAttempts: Int,
) {
    // Compute thresholds based on configured validator count
    val superMajority: Int = (2 * validatorCount / 3) + 1
    val oneThird: Int = validatorCount / 3

    // Set of valid vote thresholds
    val validVoteThresholds = setOf(0, oneThird, superMajority)
}
