package io.forge.jam.safrole.assurance

data class AssuranceConfig(
    val VALIDATOR_COUNT: Long,
    val CORE_COUNT: Long,
) {
    val superMajority: Int = (2 * VALIDATOR_COUNT.toInt() / 3) + 1
}
