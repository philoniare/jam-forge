package io.forge.jam.safrole.report

data class ReportStateConfig(
    val MAX_LOOKUP_ANCHOR_AGE: Long,
    val MAX_DEPENDENCIES: Long,
    val MAX_AUTH_POOL_ITEMS: Int,
    val MAX_ACCUMULATION_GAS: Long,
    val MAX_CORES: Int,
    val MAX_SERVICE_ID: Long = 0xFFFFFFFF,
    val MIN_GUARANTORS: Int,
    val ROTATION_PERIOD: Long,
)
