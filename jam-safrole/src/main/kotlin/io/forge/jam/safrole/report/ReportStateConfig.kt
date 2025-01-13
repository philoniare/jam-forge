package io.forge.jam.safrole.report

data class ReportStateConfig(
    val MAX_LOOKUP_ANCHOR_AGE: Long,
    val MAX_DEPENDENCIES: Long,
    val MAX_AUTH_POOL_ITEMS: Int,
    val MAX_ACCUMULATION_GAS: Long = 3_500_000_000L,
    val MAX_CORES: Int,
    val MAX_SERVICE_ID: Long = 0xFFFFFFFF,
    val MIN_GUARANTORS: Int,
    val ROTATION_PERIOD: Long,
    val MAX_VALIDATORS: Int,
    val EPOCH_LENGTH: Int,
)
