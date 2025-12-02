package io.forge.jam.safrole.accumulation

data class AccumulationConfig(
    val EPOCH_LENGTH: Int,
    val MAX_BLOCK_HISTORY: Int,
    val AUTH_QUEUE_SIZE: Int,
    val PREIMAGE_PURGE_PERIOD: Long = 32L
)
