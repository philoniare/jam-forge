package io.forge.jam.pvm

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
enum class PvmStatus {
    @SerialName("trap")
    TRAP,

    @SerialName("halt")
    HALT;

    companion object {
        fun fromString(value: String): PvmStatus = when (value.lowercase()) {
            "trap" -> TRAP
            "halt" -> HALT
            else -> throw IllegalArgumentException("Invalid status: $value. Must be either 'trap' or 'halt'")
        }
    }
}
