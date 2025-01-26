package io.forge.jam.pvm

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
enum class PvmStatus {
    @SerialName("panic")
    PANIC,

    @SerialName("halt")
    HALT;

    companion object {
        fun fromString(value: String): PvmStatus = when (value.lowercase()) {
            "panic" -> PANIC
            "halt" -> HALT
            else -> throw IllegalArgumentException("Invalid status: $value. Must be either 'panic' or 'halt'")
        }
    }
}
