package io.forge.jam.pvm.engine

enum class SandboxKind {
    Generic;

    companion object {
        fun fromString(s: String): Result<SandboxKind?> = runCatching {
            when (s.lowercase()) {
                "generic" -> Generic
                else -> throw IllegalArgumentException(
                    "Invalid sandbox value; supported values are: 'linux', 'generic'"
                )
            }
        }
    }

    override fun toString(): String = when (this) {
        Generic -> "generic"
    }
}
