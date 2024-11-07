package io.forge.jam.pvm.engine

import io.forge.jam.pvm.program.ProgramParseError

sealed class ErrorKind {
    data class Owned(val message: String) : ErrorKind()
    data class Static(val message: String) : ErrorKind()
    data class ProgramParseError(val error: ProgramParseError) : ErrorKind()

    override fun toString(): String = when (this) {
        is Owned -> message
        is Static -> message
        is ProgramParseError -> error.toString()
    }
}

class PvmError private constructor(private val kind: ErrorKind) : Exception() {
    companion object {
        fun fromDisplay(message: Any): PvmError {
            return PvmError(ErrorKind.Owned(message.toString()))
        }

        fun fromStaticStr(message: String): PvmError {
            return PvmError(ErrorKind.Static(message))
        }
    }

    override fun toString(): String = kind.toString()
    override val message: String
        get() = toString()
}

fun String.toError(): PvmError = PvmError.fromDisplay(this)

inline fun bail(message: () -> String): Nothing {
    throw PvmError.fromDisplay(message())
}

inline fun bailStatic(message: String): Nothing {
    throw PvmError.fromStaticStr(message)
}
