package io.forge.jam.pvm.engine

/**
 * Represents different backend types for the VM
 */
sealed class InstanceBackend {
    data class Interpreted(val instance: InterpretedInstance) : InstanceBackend()
}

inline fun <T> InstanceBackend.access(block: (InterpretedInstance) -> T): T {
    return when (this) {
        is InstanceBackend.Interpreted -> block(instance)
    }
}
