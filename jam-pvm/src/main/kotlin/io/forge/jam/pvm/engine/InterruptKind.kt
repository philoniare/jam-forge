package io.forge.jam.pvm.engine

/**
 * Represents different kinds of interrupts that can occur during execution.
 */
sealed class InterruptKind {
    /**
     * The execution finished normally.
     *
     * This happens when the program jumps to the address `0xffff0000`.
     */
    object Finished : InterruptKind()

    /**
     * The execution finished abnormally with a trap.
     *
     * This can happen for a few reasons:
     *   - if the `trap` instruction is executed,
     *   - if an invalid instruction is executed,
     *   - if a jump to an invalid address is made,
     *   - if a segmentation fault is triggered (when dynamic paging is not enabled for this VM)
     */
    object Trap : InterruptKind()

    /**
     * The execution triggered an external call with an `ecalli` instruction.
     */
    data class Ecalli(val value: UInt) : InterruptKind()

    /**
     * The execution triggered a segmentation fault.
     *
     * This happens when a program accesses a memory page that is not mapped,
     * or tries to write to a read-only page.
     *
     * Requires dynamic paging to be enabled with [ModuleConfig.setDynamicPaging],
     * otherwise is never emitted.
     */
    data class Segfault(val fault: Segfault) : InterruptKind()

    /**
     * The execution ran out of gas.
     *
     * Requires gas metering to be enabled with [ModuleConfig.setGasMetering],
     * otherwise is never emitted.
     */
    object NotEnoughGas : InterruptKind()

    /**
     * Executed a single instruction.
     *
     * Requires execution step-tracing to be enabled with [ModuleConfig.setStepTracing],
     * otherwise is never emitted.
     */
    object Step : InterruptKind()

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is InterruptKind) return false
        return when (this) {
            is Finished -> other is Finished
            is Trap -> other is Trap
            is Ecalli -> other is Ecalli && value == other.value
            is Segfault -> other is Segfault && fault == other.fault
            is NotEnoughGas -> other is NotEnoughGas
            is Step -> other is Step
        }
    }

    override fun hashCode(): Int {
        return when (this) {
            is Finished -> 1
            is Trap -> 2
            is Ecalli -> 31 * 3 + value.hashCode()
            is Segfault -> 31 * 4 + fault.hashCode()
            is NotEnoughGas -> 5
            is Step -> 6
        }
    }
}
