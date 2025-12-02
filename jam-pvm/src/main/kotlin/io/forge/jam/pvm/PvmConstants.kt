package io.forge.jam.pvm

/**
 * Gray Paper PVM constants.
 */
object PvmConstants {
    /**
     * ZP = 2^12: The PVM memory page size (4 KB).
     */
    const val ZP: UInt = 4096u  // 1 shl 12

    /**
     * ZZ = 2^16: The standard PVM program initialization zone size (64 KB).
     */
    const val ZZ: UInt = 65536u  // 1 shl 16

    /**
     * ZI = 2^24: The standard PVM program initialization input data size (16 MB).
     */
    const val ZI: UInt = 16777216u  // 1 shl 24

    /**
     * ZA = 2: The PVM dynamic address alignment factor.
     */
    const val ZA: Int = 2

    /**
     * The minimum valid address. Addresses below this trigger panic.
     */
    const val MIN_VALID_ADDRESS: UInt = 0x10000u  // 2^16

    /**
     * Standard register 0 initial value.
     */
    val REGISTER_0_VALUE: UInt = (0x100000000L - 0x10000L).toUInt()  // 0xFFFF0000

    /**
     * Standard stack base address.
     */
    val STACK_BASE_ADDRESS: UInt = (0x100000000L - 2L * ZZ.toLong() - ZI.toLong()).toUInt()

    /**
     * Standard input/argument start address.
     */
    val INPUT_START_ADDRESS: UInt = STACK_BASE_ADDRESS + ZZ

    const val GP_STACK_BASE: UInt = 0xFEFE0000u

    const val GP_STACK_SIZE: UInt = 0x10000u

    val GP_STACK_LOW: UInt = GP_STACK_BASE - GP_STACK_SIZE
}
