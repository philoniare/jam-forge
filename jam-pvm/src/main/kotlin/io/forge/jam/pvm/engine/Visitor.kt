package io.forge.jam.pvm.engine

import io.forge.jam.core.toHex
import io.forge.jam.pvm.Abi
import io.forge.jam.pvm.PvmLogger
import io.forge.jam.pvm.program.ProgramCounter
import io.forge.jam.pvm.program.Reg

typealias Target = UInt

fun panicImpl(visitor: Visitor, programCounter: ProgramCounter): Target? {
    with(visitor.inner) {
        this.programCounter = programCounter
        this.programCounterValid = true
        this.nextProgramCounter = null
        this.nextProgramCounterChanged = true
        this.interrupt = InterruptKind.Panic
    }
    return null
}

interface LoadTy {
    fun fromSlice(bytes: ByteArray): ULong
}

interface StoreTy {
    fun intoBytes(value: ULong): ByteArray
}

object U8StoreTy : StoreTy {
    override fun intoBytes(value: ULong): ByteArray {
        return byteArrayOf(value.toUByte().toByte())
    }
}

object U16StoreTy : StoreTy {
    override fun intoBytes(value: ULong): ByteArray {
        return ByteArray(2).apply {
            this[0] = (value and 0xFFu).toByte()
            this[1] = ((value shr 8) and 0xFFu).toByte()
        }
    }
}

object U32StoreTy : StoreTy {
    override fun intoBytes(value: ULong): ByteArray {
        return ByteArray(4).apply {
            this[0] = (value and 0xFFu).toByte()
            this[1] = ((value shr 8) and 0xFFu).toByte()
            this[2] = ((value shr 16) and 0xFFu).toByte()
            this[3] = ((value shr 24) and 0xFFu).toByte()
        }
    }
}

object U64StoreTy : StoreTy {
    override fun intoBytes(value: ULong): ByteArray {
        return ByteArray(8).apply {
            this[0] = (value and 0xFFu).toByte()
            this[1] = ((value shr 8) and 0xFFu).toByte()
            this[2] = ((value shr 16) and 0xFFu).toByte()
            this[3] = ((value shr 24) and 0xFFu).toByte()
            this[4] = ((value shr 32) and 0xFFu).toByte()
            this[5] = ((value shr 40) and 0xFFu).toByte()
            this[6] = ((value shr 48) and 0xFFu).toByte()
            this[7] = ((value shr 56) and 0xFFu).toByte()
        }
    }
}

object U8LoadTy : LoadTy {
    override fun fromSlice(bytes: ByteArray): ULong {
        return bytes[0].toUByte().toULong()
    }
}

object I8LoadTy : LoadTy {
    override fun fromSlice(bytes: ByteArray): ULong {
        // bytes[0] -> UByte -> Byte (signed) -> Long (sign extended) -> ULong
        return Cast(Cast(bytes[0].toByte()).byteToI64SignExtend()).longToUnsigned()
    }
}

object U16LoadTy : LoadTy {
    override fun fromSlice(bytes: ByteArray): ULong {
        return bytes.toUShort(0).toULong()
    }
}

object I16LoadTy : LoadTy {
    override fun fromSlice(bytes: ByteArray): ULong {
        val byte0 = bytes[0].toInt() and 0xFF
        val byte1 = bytes[1].toInt() and 0xFF
        val value = byte0 or (byte1 shl 8)
        val shortValue = (value and 0xFFFF).toShort()
        val signExtended = Cast(shortValue).shortToI64SignExtend()
        val result = Cast(signExtended).longToUnsigned()
        return result
    }
}

object U32LoadTy : LoadTy {
    override fun fromSlice(bytes: ByteArray): ULong {
        return bytes.toUInt(0).toULong()
    }
}

object I32LoadTy : LoadTy {
    override fun fromSlice(bytes: ByteArray): ULong {
        val value = bytes.toInt(0)
        return Cast(Cast(value).intToI64SignExtend()).longToUnsigned()
    }
}

object U64LoadTy : LoadTy {
    override fun fromSlice(bytes: ByteArray): ULong {
        return bytes.toULong(0)
    }
}

// Extension functions for ByteArray to convert to primitive types in little-endian order
private fun ByteArray.toShort(offset: Int = 0): Short =
    ((this[offset + 1].toInt() shl 8) or
        (this[offset].toInt() and 0xFF)).toShort()

private fun ByteArray.toUShort(offset: Int = 0): UShort =
    ((this[offset + 1].toInt() shl 8) or
        (this[offset].toInt() and 0xFF)).toUShort()

private fun ByteArray.toInt(offset: Int = 0): Int =
    ((this[offset + 3].toInt() and 0xFF) shl 24) or
        ((this[offset + 2].toInt() and 0xFF) shl 16) or
        ((this[offset + 1].toInt() and 0xFF) shl 8) or
        (this[offset].toInt() and 0xFF)

private fun ByteArray.toUInt(offset: Int = 0): UInt =
    (((this[offset + 3].toInt() and 0xFF) shl 24) or
        ((this[offset + 2].toInt() and 0xFF) shl 16) or
        ((this[offset + 1].toInt() and 0xFF) shl 8) or
        (this[offset].toInt() and 0xFF)).toUInt()

private fun ByteArray.toULong(offset: Int = 0): ULong =
    ((this[offset + 7].toULong() and 0xFFu) shl 56) or
        ((this[offset + 6].toULong() and 0xFFu) shl 48) or
        ((this[offset + 5].toULong() and 0xFFu) shl 40) or
        ((this[offset + 4].toULong() and 0xFFu) shl 32) or
        ((this[offset + 3].toULong() and 0xFFu) shl 24) or
        ((this[offset + 2].toULong() and 0xFFu) shl 16) or
        ((this[offset + 1].toULong() and 0xFFu) shl 8) or
        (this[offset].toULong() and 0xFFu)

/**
 * Visitor implementation for interpreting instructions
 */
class Visitor(
    val inner: InterpretedInstance
) {
    companion object {
        val logger: PvmLogger = PvmLogger(Visitor::class.java)
    }


    /**
     * Gets a 32-bit value from either a register or immediate
     */
    @Suppress("NOTHING_TO_INLINE")
    inline fun get32(regimm: RegImm): UInt = when (regimm) {
        is RegImm.RegValue -> Cast(inner.regs[regimm.reg.toIndex()]).ulongTruncateToU32()
        is RegImm.ImmValue -> regimm.value
    }

    /**
     * Gets a 64-bit value from either a register or immediate
     */
    @Suppress("NOTHING_TO_INLINE")
    inline fun get64(regimm: RegImm): ULong = when (regimm) {
        is RegImm.RegValue -> inner.regs[regimm.reg.toIndex()]
        is RegImm.ImmValue -> {
            Cast(Cast(Cast(regimm.value).uintToSigned()).intToI64SignExtend()).longToUnsigned()
        }
    }

    /**
     * Moves to the next instruction
     */
    @Suppress("NOTHING_TO_INLINE")
    inline fun goToNextInstruction(): Target = inner.compiledOffset + 1u

    /**
     * Sets a 32-bit value in a register.
     */
    @Suppress("NOTHING_TO_INLINE")
    inline fun set32(dst: Reg, value: UInt) {
        // The chain of casts: u32 -> i32 -> i64 -> u64
        val finalValue = Cast(value)
            .uintToSigned()
            .let { Cast(it) }
            .intToI64SignExtend()
            .let { Cast(it) }
            .longToUnsigned()

        inner.regs[dst.toIndex()] = finalValue
    }

    /**
     * Sets a 64-bit value in a register
     */
    @Suppress("NOTHING_TO_INLINE")
    inline fun set64(dst: Reg, value: ULong) {
        inner.regs[dst.toIndex()] = value
    }

    /**
     * Performs a three-operand 32-bit operation
     */
    inline fun set3_32(
        dst: Reg,
        s1: RegImm,
        s2: RegImm,
        crossinline callback: (UInt, UInt) -> UInt
    ): Target {
        val v1 = get32(s1)
        val v2 = get32(s2)
        set32(dst, callback(v1, v2))
        return goToNextInstruction()
    }

    /**
     * Performs a three-operand 64-bit operation
     */
    inline fun set3_64(
        dst: Reg,
        s1: RegImm,
        s2: RegImm,
        crossinline callback: (ULong, ULong) -> ULong
    ): Target {
        val v1 = get64(s1)
        val v2 = get64(s2)
        set64(dst, callback(v1, v2))
        return goToNextInstruction()
    }

    /**
     * Performs a conditional branch
     */
    inline fun branch(
        s1: RegImm,
        s2: RegImm,
        targetTrue: Target,
        targetFalse: Target,
        condition: (ULong, ULong) -> Boolean
    ): Target {
        val v1 = get64(s1)
        val v2 = get64(s2)
        return if (condition(v1, v2)) {
            targetTrue
        } else {
            targetFalse
        }
    }

    /**
     * Handles segmentation faults
     */
    fun segfaultImpl(programCounter: ProgramCounter, pageAddress: UInt): Target? {
        inner.apply {
            this.programCounter = programCounter
            this.programCounterValid = true
            this.nextProgramCounter = programCounter
            this.interrupt = InterruptKind.Segfault(
                SegfaultInfo(
                    pageAddress = pageAddress,
                    pageSize = module.memoryMap().pageSize
                )
            )
        }
        return null
    }

    inline fun <reified T : LoadTy> load(
        programCounter: ProgramCounter,
        dst: Reg,
        base: Reg?,
        offset: UInt,
        length: UInt,
        isDynamic: Boolean
    ): Target? {
        val address = (base?.let { Cast(inner.regs[base.toIndex()]).ulongTruncateToU32() } ?: 0u).plus(offset)
        val pageAddressLo = inner.module.roundToPageSizeDown(address)

        if (!isDynamic) {
            // Basic memory mode
            try {
                val slice = inner.basicMemory.getMemorySlice(inner.module, address, length) ?: run {
                    logger.debug("Load of $length bytes from 0x${address.toString(16)} failed! (pc = $programCounter, cycle = ${inner.cycleCounter})")
                    return panicImpl(this, programCounter)
                }
                val loadTy = when (T::class) {
                    U8LoadTy::class -> U8LoadTy
                    I8LoadTy::class -> I8LoadTy
                    U16LoadTy::class -> U16LoadTy
                    I16LoadTy::class -> I16LoadTy
                    U32LoadTy::class -> U32LoadTy
                    I32LoadTy::class -> I32LoadTy
                    U64LoadTy::class -> U64LoadTy
                    else -> throw IllegalArgumentException("Unknown LoadTy type")
                }
                logger.debug("Slice: ${slice.toHex()}")
                val value = loadTy.fromSlice(slice)
                logger.debug("Memory  $dst = $loadTy [0x${address}] = 0x${value}")
                set64(dst, value)
                return goToNextInstruction()
            } catch (e: ArrayIndexOutOfBoundsException) {
                return segfaultImpl(programCounter, pageAddressLo)
            }
        } else {
            // Dynamic memory mode
            val addressEnd = address.plus(length)
            if (addressEnd < address) {
                return panicImpl(this, programCounter)
            }

            val pageAddressHi = inner.module.roundToPageSizeDown(addressEnd - 1u)
            if (pageAddressLo == pageAddressHi) {
                // Single page access
                val page =
                    inner.dynamicMemory.pages[pageAddressLo]
                        ?: return segfaultImpl(programCounter, pageAddressLo)

                try {
                    val offset = Cast(address).uintToUSize() - Cast(pageAddressLo).uintToUSize()
                    val slice = page.sliceArray(offset.toInt() until offset.toInt() + length.toInt())

                    val loadTy = when (T::class) {
                        U8LoadTy::class -> U8LoadTy
                        I8LoadTy::class -> I8LoadTy
                        U16LoadTy::class -> U16LoadTy
                        I16LoadTy::class -> I16LoadTy
                        U32LoadTy::class -> U32LoadTy
                        I32LoadTy::class -> I32LoadTy
                        U64LoadTy::class -> U64LoadTy
                        else -> throw IllegalArgumentException("Unknown LoadTy type")
                    }

                    val value = loadTy.fromSlice(slice)
                    set64(dst, value)
                    return goToNextInstruction()

                } catch (e: IndexOutOfBoundsException) {
                    return panicImpl(this, programCounter)
                }
            } else {
                // Cross-page access
                val pages = inner.dynamicMemory.pages
                val lo = pages[pageAddressLo]
                val hi = pages[pageAddressHi]

                when {
                    lo != null && hi != null -> {
                        try {
                            val pageSize = inner.module.memoryMap().pageSize.toInt()
                            val loLen = (pageAddressHi - address).toInt()
                            val hiLen = length.toInt() - loLen
                            val buffer = ByteArray(length.toInt())

                            System.arraycopy(lo, pageSize - loLen, buffer, 0, loLen)
                            System.arraycopy(hi, 0, buffer, loLen, hiLen)

                            val loadTy = when (T::class) {
                                U8LoadTy::class -> U8LoadTy
                                I8LoadTy::class -> I8LoadTy
                                U16LoadTy::class -> U16LoadTy
                                I16LoadTy::class -> I16LoadTy
                                U32LoadTy::class -> U32LoadTy
                                I32LoadTy::class -> I32LoadTy
                                U64LoadTy::class -> U64LoadTy
                                else -> throw IllegalArgumentException("Unknown LoadTy type")
                            }
                            val value = loadTy.fromSlice(buffer)
                            set64(dst, value)
                            return goToNextInstruction()
                        } catch (e: ArrayIndexOutOfBoundsException) {
                            return segfaultImpl(programCounter, pageAddressLo)
                        }
                    }

                    lo == null -> return segfaultImpl(programCounter, pageAddressLo)
                    else -> return segfaultImpl(programCounter, pageAddressHi)
                }
            }
        }

    }

    inline fun <reified T : StoreTy> store(
        programCounter: ProgramCounter,
        src: RegImm,
        base: Reg?,
        offset: UInt,
        isDynamic: Boolean
    ): Target? {
        // Get the source value
        val value = when (src) {
            is RegImm.RegValue -> {
                // Get value from register
                val regValue = inner.regs[src.reg.toIndex()]
                logger.debug("Memory [0x${offset.toString(16)}] = ${src.reg} = 0x${regValue.toString(16)}")
                regValue
            }

            is RegImm.ImmValue -> {
                val immValue = Cast(Cast(src.value).uintToSigned())
                    .intToI64SignExtend()
                    .let { Cast(it) }
                    .longToUnsigned()
                logger.debug("Memory [0x${offset.toString(16)}] = 0x${immValue.toString(16)}")
                immValue
            }
        }

        // Get the StoreTy implementation
        val storeTy = when (T::class) {
            U8StoreTy::class -> U8StoreTy
            U16StoreTy::class -> U16StoreTy
            U32StoreTy::class -> U32StoreTy
            U64StoreTy::class -> U64StoreTy
            else -> throw IllegalArgumentException("Unknown StoreTy type")
        }

        // Convert value to bytes
        val bytes = storeTy.intoBytes(value)
        val length = bytes.size.toUInt()

        val address = (base?.let { Cast(inner.regs[base.toIndex()]).ulongTruncateToU32() } ?: 0u).plus(offset)
        val pageAddressLo = inner.module.roundToPageSizeDown(address)
        val addressEnd = address.plus(length)
        val pageAddressHi = inner.module.roundToPageSizeDown(addressEnd - 1u)

        if (!isDynamic) {
            // Basic memory mode
            try {
                inner.basicMemory.getMemorySlice(inner.module, address, length)?.let { slice ->
                    bytes.copyInto(slice)
                    return goToNextInstruction()
                } ?: return panicImpl(this, programCounter)
            } catch (e: ArrayIndexOutOfBoundsException) {
                return segfaultImpl(programCounter, pageAddressHi)
            }
        } else {
            // Dynamic memory mode
            if (addressEnd < address) {
                return panicImpl(this, programCounter)
            }

            if (pageAddressLo == pageAddressHi) {
                println("Single page")
                // Single page access
                inner.dynamicMemory.pages[pageAddressLo]?.let { page ->
                    val pageOffset = (address - pageAddressLo).toInt()
                    bytes.copyInto(page, pageOffset)
                    return goToNextInstruction()
                } ?: return panicImpl(this, programCounter)
            } else {
                println("Single page")
                // Cross-page access
                val pages = inner.dynamicMemory.pages
                val lo = pages[pageAddressLo]
                val hi = pages[pageAddressHi]

                when {
                    lo != null && hi != null -> {
                        val pageSize = inner.module.memoryMap().pageSize.toInt()
                        val loLen = (pageAddressHi - address).toInt()
                        val hiLen = bytes.size - loLen

                        // Copy to low page
                        bytes.copyInto(lo, pageSize - loLen, 0, loLen)
                        // Copy to high page
                        bytes.copyInto(hi, 0, loLen, loLen + hiLen)

                        return goToNextInstruction()
                    }

                    lo == null -> return segfaultImpl(programCounter, pageAddressLo)
                    else -> return segfaultImpl(programCounter, pageAddressHi)
                }
            }
        }
    }

    fun jumpIndirectImpl(programCounter: ProgramCounter, dynamicAddress: UInt): Target? {
        if (dynamicAddress == Abi.VM_ADDR_RETURN_TO_HOST) {
            inner.apply {
                this.programCounter = ProgramCounter(UInt.MAX_VALUE)
                this.programCounterValid = false
                this.nextProgramCounter = null
                this.nextProgramCounterChanged = true
                this.interrupt = InterruptKind.Finished
            }
            return null
        }

        val target = inner.module.jumpTable().getByAddress(dynamicAddress) ?: return panicImpl(
            this,
            programCounter,
        )

        return inner.resolveJump(target) ?: panicImpl(this, programCounter)
    }
}
