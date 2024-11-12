package io.forge.jam.pvm.engine

import io.forge.jam.core.toHex
import io.forge.jam.pvm.Abi
import io.forge.jam.pvm.PvmLogger
import io.forge.jam.pvm.program.ProgramCounter
import io.forge.jam.pvm.program.Reg

typealias Target = UInt

fun trapImpl(visitor: Visitor, programCounter: ProgramCounter): Target? {
    with(visitor.inner) {
        this.programCounter = programCounter
        this.programCounterValid = true
        this.nextProgramCounter = null
        this.nextProgramCounterChanged = true
        this.interrupt = InterruptKind.Trap
    }
    return null
}

interface LoadTy {
    fun fromSlice(bytes: ByteArray): ULong
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
        // Convert bytes to Short (signed) -> Long (sign extended) -> ULong
        val value = bytes.toShort(0)
        return Cast(Cast(value).shortToI64SignExtend()).longToUnsigned()
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
        is RegImm.RegValue -> inner.regs[regimm.reg.toIndex()].toUInt()
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
        size: UInt,
        isDynamic: Boolean
    ): Target? {
        val address = base?.let { get32(RegImm.RegValue(it)) }?.plus(offset) ?: offset

        if (!isDynamic) {
            // Basic memory mode
            inner.basicMemory.getMemorySlice(inner.module, address, size)?.let { slice ->
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
            } ?: return trapImpl(this, programCounter)
        } else {
            // Dynamic memory mode
            val addressEnd = address.plus(size)
            if (addressEnd < address) {
                return trapImpl(this, programCounter)
            }

            val pageAddressLo = inner.module.roundToPageSizeDown(address)
            val pageAddressHi = inner.module.roundToPageSizeDown(addressEnd - 1u)

            if (pageAddressLo == pageAddressHi) {
                // Single page access
                inner.dynamicMemory.pages[pageAddressLo]?.let { page ->
                    val pageOffset = (address - pageAddressLo).toInt()
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
                    val value = loadTy.fromSlice(page.sliceArray(pageOffset until pageOffset + size.toInt()))
                    set64(dst, value)
                    return goToNextInstruction()
                } ?: return segfaultImpl(programCounter, pageAddressLo)
            } else {
                // Cross-page access
                val pages = inner.dynamicMemory.pages
                val lo = pages[pageAddressLo]
                val hi = pages[pageAddressHi]

                when {
                    lo != null && hi != null -> {
                        val pageSize = inner.module.memoryMap().pageSize.toInt()
                        val loLen = (pageAddressHi - address).toInt()
                        val hiLen = size.toInt() - loLen
                        val buffer = ByteArray(size.toInt())

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
                    }

                    lo == null -> return segfaultImpl(programCounter, pageAddressLo)
                    else -> return segfaultImpl(programCounter, pageAddressHi)
                }
            }
        }

    }

    fun store() {

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

        val target = inner.module.jumpTable().getByAddress(dynamicAddress) ?: return trapImpl(
            this,
            programCounter,
        )

        return inner.resolveJump(target) ?: trapImpl(this, programCounter)
    }
}
