package io.forge.jam.pvm.program

/**
 * Visitor interface for processing different types of VM instructions.
 * Each method represents a different instruction type and returns a generic result.
 *
 * @param R The return type for all visitor methods
 */
interface InstructionVisitor<R> {

    fun panic(): R
    fun memset(): R
    fun fallthrough(): R
    fun jumpIndirect(reg: RawReg, imm: UInt): R
    fun loadImm(reg: RawReg, imm: UInt): R
    fun loadU8(reg: RawReg, imm: UInt): R
    fun loadI8(reg: RawReg, imm: UInt): R
    fun loadU16(reg: RawReg, imm: UInt): R
    fun loadI16(reg: RawReg, imm: UInt): R
    fun loadU32(reg: RawReg, imm: UInt): R
    fun loadI32(reg: RawReg, imm: UInt): R
    fun loadU64(reg: RawReg, imm: UInt): R
    fun storeU8(reg: RawReg, imm: UInt): R
    fun storeU16(reg: RawReg, imm: UInt): R
    fun storeU32(reg: RawReg, imm: UInt): R
    fun storeU64(reg: RawReg, imm: UInt): R
    fun loadImmAndJump(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchEqImm(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchNotEqImm(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchLessUnsignedImm(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchLessSignedImm(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchGreaterOrEqualUnsignedImm(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchGreaterOrEqualSignedImm(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchLessOrEqualSignedImm(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchLessOrEqualUnsignedImm(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchGreaterSignedImm(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchGreaterUnsignedImm(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun storeImmIndirectU8(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun storeImmIndirectU16(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun storeImmIndirectU32(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun storeImmIndirectU64(reg: RawReg, imm1: UInt, imm2: UInt): R
    fun storeIndirectU8(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun storeIndirectU16(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun storeIndirectU32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun storeIndirectU64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectU8(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectI8(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectU16(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectI16(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectU32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectI32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectU64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun addImm32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun addImm64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun andImm(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun xorImm(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun orImm(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun mulImm32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun mulImm64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun setLessThanUnsignedImm(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun setLessThanSignedImm(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalLeftImm32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalLeftImm64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalRightImm32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalRightImm64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftArithmeticRightImm32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftArithmeticRightImm64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun negateAndAddImm32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun negateAndAddImm64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun setGreaterThanUnsignedImm(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun setGreaterThanSignedImm(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalRightImmAlt32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalRightImmAlt64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftArithmeticRightImmAlt32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftArithmeticRightImmAlt64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalLeftImmAlt32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalLeftImmAlt64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun cmovIfZeroImm(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun cmovIfNotZeroImm(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun rotateRightImm32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun rotateRightImmAlt32(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun rotateRightImm64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun rotateRightImmAlt64(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun branchEq(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun branchNotEq(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun branchLessUnsigned(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun branchLessSigned(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun branchGreaterOrEqualUnsigned(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun branchGreaterOrEqualSigned(reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun add32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun add64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun sub32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun sub64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun and(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun xor(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun or(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mul32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mul64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mulUpperSignedSigned(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mulUpperUnsignedUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mulUpperSignedUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun setLessThanUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun setLessThanSigned(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun shiftLogicalLeft32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun shiftLogicalLeft64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun shiftLogicalRight32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun shiftLogicalRight64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun shiftArithmeticRight32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun shiftArithmeticRight64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun divUnsigned32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun divUnsigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun divSigned32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun divSigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun remUnsigned32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun remUnsigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun remSigned32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun remSigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun andInverted(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun orInverted(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun xnor(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun maximum(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun maximumUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun minimum(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun minimumUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun rotateLeft32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun rotateLeft64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun rotateRight32(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun rotateRight64(reg1: RawReg, reg2: RawReg, reg3: RawReg): R


    fun cmovIfZero(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun cmovIfNotZero(reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun jump(imm: UInt): R
    fun ecalli(imm: UInt): R
    fun storeImmU8(imm1: UInt, imm2: UInt): R
    fun storeImmU16(imm1: UInt, imm2: UInt): R
    fun storeImmU32(imm1: UInt, imm2: UInt): R
    fun storeImmU64(imm1: UInt, imm2: UInt): R
    fun moveReg(reg1: RawReg, reg2: RawReg): R
    fun sbrk(reg1: RawReg, reg2: RawReg): R

    fun countLeadingZeroBits32(reg1: RawReg, reg2: RawReg): R
    fun countLeadingZeroBits64(reg1: RawReg, reg2: RawReg): R
    fun countTrailingZeroBits32(reg1: RawReg, reg2: RawReg): R
    fun countTrailingZeroBits64(reg1: RawReg, reg2: RawReg): R
    fun countSetBits32(reg1: RawReg, reg2: RawReg): R
    fun countSetBits64(reg1: RawReg, reg2: RawReg): R
    fun signExtend8(reg1: RawReg, reg2: RawReg): R
    fun signExtend16(reg1: RawReg, reg2: RawReg): R
    fun zeroExtend16(reg1: RawReg, reg2: RawReg): R
    fun reverseByte(reg1: RawReg, reg2: RawReg): R

    fun loadImmAndJumpIndirect(reg1: RawReg, reg2: RawReg, imm1: UInt, imm2: UInt): R
    fun loadImm64(reg: RawReg, imm: ULong): R

    // Invalid instruction
    fun invalid(): R
}
