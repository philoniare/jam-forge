package io.forge.jam.pvm.engine

import io.forge.jam.pvm.PvmLogger
import io.forge.jam.pvm.program.InstructionVisitor
import io.forge.jam.pvm.program.RawReg

class GasVisitor : InstructionVisitor<Unit> {
    private var cost: UInt = 0u
    private var lastBlockCost: UInt? = null

    companion object {
        private val logger = PvmLogger(GasVisitor::class.java)
        fun panicCost(): UInt {
            val gasVisitor = GasVisitor()
            gasVisitor.panic()
            return gasVisitor.takeBlockCost()!!
        }
    }

    fun startNewBasicBlock() {
        lastBlockCost = cost
        cost = 0u
    }

    fun takeBlockCost(): UInt? {
        val cost = lastBlockCost
        lastBlockCost = null
        return cost
    }

    override fun invalid() {
        panic()
    }

    override fun panic() {
        cost += 1u
        startNewBasicBlock()
    }

    override fun memset() {
        TODO("Not yet implemented")
    }

    override fun fallthrough() {
        cost += 1u
        startNewBasicBlock()
    }

    override fun sbrk(_d: RawReg, _s: RawReg) {
        cost += 1u
    }

    override fun countLeadingZeroBits32(reg1: RawReg, reg2: RawReg) {
        cost += 1u
    }

    override fun countLeadingZeroBits64(reg1: RawReg, reg2: RawReg) {
        cost += 1u
    }

    override fun countTrailingZeroBits32(reg1: RawReg, reg2: RawReg) {
        cost += 1u
    }

    override fun countTrailingZeroBits64(reg1: RawReg, reg2: RawReg) {
        cost += 1u
    }

    override fun countSetBits32(reg1: RawReg, reg2: RawReg) {
        cost += 1u
    }

    override fun countSetBits64(reg1: RawReg, reg2: RawReg) {
        cost += 1u
    }

    override fun signExtend8(reg1: RawReg, reg2: RawReg) {
        cost += 1u
    }

    override fun signExtend16(reg1: RawReg, reg2: RawReg) {
        cost += 1u
    }

    override fun zeroExtend16(reg1: RawReg, reg2: RawReg) {
        cost += 1u
    }

    override fun reverseByte(reg1: RawReg, reg2: RawReg) {
        cost += 1u
    }

    override fun ecalli(_imm: UInt) {
        cost += 1u
    }

    override fun setLessThanUnsigned(_d: RawReg, _s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun setLessThanSigned(_d: RawReg, _s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun shiftArithmeticRight32(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun shiftLogicalLeft32(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun shiftLogicalRight64(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun shiftArithmeticRight64(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun shiftLogicalLeft64(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun shiftLogicalRight32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        cost += 1u
    }

    override fun xor(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun and(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun or(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun add32(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun add64(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun sub32(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun sub64(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun mul32(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun mul64(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun mulUpperSignedSigned(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun mulUpperUnsignedUnsigned(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun mulUpperSignedUnsigned(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun divUnsigned32(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun divSigned32(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun remUnsigned32(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun remSigned32(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun divUnsigned64(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun divSigned64(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun remUnsigned64(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun remSigned64(d: RawReg, s1: RawReg, s2: RawReg) {
        cost += 1u
    }

    override fun andInverted(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun orInverted(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun xnor(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun maximum(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun maximumUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun minimum(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun minimumUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun rotateLeft32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun rotateLeft64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun rotateRight32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun rotateRight64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun mulImm32(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun mulImm64(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun setLessThanUnsignedImm(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun setLessThanSignedImm(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun setGreaterThanUnsignedImm(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun setGreaterThanSignedImm(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun shiftLogicalRightImm32(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun shiftArithmeticRightImm32(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun shiftLogicalLeftImm32(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun shiftLogicalRightImmAlt32(d: RawReg, s2: RawReg, s1: UInt) {
        cost += 1u
    }

    override fun shiftArithmeticRightImmAlt32(d: RawReg, s2: RawReg, s1: UInt) {
        cost += 1u
    }

    override fun shiftLogicalLeftImmAlt32(d: RawReg, s2: RawReg, s1: UInt) {
        cost += 1u
    }

    override fun shiftLogicalRightImm64(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun shiftArithmeticRightImm64(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun shiftLogicalLeftImm64(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun shiftLogicalRightImmAlt64(d: RawReg, s2: RawReg, s1: UInt) {
        cost += 1u
    }

    override fun shiftArithmeticRightImmAlt64(d: RawReg, s2: RawReg, s1: UInt) {
        cost += 1u
    }

    override fun shiftLogicalLeftImmAlt64(d: RawReg, s2: RawReg, s1: UInt) {
        cost += 1u
    }

    override fun orImm(d: RawReg, s: RawReg, imm: UInt) {
        cost += 1u
    }

    override fun andImm(d: RawReg, s: RawReg, imm: UInt) {
        cost += 1u
    }

    override fun xorImm(d: RawReg, s: RawReg, imm: UInt) {
        cost += 1u
    }

    override fun moveReg(d: RawReg, s: RawReg) {
        cost += 1u
    }

    override fun cmovIfZero(d: RawReg, s: RawReg, c: RawReg) {
        cost += 1u
    }

    override fun cmovIfNotZero(d: RawReg, s: RawReg, c: RawReg) {
        cost += 1u
    }

    override fun cmovIfZeroImm(d: RawReg, c: RawReg, s: UInt) {
        cost += 1u
    }

    override fun cmovIfNotZeroImm(d: RawReg, c: RawReg, s: UInt) {
        cost += 1u
    }

    override fun rotateRightImm32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun rotateRightImmAlt32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun rotateRightImm64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun rotateRightImmAlt64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun addImm32(d: RawReg, s: RawReg, imm: UInt) {
        cost += 1u
    }

    override fun addImm64(d: RawReg, s: RawReg, imm: UInt) {
        cost += 1u
    }

    override fun negateAndAddImm32(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun negateAndAddImm64(d: RawReg, s1: RawReg, s2: UInt) {
        cost += 1u
    }

    override fun storeImmIndirectU8(base: RawReg, offset: UInt, value: UInt) {
        cost += 1u
    }

    override fun storeImmIndirectU16(base: RawReg, offset: UInt, value: UInt) {
        cost += 1u
    }

    override fun storeImmIndirectU32(base: RawReg, offset: UInt, value: UInt) {
        cost += 1u
    }

    override fun storeImmIndirectU64(base: RawReg, offset: UInt, value: UInt) {
        cost += 1u
    }

    override fun storeIndirectU8(src: RawReg, base: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun storeIndirectU16(src: RawReg, base: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun storeIndirectU32(src: RawReg, base: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun storeIndirectU64(src: RawReg, base: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun storeImmU8(offset: UInt, value: UInt) {
        cost += 1u
    }

    override fun storeImmU16(offset: UInt, value: UInt) {
        cost += 1u
    }

    override fun storeImmU32(offset: UInt, value: UInt) {
        cost += 1u
    }

    override fun storeImmU64(offset: UInt, value: UInt) {
        cost += 1u
    }

    override fun storeU8(src: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun storeU16(src: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun storeU32(src: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun storeU64(src: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadIndirectU8(dst: RawReg, base: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadIndirectI8(dst: RawReg, base: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadIndirectU16(dst: RawReg, base: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadIndirectI16(dst: RawReg, base: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadIndirectU32(dst: RawReg, base: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadIndirectI32(dst: RawReg, base: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadIndirectU64(dst: RawReg, base: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadU8(dst: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadI8(dst: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadU16(dst: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadI16(dst: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadU32(dst: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadI32(dst: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun loadU64(dst: RawReg, offset: UInt) {
        cost += 1u
    }

    override fun branchLessUnsigned(s1: RawReg, s2: RawReg, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchLessSigned(s1: RawReg, s2: RawReg, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchGreaterOrEqualUnsigned(s1: RawReg, s2: RawReg, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchGreaterOrEqualSigned(s1: RawReg, s2: RawReg, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchEq(s1: RawReg, s2: RawReg, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchNotEq(s1: RawReg, s2: RawReg, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchEqImm(s1: RawReg, s2: UInt, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchNotEqImm(s1: RawReg, s2: UInt, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchLessUnsignedImm(s1: RawReg, s2: UInt, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchLessSignedImm(s1: RawReg, s2: UInt, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchGreaterOrEqualUnsignedImm(s1: RawReg, s2: UInt, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchGreaterOrEqualSignedImm(s1: RawReg, s2: UInt, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchLessOrEqualUnsignedImm(s1: RawReg, s2: UInt, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchLessOrEqualSignedImm(s1: RawReg, s2: UInt, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchGreaterUnsignedImm(s1: RawReg, s2: UInt, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun branchGreaterSignedImm(s1: RawReg, s2: UInt, imm: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun loadImm(dst: RawReg, value: UInt) {
        cost += 1u
    }

    override fun loadImm64(dst: RawReg, value: ULong) {
        cost += 1u
    }

    override fun loadImmAndJump(ra: RawReg, value: UInt, target: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun loadImmAndJumpIndirect(ra: RawReg, base: RawReg, value: UInt, offset: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun jump(target: UInt) {
        cost += 1u
        startNewBasicBlock()
    }

    override fun jumpIndirect(base: RawReg, offset: UInt) {
        cost += 1u
        startNewBasicBlock()
    }
}
