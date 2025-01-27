package io.forge.jam.pvm.program

import io.forge.jam.pvm.writeSimpleVarint
import io.forge.jam.pvm.writeSimpleVarint64

/**
 * Represents a virtual machine instruction.
 * Each variant contains the instruction type and its associated parameters.
 */
sealed class Instruction {
    data object Panic : Instruction()
    data object Fallthrough : Instruction()
    data object Memset : Instruction()
    data class JumpIndirect(val reg: RawReg, val imm: UInt) : Instruction()
    data class LoadImm(val reg: RawReg, val imm: UInt) : Instruction()
    data class LoadU8(val reg: RawReg, val imm: UInt) : Instruction()
    data class LoadI8(val reg: RawReg, val imm: UInt) : Instruction()
    data class LoadU16(val reg: RawReg, val imm: UInt) : Instruction()
    data class LoadI16(val reg: RawReg, val imm: UInt) : Instruction()
    data class LoadU32(val reg: RawReg, val imm: UInt) : Instruction()
    data class LoadI32(val reg: RawReg, val imm: UInt) : Instruction()
    data class LoadU64(val reg: RawReg, val imm: UInt) : Instruction()
    data class StoreU8(val reg: RawReg, val imm: UInt) : Instruction()
    data class StoreU16(val reg: RawReg, val imm: UInt) : Instruction()
    data class StoreU32(val reg: RawReg, val imm: UInt) : Instruction()
    data class StoreU64(val reg: RawReg, val imm: UInt) : Instruction()

    data class LoadImmAndJump(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class BranchEqImm(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class BranchNotEqImm(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class BranchLessUnsignedImm(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class BranchLessSignedImm(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class BranchGreaterOrEqualUnsignedImm(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class BranchGreaterOrEqualSignedImm(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class BranchLessOrEqualSignedImm(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class BranchLessOrEqualUnsignedImm(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class BranchGreaterSignedImm(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class BranchGreaterUnsignedImm(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()

    data class StoreImmIndirectU8(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class StoreImmIndirectU16(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class StoreImmIndirectU32(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()
    data class StoreImmIndirectU64(val reg: RawReg, val imm1: UInt, val imm2: UInt) : Instruction()

    data class StoreIndirectU8(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class StoreIndirectU16(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class StoreIndirectU32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class StoreIndirectU64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class LoadIndirectU8(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class LoadIndirectI8(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class LoadIndirectU16(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class LoadIndirectI16(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class LoadIndirectI32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class LoadIndirectU32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class LoadIndirectU64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class AddImm32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class AddImm64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class AndImm(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class XorImm(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class OrImm(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class MulImm32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class MulImm64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class SetLessThanUnsignedImm(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class SetLessThanSignedImm(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftLogicalLeftImm32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftLogicalLeftImm64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftLogicalRightImm32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftLogicalRightImm64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftArithmeticRightImm32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftArithmeticRightImm64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class NegateAndAddImm32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class NegateAndAddImm64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class SetGreaterThanUnsignedImm(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class SetGreaterThanSignedImm(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftLogicalRightImmAlt32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftLogicalRightImmAlt64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftArithmeticRightImmAlt32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftArithmeticRightImmAlt64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftLogicalLeftImmAlt32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class ShiftLogicalLeftImmAlt64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class CmovIfZeroImm(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class CmovIfNotZeroImm(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()

    data class RotateRightImm32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class RotateRightImmAlt32(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class RotateRightImm64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class RotateRightImmAlt64(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()

    data class BranchEq(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class BranchNotEq(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class BranchLessUnsigned(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class BranchLessSigned(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class BranchGreaterOrEqualUnsigned(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class BranchGreaterOrEqualSigned(val reg1: RawReg, val reg2: RawReg, val imm: UInt) : Instruction()
    data class Add32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class Add64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class Sub32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class Sub64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class And(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class Xor(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class Or(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class Mul32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class Mul64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class MulUpperSignedSigned(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class MulUpperUnsignedUnsigned(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class MulUpperSignedUnsigned(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class SetLessThanUnsigned(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class SetLessThanSigned(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class ShiftLogicalLeft32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class ShiftLogicalLeft64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class ShiftLogicalRight32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class ShiftLogicalRight64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class ShiftArithmeticRight32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class ShiftArithmeticRight64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class DivUnsigned32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class DivUnsigned64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class DivSigned32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class DivSigned64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class RemUnsigned32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class RemUnsigned64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class RemSigned32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class RemSigned64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class CmovIfZero(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class CmovIfNotZero(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class AndInverted(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class OrInverted(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class Xnor(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class Maximum(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class MaximumUnsigned(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class Minimum(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class MinimumUnsigned(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class RotateLeft32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class RotateLeft64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class RotateRight32(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()
    data class RotateRight64(val reg1: RawReg, val reg2: RawReg, val reg3: RawReg) : Instruction()

    data class Jump(val imm: UInt) : Instruction()
    data class Ecalli(val imm: UInt) : Instruction()
    data class StoreImmU8(val imm1: UInt, val imm2: UInt) : Instruction()
    data class StoreImmU16(val imm1: UInt, val imm2: UInt) : Instruction()
    data class StoreImmU32(val imm1: UInt, val imm2: UInt) : Instruction()
    data class StoreImmU64(val imm1: UInt, val imm2: UInt) : Instruction()
    data class MoveReg(val reg1: RawReg, val reg2: RawReg) : Instruction()
    data class Sbrk(val reg1: RawReg, val reg2: RawReg) : Instruction()
    data class CountLeadingZeroBits32(val reg1: RawReg, val reg2: RawReg) : Instruction()
    data class CountLeadingZeroBits64(val reg1: RawReg, val reg2: RawReg) : Instruction()
    data class CountTrailingZeroBits32(val reg1: RawReg, val reg2: RawReg) : Instruction()
    data class CountTrailingZeroBits64(val reg1: RawReg, val reg2: RawReg) : Instruction()
    data class CountSetBits32(val reg1: RawReg, val reg2: RawReg) : Instruction()
    data class CountSetBits64(val reg1: RawReg, val reg2: RawReg) : Instruction()
    data class SignExtend8(val reg1: RawReg, val reg2: RawReg) : Instruction()
    data class SignExtend16(val reg1: RawReg, val reg2: RawReg) : Instruction()
    data class ZeroExtend16(val reg1: RawReg, val reg2: RawReg) : Instruction()
    data class ReverseByte(val reg1: RawReg, val reg2: RawReg) : Instruction()

    data class LoadImmAndJumpIndirect(val reg1: RawReg, val reg2: RawReg, val imm1: UInt, val imm2: UInt) :
        Instruction()

    data class LoadImm64(val reg: RawReg, val imm: ULong) : Instruction()
    data object Invalid : Instruction()

    /**
     * Visits the instruction with a visitor implementation
     */
    fun <T> visit(visitor: InstructionVisitor<T>): T = when (this) {
        is Panic -> visitor.panic()
        is Memset -> visitor.memset()
        is Fallthrough -> visitor.fallthrough()
        is JumpIndirect -> visitor.jumpIndirect(reg, imm)
        is LoadImm -> visitor.loadImm(reg, imm)
        is LoadU8 -> visitor.loadU8(reg, imm)
        is LoadI8 -> visitor.loadI8(reg, imm)
        is LoadU16 -> visitor.loadU16(reg, imm)
        is LoadI16 -> visitor.loadI16(reg, imm)
        is LoadU32 -> visitor.loadU32(reg, imm)
        is LoadI32 -> visitor.loadI32(reg, imm)
        is LoadU64 -> visitor.loadU64(reg, imm)
        is StoreU8 -> visitor.storeU8(reg, imm)
        is StoreU16 -> visitor.storeU16(reg, imm)
        is StoreU32 -> visitor.storeU32(reg, imm)
        is StoreU64 -> visitor.storeU64(reg, imm)
        is LoadImmAndJump -> visitor.loadImmAndJump(reg, imm1, imm2)
        is BranchEqImm -> visitor.branchEqImm(reg, imm1, imm2)
        is BranchNotEqImm -> visitor.branchNotEqImm(reg, imm1, imm2)
        is BranchLessUnsignedImm -> visitor.branchLessUnsignedImm(reg, imm1, imm2)
        is BranchLessSignedImm -> visitor.branchLessSignedImm(reg, imm1, imm2)
        is BranchGreaterOrEqualUnsignedImm -> visitor.branchGreaterOrEqualUnsignedImm(reg, imm1, imm2)
        is BranchGreaterOrEqualSignedImm -> visitor.branchGreaterOrEqualSignedImm(reg, imm1, imm2)
        is BranchLessOrEqualSignedImm -> visitor.branchLessOrEqualSignedImm(reg, imm1, imm2)
        is BranchLessOrEqualUnsignedImm -> visitor.branchLessOrEqualUnsignedImm(reg, imm1, imm2)
        is BranchGreaterSignedImm -> visitor.branchGreaterSignedImm(reg, imm1, imm2)
        is BranchGreaterUnsignedImm -> visitor.branchGreaterUnsignedImm(reg, imm1, imm2)
        is StoreImmIndirectU8 -> visitor.storeImmIndirectU8(reg, imm1, imm2)
        is StoreImmIndirectU16 -> visitor.storeImmIndirectU16(reg, imm1, imm2)
        is StoreImmIndirectU32 -> visitor.storeImmIndirectU32(reg, imm1, imm2)
        is StoreImmIndirectU64 -> visitor.storeImmIndirectU64(reg, imm1, imm2)
        is StoreIndirectU8 -> visitor.storeIndirectU8(reg1, reg2, imm)
        is StoreIndirectU16 -> visitor.storeIndirectU16(reg1, reg2, imm)
        is StoreIndirectU32 -> visitor.storeIndirectU32(reg1, reg2, imm)
        is StoreIndirectU64 -> visitor.storeIndirectU64(reg1, reg2, imm)
        is LoadIndirectU8 -> visitor.loadIndirectU8(reg1, reg2, imm)
        is LoadIndirectI8 -> visitor.loadIndirectI8(reg1, reg2, imm)
        is LoadIndirectU16 -> visitor.loadIndirectU16(reg1, reg2, imm)
        is LoadIndirectI16 -> visitor.loadIndirectI16(reg1, reg2, imm)
        is LoadIndirectI32 -> visitor.loadIndirectI32(reg1, reg2, imm)
        is LoadIndirectU32 -> visitor.loadIndirectU32(reg1, reg2, imm)
        is LoadIndirectU64 -> visitor.loadIndirectU64(reg1, reg2, imm)
        is AddImm32 -> visitor.addImm32(reg1, reg2, imm)
        is AddImm64 -> visitor.addImm64(reg1, reg2, imm)
        is AndImm -> visitor.andImm(reg1, reg2, imm)
        is XorImm -> visitor.xorImm(reg1, reg2, imm)
        is OrImm -> visitor.orImm(reg1, reg2, imm)
        is MulImm32 -> visitor.mulImm32(reg1, reg2, imm)
        is MulImm64 -> visitor.mulImm64(reg1, reg2, imm)
        is MulUpperSignedSigned -> visitor.mulUpperSignedSigned(reg1, reg2, reg3)
        is MulUpperSignedUnsigned -> visitor.mulUpperSignedUnsigned(reg1, reg2, reg3)
        is MulUpperUnsignedUnsigned -> visitor.mulUpperUnsignedUnsigned(reg1, reg2, reg3)
        is SetLessThanUnsignedImm -> visitor.setLessThanUnsignedImm(reg1, reg2, imm)
        is SetLessThanSignedImm -> visitor.setLessThanSignedImm(reg1, reg2, imm)
        is ShiftLogicalLeft32 -> visitor.shiftLogicalLeft32(reg1, reg2, reg3)
        is ShiftLogicalLeft64 -> visitor.shiftLogicalLeft64(reg1, reg2, reg3)
        is ShiftLogicalRight32 -> visitor.shiftLogicalRight32(reg1, reg2, reg3)
        is ShiftLogicalRight64 -> visitor.shiftLogicalRight64(reg1, reg2, reg3)
        is ShiftArithmeticRight32 -> visitor.shiftArithmeticRight32(reg1, reg2, reg3)
        is ShiftArithmeticRight64 -> visitor.shiftArithmeticRight64(reg1, reg2, reg3)
        is NegateAndAddImm32 -> visitor.negateAndAddImm32(reg1, reg2, imm)
        is NegateAndAddImm64 -> visitor.negateAndAddImm64(reg1, reg2, imm)
        is SetGreaterThanUnsignedImm -> visitor.setGreaterThanUnsignedImm(reg1, reg2, imm)
        is SetGreaterThanSignedImm -> visitor.setGreaterThanSignedImm(reg1, reg2, imm)
        is ShiftLogicalRightImmAlt32 -> visitor.shiftLogicalRightImmAlt32(reg1, reg2, imm)
        is ShiftLogicalRightImmAlt64 -> visitor.shiftLogicalRightImmAlt64(reg1, reg2, imm)
        is ShiftArithmeticRightImmAlt32 -> visitor.shiftArithmeticRightImmAlt32(reg1, reg2, imm)
        is ShiftArithmeticRightImmAlt64 -> visitor.shiftArithmeticRightImmAlt64(reg1, reg2, imm)
        is ShiftLogicalLeftImmAlt32 -> visitor.shiftLogicalLeftImmAlt32(reg1, reg2, imm)
        is ShiftLogicalLeftImmAlt64 -> visitor.shiftLogicalLeftImmAlt64(reg1, reg2, imm)
        is CmovIfZeroImm -> visitor.cmovIfZeroImm(reg1, reg2, imm)
        is CmovIfNotZeroImm -> visitor.cmovIfNotZeroImm(reg1, reg2, imm)

        is RotateRightImm32 -> visitor.rotateRightImm32(reg1, reg2, imm)
        is RotateRightImm64 -> visitor.rotateRightImm64(reg1, reg2, imm)
        is RotateRightImmAlt32 -> visitor.rotateRightImmAlt32(reg1, reg2, imm)
        is RotateRightImmAlt64 -> visitor.rotateRightImmAlt64(reg1, reg2, imm)

        is BranchEq -> visitor.branchEq(reg1, reg2, imm)
        is BranchNotEq -> visitor.branchNotEq(reg1, reg2, imm)
        is BranchLessUnsigned -> visitor.branchLessUnsigned(reg1, reg2, imm)
        is BranchLessSigned -> visitor.branchLessSigned(reg1, reg2, imm)
        is BranchGreaterOrEqualUnsigned -> visitor.branchGreaterOrEqualUnsigned(reg1, reg2, imm)
        is BranchGreaterOrEqualSigned -> visitor.branchGreaterOrEqualSigned(reg1, reg2, imm)
        is Add32 -> visitor.add32(reg1, reg2, reg3)
        is Add64 -> visitor.add64(reg1, reg2, reg3)
        is Sub32 -> visitor.sub32(reg1, reg2, reg3)
        is Sub64 -> visitor.sub64(reg1, reg2, reg3)
        is And -> visitor.and(reg1, reg2, reg3)
        is Xor -> visitor.xor(reg1, reg2, reg3)
        is Or -> visitor.or(reg1, reg2, reg3)
        is Mul32 -> visitor.mul32(reg1, reg2, reg3)
        is Mul64 -> visitor.mul64(reg1, reg2, reg3)
        is SetLessThanUnsigned -> visitor.setLessThanUnsigned(reg1, reg2, reg3)
        is SetLessThanSigned -> visitor.setLessThanSigned(reg1, reg2, reg3)
        is DivUnsigned32 -> visitor.divUnsigned32(reg1, reg2, reg3)
        is DivUnsigned64 -> visitor.divUnsigned64(reg1, reg2, reg3)
        is DivSigned32 -> visitor.divSigned32(reg1, reg2, reg3)
        is DivSigned64 -> visitor.divSigned64(reg1, reg2, reg3)
        is RemUnsigned32 -> visitor.remUnsigned32(reg1, reg2, reg3)
        is RemUnsigned64 -> visitor.remUnsigned64(reg1, reg2, reg3)
        is RemSigned32 -> visitor.remSigned32(reg1, reg2, reg3)
        is RemSigned64 -> visitor.remSigned64(reg1, reg2, reg3)
        is CmovIfZero -> visitor.cmovIfZero(reg1, reg2, reg3)
        is CmovIfNotZero -> visitor.cmovIfNotZero(reg1, reg2, reg3)

        is AndInverted -> visitor.andInverted(reg1, reg2, reg3)
        is OrInverted -> visitor.orInverted(reg1, reg2, reg3)
        is Xnor -> visitor.xnor(reg1, reg2, reg3)
        is Maximum -> visitor.maximum(reg1, reg2, reg3)
        is MaximumUnsigned -> visitor.maximumUnsigned(reg1, reg2, reg3)
        is Minimum -> visitor.minimum(reg1, reg2, reg3)
        is MinimumUnsigned -> visitor.minimumUnsigned(reg1, reg2, reg3)
        is RotateLeft32 -> visitor.rotateLeft32(reg1, reg2, reg3)
        is RotateLeft64 -> visitor.rotateLeft64(reg1, reg2, reg3)
        is RotateRight32 -> visitor.rotateRight32(reg1, reg2, reg3)
        is RotateRight64 -> visitor.rotateRight64(reg1, reg2, reg3)

        is Jump -> visitor.jump(imm)
        is Ecalli -> visitor.ecalli(imm)
        is StoreImmU8 -> visitor.storeImmU8(imm1, imm2)
        is StoreImmU16 -> visitor.storeImmU16(imm1, imm2)
        is StoreImmU32 -> visitor.storeImmU32(imm1, imm2)
        is StoreImmU64 -> visitor.storeImmU64(imm1, imm2)
        is MoveReg -> visitor.moveReg(reg1, reg2)
        is Sbrk -> visitor.sbrk(reg1, reg2)

        is CountLeadingZeroBits32 -> visitor.countLeadingZeroBits32(reg1, reg2)
        is CountLeadingZeroBits64 -> visitor.countLeadingZeroBits64(reg1, reg2)
        is CountTrailingZeroBits32 -> visitor.countTrailingZeroBits32(reg1, reg2)
        is CountTrailingZeroBits64 -> visitor.countTrailingZeroBits64(reg1, reg2)
        is CountSetBits32 -> visitor.countSetBits32(reg1, reg2)
        is CountSetBits64 -> visitor.countSetBits64(reg1, reg2)
        is SignExtend8 -> visitor.signExtend8(reg1, reg2)
        is SignExtend16 -> visitor.signExtend16(reg1, reg2)
        is ZeroExtend16 -> visitor.zeroExtend16(reg1, reg2)
        is ReverseByte -> visitor.reverseByte(reg1, reg2)

        is LoadImmAndJumpIndirect -> visitor.loadImmAndJumpIndirect(reg1, reg2, imm1, imm2)
        is Invalid -> visitor.invalid()
        is ShiftArithmeticRightImm32 -> visitor.shiftArithmeticRightImm32(reg1, reg2, imm)
        is ShiftArithmeticRightImm64 -> visitor.shiftArithmeticRightImm64(reg1, reg2, imm)
        is ShiftLogicalLeftImm32 -> visitor.shiftLogicalLeftImm32(reg1, reg2, imm)
        is ShiftLogicalLeftImm64 -> visitor.shiftLogicalLeftImm64(reg1, reg2, imm)
        is ShiftLogicalRightImm32 -> visitor.shiftLogicalRightImm32(reg1, reg2, imm)
        is ShiftLogicalRightImm64 -> visitor.shiftLogicalRightImm64(reg1, reg2, imm)
        is LoadImm64 -> visitor.loadImm64(reg, imm)
    }

    /**
     * Gets the opcode for this instruction
     */
    fun opcode(): Opcode = when (this) {
        is Panic -> Opcode.panic
        is Fallthrough -> Opcode.fallthrough
        is JumpIndirect -> Opcode.jump_indirect
        is LoadImm -> Opcode.load_imm
        is LoadU8 -> Opcode.load_u8
        is LoadI8 -> Opcode.load_i8
        is LoadU16 -> Opcode.load_u16
        is LoadI16 -> Opcode.load_i16
        is LoadU32 -> Opcode.load_u32
        is LoadI32 -> Opcode.load_i32
        is LoadU64 -> Opcode.load_u64
        is StoreU8 -> Opcode.store_u8
        is StoreU16 -> Opcode.store_u16
        is StoreU32 -> Opcode.store_u32
        is StoreU64 -> Opcode.store_u64
        is LoadImmAndJump -> Opcode.load_imm_and_jump
        is BranchEqImm -> Opcode.branch_eq_imm
        is BranchNotEqImm -> Opcode.branch_not_eq_imm
        is BranchLessUnsignedImm -> Opcode.branch_less_unsigned_imm
        is BranchLessSignedImm -> Opcode.branch_less_signed_imm
        is BranchGreaterOrEqualUnsignedImm -> Opcode.branch_greater_or_equal_unsigned_imm
        is BranchGreaterOrEqualSignedImm -> Opcode.branch_greater_or_equal_signed_imm
        is BranchLessOrEqualSignedImm -> Opcode.branch_less_or_equal_signed_imm
        is BranchLessOrEqualUnsignedImm -> Opcode.branch_less_or_equal_unsigned_imm
        is BranchGreaterSignedImm -> Opcode.branch_greater_signed_imm
        is BranchGreaterUnsignedImm -> Opcode.branch_greater_unsigned_imm
        is StoreImmIndirectU8 -> Opcode.store_imm_indirect_u8
        is StoreImmIndirectU16 -> Opcode.store_imm_indirect_u16
        is StoreImmIndirectU32 -> Opcode.store_imm_indirect_u32
        is StoreImmIndirectU64 -> Opcode.store_imm_indirect_u64
        is StoreIndirectU8 -> Opcode.store_indirect_u8
        is StoreIndirectU16 -> Opcode.store_indirect_u16
        is StoreIndirectU32 -> Opcode.store_indirect_u32
        is StoreIndirectU64 -> Opcode.store_indirect_u64
        is LoadIndirectU8 -> Opcode.load_indirect_u8
        is LoadIndirectI8 -> Opcode.load_indirect_i8
        is LoadIndirectU16 -> Opcode.load_indirect_u16
        is LoadIndirectI16 -> Opcode.load_indirect_i16
        is LoadIndirectI32 -> Opcode.load_indirect_i32
        is LoadIndirectU32 -> Opcode.load_indirect_u32
        is LoadIndirectU64 -> Opcode.load_indirect_u64
        is AddImm32 -> Opcode.add_imm_32
        is AddImm64 -> Opcode.add_imm_64
        is AndImm -> Opcode.and_imm
        is XorImm -> Opcode.xor_imm
        is OrImm -> Opcode.or_imm
        is MulImm32 -> Opcode.mul_imm_32
        is MulImm64 -> Opcode.mul_imm_64
        is SetLessThanUnsignedImm -> Opcode.set_less_than_unsigned_imm
        is SetLessThanSignedImm -> Opcode.set_less_than_signed_imm
        is ShiftLogicalLeftImm32 -> Opcode.shift_logical_left_imm_32
        is ShiftLogicalLeftImm64 -> Opcode.shift_logical_left_imm_64
        is ShiftLogicalRightImm32 -> Opcode.shift_logical_right_imm_32
        is ShiftLogicalRightImm64 -> Opcode.shift_logical_right_imm_64
        is NegateAndAddImm32 -> Opcode.negate_and_add_imm_32
        is NegateAndAddImm64 -> Opcode.negate_and_add_imm_64
        is SetGreaterThanUnsignedImm -> Opcode.set_greater_than_unsigned_imm
        is SetGreaterThanSignedImm -> Opcode.set_greater_than_signed_imm
        is ShiftLogicalRightImmAlt32 -> Opcode.shift_logical_right_imm_alt_32
        is ShiftLogicalRightImmAlt64 -> Opcode.shift_logical_right_imm_alt_64
        is ShiftArithmeticRightImmAlt32 -> Opcode.shift_arithmetic_right_imm_alt_32
        is ShiftArithmeticRightImmAlt64 -> Opcode.shift_arithmetic_right_imm_alt_64
        is ShiftLogicalLeftImmAlt32 -> Opcode.shift_logical_left_imm_alt_32
        is ShiftLogicalLeftImmAlt64 -> Opcode.shift_logical_left_imm_alt_64
        is CmovIfZeroImm -> Opcode.cmov_if_zero_imm
        is CmovIfNotZeroImm -> Opcode.cmov_if_not_zero_imm

        is BranchEq -> Opcode.branch_eq
        is BranchNotEq -> Opcode.branch_not_eq
        is BranchLessUnsigned -> Opcode.branch_less_unsigned
        is BranchLessSigned -> Opcode.branch_less_signed
        is BranchGreaterOrEqualUnsigned -> Opcode.branch_greater_or_equal_unsigned
        is BranchGreaterOrEqualSigned -> Opcode.branch_greater_or_equal_signed
        is Add32 -> Opcode.add_32
        is Add64 -> Opcode.add_64
        is Sub32 -> Opcode.sub_32
        is Sub64 -> Opcode.sub_64
        is And -> Opcode.and
        is Xor -> Opcode.xor
        is Or -> Opcode.or
        is Mul32 -> Opcode.mul_32
        is Mul64 -> Opcode.mul_64
        is MulUpperSignedSigned -> Opcode.mul_upper_signed_signed
        is MulUpperUnsignedUnsigned -> Opcode.mul_upper_unsigned_unsigned
        is MulUpperSignedUnsigned -> Opcode.mul_upper_signed_unsigned
        is SetLessThanUnsigned -> Opcode.set_less_than_unsigned
        is SetLessThanSigned -> Opcode.set_less_than_signed
        is ShiftLogicalLeft32 -> Opcode.shift_logical_left_32
        is ShiftLogicalLeft64 -> Opcode.shift_logical_left_64
        is ShiftLogicalRight32 -> Opcode.shift_logical_right_32
        is ShiftLogicalRight64 -> Opcode.shift_logical_right_64
        is ShiftArithmeticRight32 -> Opcode.shift_arithmetic_right_32
        is ShiftArithmeticRight64 -> Opcode.shift_arithmetic_right_64
        is DivUnsigned32 -> Opcode.div_unsigned_32
        is DivUnsigned64 -> Opcode.div_unsigned_64
        is DivSigned32 -> Opcode.div_signed_32
        is DivSigned64 -> Opcode.div_signed_64
        is RemUnsigned32 -> Opcode.rem_unsigned_32
        is RemUnsigned64 -> Opcode.rem_unsigned_64
        is RemSigned32 -> Opcode.rem_signed_32
        is RemSigned64 -> Opcode.rem_signed_64
        is CmovIfZero -> Opcode.cmov_if_zero
        is CmovIfNotZero -> Opcode.cmov_if_not_zero
        is Jump -> Opcode.jump
        is Ecalli -> Opcode.ecalli
        is StoreImmU8 -> Opcode.store_imm_u8
        is StoreImmU16 -> Opcode.store_imm_u16
        is StoreImmU32 -> Opcode.store_imm_u32
        is StoreImmU64 -> Opcode.store_imm_u64
        is MoveReg -> Opcode.move_reg
        is Sbrk -> Opcode.sbrk
        is LoadImmAndJumpIndirect -> Opcode.load_imm_and_jump_indirect
        is LoadImm64 -> Opcode.load_imm_64
        is ShiftArithmeticRightImm32 -> Opcode.shift_arithmetic_right_imm_32
        is ShiftArithmeticRightImm64 -> Opcode.shift_arithmetic_right_imm_64
        is Invalid -> Opcode.panic
        is AndInverted -> TODO()
        is CountLeadingZeroBits32 -> TODO()
        is CountLeadingZeroBits64 -> TODO()
        is CountSetBits32 -> TODO()
        is CountSetBits64 -> TODO()
        is CountTrailingZeroBits32 -> TODO()
        is CountTrailingZeroBits64 -> TODO()
        is Maximum -> TODO()
        is MaximumUnsigned -> TODO()
        Memset -> TODO()
        is Minimum -> TODO()
        is MinimumUnsigned -> TODO()
        is OrInverted -> TODO()
        is ReverseByte -> TODO()
        is RotateLeft32 -> TODO()
        is RotateLeft64 -> TODO()
        is RotateRight32 -> TODO()
        is RotateRight64 -> TODO()
        is RotateRightImm32 -> TODO()
        is RotateRightImm64 -> TODO()
        is RotateRightImmAlt32 -> TODO()
        is RotateRightImmAlt64 -> TODO()
        is SignExtend16 -> TODO()
        is SignExtend8 -> TODO()
        is Xnor -> TODO()
        is ZeroExtend16 -> TODO()
    }

    /**
     * Serializes the instruction into a byte buffer
     */
    fun serializeInto(position: UInt, buffer: ByteArray): UInt {
        return when (this) {
            is Panic -> serializeArgless(buffer, Opcode.panic)
            is Fallthrough -> serializeArgless(buffer, Opcode.fallthrough)
            is JumpIndirect -> serializeRegImm(buffer, Opcode.jump_indirect, reg, imm)
            is LoadImm -> serializeRegImm(buffer, Opcode.load_imm, reg, imm)
            is LoadU8 -> serializeRegImm(buffer, Opcode.load_u8, reg, imm)
            is LoadI8 -> serializeRegImm(buffer, Opcode.load_i8, reg, imm)
            is LoadU16 -> serializeRegImm(buffer, Opcode.load_u16, reg, imm)
            is LoadI16 -> serializeRegImm(buffer, Opcode.load_i16, reg, imm)
            is LoadU32 -> serializeRegImm(buffer, Opcode.load_u32, reg, imm)
            is LoadI32 -> serializeRegImm(buffer, Opcode.load_i32, reg, imm)
            is LoadU64 -> serializeRegImm(buffer, Opcode.load_u64, reg, imm)
            is StoreU8 -> serializeRegImm(buffer, Opcode.store_u8, reg, imm)
            is StoreU16 -> serializeRegImm(buffer, Opcode.store_u16, reg, imm)
            is StoreU32 -> serializeRegImm(buffer, Opcode.store_u32, reg, imm)
            is StoreU64 -> serializeRegImm(buffer, Opcode.store_u64, reg, imm)
            is LoadImmAndJump -> serializeRegImmOffset(buffer, position, Opcode.load_imm_and_jump, reg, imm1, imm2)
            is BranchEqImm -> serializeRegImmOffset(buffer, position, Opcode.branch_eq_imm, reg, imm1, imm2)
            is BranchNotEqImm -> serializeRegImmOffset(buffer, position, Opcode.branch_not_eq_imm, reg, imm1, imm2)
            is BranchLessUnsignedImm -> serializeRegImmOffset(
                buffer,
                position,
                Opcode.branch_less_unsigned_imm,
                reg,
                imm1,
                imm2
            )

            is BranchLessSignedImm -> serializeRegImmOffset(
                buffer,
                position,
                Opcode.branch_less_signed_imm,
                reg,
                imm1,
                imm2
            )

            is BranchGreaterOrEqualUnsignedImm -> serializeRegImmOffset(
                buffer,
                position,
                Opcode.branch_greater_or_equal_unsigned_imm,
                reg,
                imm1,
                imm2
            )

            is BranchGreaterOrEqualSignedImm -> serializeRegImmOffset(
                buffer,
                position,
                Opcode.branch_greater_or_equal_signed_imm,
                reg,
                imm1,
                imm2
            )

            is BranchLessOrEqualSignedImm -> serializeRegImmOffset(
                buffer,
                position,
                Opcode.branch_less_or_equal_signed_imm,
                reg,
                imm1,
                imm2
            )

            is BranchLessOrEqualUnsignedImm -> serializeRegImmOffset(
                buffer,
                position,
                Opcode.branch_less_or_equal_unsigned_imm,
                reg,
                imm1,
                imm2
            )

            is BranchGreaterSignedImm -> serializeRegImmOffset(
                buffer,
                position,
                Opcode.branch_greater_signed_imm,
                reg,
                imm1,
                imm2
            )

            is BranchGreaterUnsignedImm -> serializeRegImmOffset(
                buffer,
                position,
                Opcode.branch_greater_unsigned_imm,
                reg,
                imm1,
                imm2
            )

            is StoreImmIndirectU8 -> serializeRegImmImm(buffer, Opcode.store_imm_indirect_u8, reg, imm1, imm2)
            is StoreImmIndirectU16 -> serializeRegImmImm(
                buffer,
                Opcode.store_imm_indirect_u16,
                reg,
                imm1,
                imm2
            )

            is StoreImmIndirectU32 -> serializeRegImmImm(
                buffer,
                Opcode.store_imm_indirect_u32,
                reg,
                imm1,
                imm2
            )

            is StoreImmIndirectU64 -> serializeRegImmImm(
                buffer,
                Opcode.store_imm_indirect_u64,
                reg,
                imm1,
                imm2
            )

            is StoreIndirectU8 -> serializeRegRegImm(buffer, Opcode.store_indirect_u8, reg1, reg2, imm)
            is StoreIndirectU16 -> serializeRegRegImm(buffer, Opcode.store_indirect_u16, reg1, reg2, imm)
            is StoreIndirectU32 -> serializeRegRegImm(buffer, Opcode.store_indirect_u32, reg1, reg2, imm)
            is StoreIndirectU64 -> serializeRegRegImm(buffer, Opcode.store_indirect_u64, reg1, reg2, imm)
            is LoadIndirectU8 -> serializeRegRegImm(buffer, Opcode.load_indirect_u8, reg1, reg2, imm)
            is LoadIndirectI8 -> serializeRegRegImm(buffer, Opcode.load_indirect_i8, reg1, reg2, imm)
            is LoadIndirectU16 -> serializeRegRegImm(buffer, Opcode.load_indirect_u16, reg1, reg2, imm)
            is LoadIndirectI16 -> serializeRegRegImm(buffer, Opcode.load_indirect_i16, reg1, reg2, imm)
            is LoadIndirectI32 -> serializeRegRegImm(buffer, Opcode.load_indirect_i32, reg1, reg2, imm)
            is LoadIndirectU32 -> serializeRegRegImm(buffer, Opcode.load_indirect_u32, reg1, reg2, imm)
            is LoadIndirectU64 -> serializeRegRegImm(buffer, Opcode.load_indirect_u64, reg1, reg2, imm)
            is AddImm32 -> serializeRegRegImm(buffer, Opcode.add_imm_32, reg1, reg2, imm)
            is AddImm64 -> serializeRegRegImm(buffer, Opcode.add_imm_64, reg1, reg2, imm)
            is AndImm -> serializeRegRegImm(buffer, Opcode.and_imm, reg1, reg2, imm)
            is XorImm -> serializeRegRegImm(buffer, Opcode.xor_imm, reg1, reg2, imm)
            is OrImm -> serializeRegRegImm(buffer, Opcode.or_imm, reg1, reg2, imm)
            is MulImm32 -> serializeRegRegImm(buffer, Opcode.mul_imm_32, reg1, reg2, imm)
            is MulImm64 -> serializeRegRegImm(buffer, Opcode.mul_imm_64, reg1, reg2, imm)
            is SetLessThanUnsignedImm -> serializeRegRegImm(buffer, Opcode.set_less_than_unsigned_imm, reg1, reg2, imm)
            is SetLessThanSignedImm -> serializeRegRegImm(buffer, Opcode.set_less_than_signed_imm, reg1, reg2, imm)
            is ShiftLogicalLeftImm32 -> serializeRegRegImm(buffer, Opcode.shift_logical_left_imm_32, reg1, reg2, imm)
            is ShiftLogicalLeftImm64 -> serializeRegRegImm(buffer, Opcode.shift_logical_left_imm_64, reg1, reg2, imm)
            is ShiftLogicalRightImm32 -> serializeRegRegImm(buffer, Opcode.shift_logical_right_imm_32, reg1, reg2, imm)
            is ShiftLogicalRightImm64 -> serializeRegRegImm(buffer, Opcode.shift_logical_right_imm_64, reg1, reg2, imm)
            is NegateAndAddImm32 -> serializeRegRegImm(buffer, Opcode.negate_and_add_imm_32, reg1, reg2, imm)
            is NegateAndAddImm64 -> serializeRegRegImm(buffer, Opcode.negate_and_add_imm_64, reg1, reg2, imm)
            is SetGreaterThanUnsignedImm -> serializeRegRegImm(
                buffer,
                Opcode.set_greater_than_unsigned_imm,
                reg1,
                reg2,
                imm
            )

            is SetGreaterThanSignedImm -> serializeRegRegImm(
                buffer,
                Opcode.set_greater_than_signed_imm,
                reg1,
                reg2,
                imm
            )

            is ShiftLogicalRightImmAlt32 -> serializeRegRegImm(
                buffer,
                Opcode.shift_logical_right_imm_alt_32,
                reg1,
                reg2,
                imm
            )

            is ShiftLogicalRightImmAlt64 -> serializeRegRegImm(
                buffer,
                Opcode.shift_logical_right_imm_alt_64,
                reg1,
                reg2,
                imm
            )

            is ShiftArithmeticRightImmAlt32 -> serializeRegRegImm(
                buffer,
                Opcode.shift_arithmetic_right_imm_alt_32,
                reg1,
                reg2,
                imm
            )

            is ShiftArithmeticRightImmAlt64 -> serializeRegRegImm(
                buffer,
                Opcode.shift_arithmetic_right_imm_alt_64,
                reg1,
                reg2,
                imm
            )

            is ShiftLogicalLeftImmAlt32 -> serializeRegRegImm(
                buffer,
                Opcode.shift_logical_left_imm_alt_32,
                reg1,
                reg2,
                imm
            )

            is ShiftLogicalLeftImmAlt64 -> serializeRegRegImm(
                buffer,
                Opcode.shift_logical_left_imm_alt_64,
                reg1,
                reg2,
                imm
            )

            is CmovIfZeroImm -> serializeRegRegImm(buffer, Opcode.cmov_if_zero_imm, reg1, reg2, imm)
            is CmovIfNotZeroImm -> serializeRegRegImm(buffer, Opcode.cmov_if_not_zero_imm, reg1, reg2, imm)
            is BranchEq -> serializeRegRegOffset(buffer, position, Opcode.branch_eq, reg1, reg2, imm)
            is BranchNotEq -> serializeRegRegOffset(buffer, position, Opcode.branch_not_eq, reg1, reg2, imm)
            is BranchLessUnsigned -> serializeRegRegOffset(
                buffer,
                position,
                Opcode.branch_less_unsigned,
                reg1,
                reg2,
                imm
            )

            is BranchLessSigned -> serializeRegRegOffset(buffer, position, Opcode.branch_less_signed, reg1, reg2, imm)
            is BranchGreaterOrEqualUnsigned -> serializeRegRegOffset(
                buffer,
                position,
                Opcode.branch_greater_or_equal_unsigned,
                reg1,
                reg2,
                imm
            )

            is BranchGreaterOrEqualSigned -> serializeRegRegOffset(
                buffer,
                position,
                Opcode.branch_greater_or_equal_signed,
                reg1,
                reg2,
                imm
            )

            is Add32 -> serializeRegRegReg(buffer, Opcode.add_32, reg1, reg2, reg3)
            is Add64 -> serializeRegRegReg(buffer, Opcode.add_64, reg1, reg2, reg3)
            is Sub32 -> serializeRegRegReg(buffer, Opcode.sub_32, reg1, reg2, reg3)
            is Sub64 -> serializeRegRegReg(buffer, Opcode.sub_64, reg1, reg2, reg3)
            is And -> serializeRegRegReg(buffer, Opcode.and, reg1, reg2, reg3)
            is Xor -> serializeRegRegReg(buffer, Opcode.xor, reg1, reg2, reg3)
            is Or -> serializeRegRegReg(buffer, Opcode.or, reg1, reg2, reg3)
            is Mul32 -> serializeRegRegReg(buffer, Opcode.mul_32, reg1, reg2, reg3)
            is Mul64 -> serializeRegRegReg(buffer, Opcode.mul_64, reg1, reg2, reg3)
            is MulUpperSignedSigned -> serializeRegRegReg(buffer, Opcode.mul_upper_signed_signed, reg1, reg2, reg3)
            is MulUpperUnsignedUnsigned -> serializeRegRegReg(
                buffer,
                Opcode.mul_upper_unsigned_unsigned,
                reg1,
                reg2,
                reg3
            )

            is MulUpperSignedUnsigned -> serializeRegRegReg(
                buffer,
                Opcode.mul_upper_signed_unsigned,
                reg1,
                reg2,
                reg3
            )

            is SetLessThanUnsigned -> serializeRegRegReg(buffer, Opcode.set_less_than_unsigned, reg1, reg2, reg3)
            is SetLessThanSigned -> serializeRegRegReg(buffer, Opcode.set_less_than_signed, reg1, reg2, reg3)
            is ShiftLogicalLeft32 -> serializeRegRegReg(buffer, Opcode.shift_logical_left_32, reg1, reg2, reg3)
            is ShiftLogicalLeft64 -> serializeRegRegReg(buffer, Opcode.shift_logical_left_64, reg1, reg2, reg3)
            is ShiftLogicalRight32 -> serializeRegRegReg(buffer, Opcode.shift_logical_right_32, reg1, reg2, reg3)
            is ShiftLogicalRight64 -> serializeRegRegReg(buffer, Opcode.shift_logical_right_64, reg1, reg2, reg3)
            is ShiftArithmeticRight32 -> serializeRegRegReg(buffer, Opcode.shift_arithmetic_right_32, reg1, reg2, reg3)
            is ShiftArithmeticRight64 -> serializeRegRegReg(buffer, Opcode.shift_arithmetic_right_64, reg1, reg2, reg3)
            is DivUnsigned32 -> serializeRegRegReg(buffer, Opcode.div_unsigned_32, reg1, reg2, reg3)
            is DivUnsigned64 -> serializeRegRegReg(buffer, Opcode.div_unsigned_64, reg1, reg2, reg3)
            is DivSigned32 -> serializeRegRegReg(buffer, Opcode.div_signed_32, reg1, reg2, reg3)
            is DivSigned64 -> serializeRegRegReg(buffer, Opcode.div_signed_64, reg1, reg2, reg3)
            is RemUnsigned32 -> serializeRegRegReg(buffer, Opcode.rem_unsigned_32, reg1, reg2, reg3)
            is RemUnsigned64 -> serializeRegRegReg(buffer, Opcode.rem_unsigned_64, reg1, reg2, reg3)
            is RemSigned32 -> serializeRegRegReg(buffer, Opcode.rem_signed_32, reg1, reg2, reg3)
            is RemSigned64 -> serializeRegRegReg(buffer, Opcode.rem_signed_64, reg1, reg2, reg3)
            is CmovIfZero -> serializeRegRegReg(buffer, Opcode.cmov_if_zero, reg1, reg2, reg3)
            is CmovIfNotZero -> serializeRegRegReg(buffer, Opcode.cmov_if_not_zero, reg1, reg2, reg3)

            is Jump -> serializeOffset(buffer, position, Opcode.jump, imm)
            is Ecalli -> serializeImm(buffer, Opcode.ecalli, imm)
            is StoreImmU8 -> serializeImmImm(buffer, Opcode.store_imm_u8, imm1, imm2)
            is StoreImmU16 -> serializeImmImm(buffer, Opcode.store_imm_u16, imm1, imm2)
            is StoreImmU32 -> serializeImmImm(buffer, Opcode.store_imm_u32, imm1, imm2)
            is StoreImmU64 -> serializeImmImm(buffer, Opcode.store_imm_u64, imm1, imm2)
            is MoveReg -> serializeRegReg(buffer, Opcode.move_reg, reg1, reg2)
            is Sbrk -> serializeRegReg(buffer, Opcode.sbrk, reg1, reg2)
            is LoadImmAndJumpIndirect -> serializeRegRegImmImm(
                buffer,
                Opcode.load_imm_and_jump_indirect,
                reg1,
                reg2,
                imm1,
                imm2
            )

            is LoadImm64 -> serializeImm64(buffer, Opcode.load_imm_64, reg, imm)
            is Invalid -> serializeArgless(buffer, Opcode.panic)
            is ShiftArithmeticRightImm32 -> serializeRegRegImm(
                buffer,
                Opcode.shift_arithmetic_right_imm_32,
                reg1,
                reg2,
                imm
            )

            is ShiftArithmeticRightImm64 -> serializeRegRegImm(
                buffer,
                Opcode.shift_arithmetic_right_imm_64,
                reg1,
                reg2,
                imm
            )

            is AndInverted -> TODO()
            is CountLeadingZeroBits32 -> TODO()
            is CountLeadingZeroBits64 -> TODO()
            is CountSetBits32 -> TODO()
            is CountSetBits64 -> TODO()
            is CountTrailingZeroBits32 -> TODO()
            is CountTrailingZeroBits64 -> TODO()
            is Maximum -> TODO()
            is MaximumUnsigned -> TODO()
            is Memset -> TODO()
            is Minimum -> TODO()
            is MinimumUnsigned -> TODO()
            is OrInverted -> TODO()
            is ReverseByte -> TODO()
            is RotateLeft32 -> TODO()
            is RotateLeft64 -> TODO()
            is RotateRight32 -> TODO()
            is RotateRight64 -> TODO()
            is RotateRightImm32 -> TODO()
            is RotateRightImm64 -> TODO()
            is RotateRightImmAlt32 -> TODO()
            is RotateRightImmAlt64 -> TODO()
            is SignExtend16 -> TODO()
            is SignExtend8 -> TODO()
            is Xnor -> TODO()
            is ZeroExtend16 -> TODO()
        }
    }

    companion object {
        /**
         * Serialization helper methods would go here
         */
        private fun serializeArgless(buffer: ByteArray, opcode: Opcode): UInt {
            buffer[0] = opcode.ordinal.toByte()
            return 1u
        }

        private fun serializeOffset(buffer: ByteArray, position: UInt, opcode: Opcode, imm: UInt): UInt {
            val adjustedImm = imm.minus(position)
            buffer[0] = opcode.ordinal.toByte()
            return writeSimpleVarint(adjustedImm, buffer.sliceArray(1 until buffer.size).toUByteArray()) + 1u
        }

        private fun serializeImm(buffer: ByteArray, opcode: Opcode, imm: UInt): UInt {
            buffer[0] = opcode.ordinal.toByte()
            return writeSimpleVarint(imm, buffer.sliceArray(1 until buffer.size).toUByteArray()) + 1u
        }

        private fun serializeImm64(buffer: ByteArray, opcode: Opcode, reg: RawReg, imm: ULong): UInt {
            buffer[0] = opcode.ordinal.toByte()
            buffer[1] = reg.rawUnparsed().toByte()
            return writeSimpleVarint64(imm, buffer.sliceArray(2 until buffer.size).toUByteArray()) + 2u
        }

        private fun serializeImmImm(buffer: ByteArray, opcode: Opcode, imm1: UInt, imm2: UInt): UInt {
            buffer[0] = opcode.ordinal.toByte()
            var position = 2u
            val imm1Length =
                writeSimpleVarint(imm1, buffer.sliceArray(position.toInt() until buffer.size).toUByteArray())
            buffer[1] = imm1Length.toByte()
            position += imm1Length
            position += writeSimpleVarint(imm2, buffer.sliceArray(position.toInt() until buffer.size).toUByteArray())
            return position
        }

        private fun serializeRegReg(buffer: ByteArray, opcode: Opcode, reg1: RawReg, reg2: RawReg): UInt {
            buffer[0] = opcode.ordinal.toByte()
            buffer[1] = (reg1.rawUnparsed() or reg2.rawUnparsed()).toByte()
            return 2u
        }

        private fun serializeRegImm(buffer: ByteArray, opcode: Opcode, reg: RawReg, imm: UInt): UInt {
            buffer[0] = opcode.ordinal.toByte()
            buffer[1] = reg.rawUnparsed().toByte()
            return writeSimpleVarint(imm, buffer.sliceArray(2 until buffer.size).toUByteArray()) + 2u
        }

        private fun serializeRegImmOffset(
            buffer: ByteArray,
            position: UInt,
            opcode: Opcode,
            reg: RawReg,
            imm1: UInt,
            imm2: UInt
        ): UInt {
            val adjustedImm2 = imm2.minus(position)
            buffer[0] = opcode.ordinal.toByte()
            var pos = 2u
            val imm1Length = writeSimpleVarint(imm1, buffer.sliceArray(pos.toInt() until buffer.size).toUByteArray())
            pos += imm1Length
            buffer[1] = (reg.rawUnparsed() or (imm1Length shl 4)).toByte()
            pos += writeSimpleVarint(adjustedImm2, buffer.sliceArray(pos.toInt() until buffer.size).toUByteArray())
            return pos
        }

        private fun serializeRegRegImmImm(
            buffer: ByteArray,
            opcode: Opcode,
            reg1: RawReg,
            reg2: RawReg,
            imm1: UInt,
            imm2: UInt
        ): UInt {
            buffer[0] = opcode.ordinal.toByte()
            buffer[1] = (reg1.rawUnparsed() or (reg2.rawUnparsed() shl 4)).toByte()
            var position = 3u
            val imm1Length =
                writeSimpleVarint(imm1, buffer.sliceArray(position.toInt() until buffer.size).toUByteArray())
            buffer[2] = imm1Length.toByte()
            position += imm1Length
            position += writeSimpleVarint(imm2, buffer.sliceArray(position.toInt() until buffer.size).toUByteArray())
            return position
        }

        private fun serializeRegImmImm(buffer: ByteArray, opcode: Opcode, reg: RawReg, imm1: UInt, imm2: UInt): UInt {
            buffer[0] = opcode.ordinal.toByte()
            var position = 2u
            val imm1Length =
                writeSimpleVarint(imm1, buffer.sliceArray(position.toInt() until buffer.size).toUByteArray())
            position += imm1Length
            buffer[1] = (reg.rawUnparsed() or (imm1Length shl 4)).toByte()
            position += writeSimpleVarint(imm2, buffer.sliceArray(position.toInt() until buffer.size).toUByteArray())
            return position
        }

        private fun serializeRegRegImm(buffer: ByteArray, opcode: Opcode, reg1: RawReg, reg2: RawReg, imm: UInt): UInt {
            buffer[0] = opcode.ordinal.toByte()
            buffer[1] = (reg1.rawUnparsed() or (reg2.rawUnparsed() shl 4)).toByte()
            return writeSimpleVarint(imm, buffer.sliceArray(2 until buffer.size).toUByteArray()) + 2u
        }

        private fun serializeRegRegOffset(
            buffer: ByteArray,
            position: UInt,
            opcode: Opcode,
            reg1: RawReg,
            reg2: RawReg,
            imm: UInt
        ): UInt {
            val adjustedImm = imm.minus(position)
            buffer[0] = opcode.ordinal.toByte()
            buffer[1] = (reg1.rawUnparsed() or (reg2.rawUnparsed() shl 4)).toByte()
            return writeSimpleVarint(adjustedImm, buffer.sliceArray(2 until buffer.size).toUByteArray()) + 2u
        }

        private fun serializeRegRegReg(
            buffer: ByteArray,
            opcode: Opcode,
            reg1: RawReg,
            reg2: RawReg,
            reg3: RawReg
        ): UInt {
            buffer[0] = opcode.ordinal.toByte()
            buffer[1] = (reg2.rawUnparsed() or (reg3.rawUnparsed() shl 4)).toByte()
            buffer[2] = reg1.rawUnparsed().toByte()
            return 3u
        }
    }
}
