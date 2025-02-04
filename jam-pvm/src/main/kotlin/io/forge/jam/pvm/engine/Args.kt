package io.forge.jam.pvm.engine

import io.forge.jam.pvm.program.ProgramCounter
import io.forge.jam.pvm.program.RawReg
import io.forge.jam.pvm.program.toU32

/**
 * Arguments structure for VM instructions.
 */
data class Args(
    val a0: UInt = 0u,
    val a1: UInt = 0u,
    val a2: UInt = 0u,
    val a3: UInt = 0u,
    val a4: UInt = 0u
) {
    companion object {
        /**
         * Creates a default Args instance with all values set to 0
         */
        fun default(): Args = Args()

        fun panic(programCounter: ProgramCounter) = Args(a0 = programCounter.value)

        fun step(programCounter: ProgramCounter) = Args(a0 = programCounter.value)

        fun chargeGas(programCounter: ProgramCounter, gasCost: UInt) = Args(
            a0 = programCounter.value,
            a1 = gasCost
        )

        fun stepOutOfRange() = Args()
        fun outOfRange(gasCost: UInt) = Args(a0 = gasCost)

        fun moveReg(d: RawReg, s: RawReg) = Args(a0 = d.toU32(), a1 = s.toU32())

        fun add32(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun countLeadingZeroBits32(d: RawReg, s: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s.toU32()
        )

        fun countLeadingZeroBits64(d: RawReg, s: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s.toU32()
        )

        fun add64(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun addImm32(a0: RawReg, a1: RawReg, imm: UInt) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = imm
        )

        fun addImm64(a0: RawReg, a1: RawReg, imm: UInt) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = imm
        )

        fun and(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun andImm(a0: RawReg, a1: RawReg, imm: UInt) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = imm
        )

        fun loadImm(a0: RawReg, imm: UInt) = Args(a0 = a0.toU32(), a1 = imm)

        fun branchEqImm(a0: RawReg, a1: UInt, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2,
            a3 = a3
        )

        fun branchEq(a0: RawReg, a1: RawReg, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchEqImm(a0: RawReg, a1: UInt, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2.value,
            a3 = a3.value
        )

        fun unresolvedBranchEq(a0: RawReg, a1: RawReg, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.value,
            a3 = a3.value
        )

        fun invalidBranch(a0: ProgramCounter) = Args(a0 = a0.value)

        fun xorImm(a0: RawReg, a1: RawReg, imm: UInt) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = imm
        )

        fun xor(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun sub32(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun sub64(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun branchGreaterOrEqualSignedImm(a0: RawReg, a1: UInt, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchGreaterOrEqualSignedImm(a0: RawReg, a1: UInt, a2: ProgramCounter, a3: ProgramCounter) =
            Args(
                a0 = a0.toU32(),
                a1 = a1,
                a2 = a2.value,
                a3 = a3.value
            )

        fun branchGreaterOrEqualSigned(a0: RawReg, a1: RawReg, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchGreaterOrEqualSigned(a0: RawReg, a1: RawReg, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.value,
            a3 = a3.value
        )

        fun branchGreaterOrEqualUnsignedImm(a0: RawReg, a1: UInt, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchGreaterOrEqualUnsignedImm(a0: RawReg, a1: UInt, a2: ProgramCounter, a3: ProgramCounter) =
            Args(
                a0 = a0.toU32(),
                a1 = a1,
                a2 = a2.value,
                a3 = a3.value
            )

        fun branchGreaterOrEqualUnsigned(a0: RawReg, a1: RawReg, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchGreaterOrEqualUnsigned(a0: RawReg, a1: RawReg, a2: ProgramCounter, a3: ProgramCounter) =
            Args(
                a0 = a0.toU32(),
                a1 = a1.toU32(),
                a2 = a2.value,
                a3 = a3.value
            )

        fun branchGreaterSignedImm(a0: RawReg, a1: UInt, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchGreaterSignedImm(a0: RawReg, a1: UInt, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2.value,
            a3 = a3.value
        )

        fun branchGreaterUnsignedImm(a0: RawReg, a1: UInt, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchGreaterUnsignedImm(a0: RawReg, a1: UInt, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2.value,
            a3 = a3.value
        )

        fun branchLessOrEqualUnsignedImm(a0: RawReg, a1: UInt, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchLessOrEqualUnsignedImm(a0: RawReg, a1: UInt, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2.value,
            a3 = a3.value
        )

        fun branchLessOrEqualSignedImm(a0: RawReg, a1: UInt, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchLessOrEqualSignedImm(a0: RawReg, a1: UInt, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2.value,
            a3 = a3.value
        )

        fun branchLessSigned(a0: RawReg, a1: RawReg, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchLessSigned(a0: RawReg, a1: RawReg, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.value,
            a3 = a3.value
        )

        fun branchLessSignedImm(a0: RawReg, a1: UInt, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchLessSignedImm(a0: RawReg, a1: UInt, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2.value,
            a3 = a3.value
        )

        fun branchLessUnsigned(a0: RawReg, a1: RawReg, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchLessUnsigned(a0: RawReg, a1: RawReg, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.value,
            a3 = a3.value
        )

        fun branchLessUnsignedImm(a0: RawReg, a1: UInt, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchLessUnsignedImm(a0: RawReg, a1: UInt, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2.value,
            a3 = a3.value
        )

        fun branchNotEq(a0: RawReg, a1: RawReg, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchNotEq(a0: RawReg, a1: RawReg, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.value,
            a3 = a3.value
        )

        fun branchNotEqImm(a0: RawReg, a1: UInt, a2: Target, a3: Target) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2,
            a3 = a3
        )

        fun unresolvedBranchNotEqImm(a0: RawReg, a1: UInt, a2: ProgramCounter, a3: ProgramCounter) = Args(
            a0 = a0.toU32(),
            a1 = a1,
            a2 = a2.value,
            a3 = a3.value
        )

        fun cmovIfZeroImm(a0: RawReg, a1: RawReg, a2: UInt) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2
        )

        fun cmovIfZero(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun divSigned32(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun divSigned64(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun divUnsigned32(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun divUnsigned64(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun loadI8Basic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadI8Dynamic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun mul32(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun mul64(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun mulImm32(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun mulImm64(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun mulUpperSignedSigned32(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun mulUpperSignedSigned64(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun mulUpperUnsignedUnsigned32(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun mulUpperUnsignedUnsigned64(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun mulUpperSignedUnsigned32(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun mulUpperSignedUnsigned64(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun orImm(a0: RawReg, a1: RawReg, imm: UInt) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = imm
        )

        fun or(a0: RawReg, a1: RawReg, a2: RawReg) = Args(
            a0 = a0.toU32(),
            a1 = a1.toU32(),
            a2 = a2.toU32()
        )

        fun negateAndAddImm32(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun negateAndAddImm64(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun remUnsigned32(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun remUnsigned64(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun remSigned32(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun remSigned64(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun jumpIndirect(programCounter: ProgramCounter, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = base.toU32(),
            a2 = offset
        )

        fun loadImmAndJumpIndirect(
            programCounter: ProgramCounter,
            ra: RawReg,
            base: RawReg,
            value: UInt,
            offset: UInt
        ) = Args(
            a0 = programCounter.value,
            a1 = ra.toU32(),
            a2 = base.toU32(),
            a3 = value,
            a4 = offset
        )

        fun setLessThanUnsignedImm(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun setGreaterThanUnsignedImm(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun setLessThanSignedImm(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun setGreaterThanSignedImm(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun setLessThanUnsigned(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun setLessThanSigned(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun shiftLogicalRight32(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun shiftLogicalRight64(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun shiftArithmeticRight32(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun shiftArithmeticRight64(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun shiftLogicalLeft32(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun shiftLogicalLeft64(d: RawReg, s1: RawReg, s2: RawReg) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2.toU32()
        )

        fun shiftLogicalRightImm32(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun shiftLogicalRightImm64(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun shiftLogicalRightImmAlt32(d: RawReg, s2: RawReg, s1: UInt) = Args(
            a0 = d.toU32(),
            a1 = s2.toU32(),
            a2 = s1
        )

        fun shiftLogicalRightImmAlt64(d: RawReg, s2: RawReg, s1: UInt) = Args(
            a0 = d.toU32(),
            a1 = s2.toU32(),
            a2 = s1
        )

        fun shiftArithmeticRightImm32(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun shiftArithmeticRightImm64(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun shiftArithmeticRightImmAlt32(d: RawReg, s2: RawReg, s1: UInt) = Args(
            a0 = d.toU32(),
            a1 = s2.toU32(),
            a2 = s1
        )

        fun shiftArithmeticRightImmAlt64(d: RawReg, s2: RawReg, s1: UInt) = Args(
            a0 = d.toU32(),
            a1 = s2.toU32(),
            a2 = s1
        )

        fun shiftLogicalLeftImm32(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun shiftLogicalLeftImm64(d: RawReg, s1: RawReg, s2: UInt) = Args(
            a0 = d.toU32(),
            a1 = s1.toU32(),
            a2 = s2
        )

        fun shiftLogicalLeftImmAlt32(d: RawReg, s2: RawReg, s1: UInt) = Args(
            a0 = d.toU32(),
            a1 = s2.toU32(),
            a2 = s1
        )

        fun shiftLogicalLeftImmAlt64(d: RawReg, s2: RawReg, s1: UInt) = Args(
            a0 = d.toU32(),
            a1 = s2.toU32(),
            a2 = s1
        )

        fun jump(target: Target) = Args(
            a0 = target
        )

        fun fallthrough() = Args()

        fun unresolvedJump(programCounter: ProgramCounter, jumpTo: ProgramCounter) = Args(
            a0 = programCounter.value,
            a1 = jumpTo.value
        )

        fun unresolvedFallthrough(jumpTo: ProgramCounter) = Args(
            a0 = jumpTo.value
        )

        fun loadI16Basic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadI16Dynamic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadIndirectI8Basic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectI8Dynamic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectU16Basic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectU16Dynamic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectI16Basic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectI16Dynamic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectU8Basic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectU8Dynamic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectU32Basic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectU32Dynamic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectU64Basic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectU64Dynamic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadU8Basic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadU8Dynamic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadU16Basic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadU16Dynamic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadU32Basic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadU32Dynamic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadImm64(dst: RawReg, immLo: UInt, immHi: UInt) = Args(
            a0 = dst.toU32(),
            a1 = immLo,
            a2 = immHi
        )

        fun loadI32Basic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadI32Dynamic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadU64Basic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadU64Dynamic(programCounter: ProgramCounter, dst: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = offset
        )

        fun loadIndirectI32Basic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun loadIndirectI32Dynamic(programCounter: ProgramCounter, dst: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = dst.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun storeImmU8Basic(programCounter: ProgramCounter, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = offset,
            a2 = value
        )

        fun storeImmU8Dynamic(programCounter: ProgramCounter, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = offset,
            a2 = value
        )

        fun storeU16Basic(programCounter: ProgramCounter, src: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = offset
        )

        fun storeU16Dynamic(programCounter: ProgramCounter, src: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = offset
        )

        fun storeU32Basic(programCounter: ProgramCounter, src: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = offset
        )

        fun storeU32Dynamic(programCounter: ProgramCounter, src: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = offset
        )

        fun storeU64Basic(programCounter: ProgramCounter, src: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = offset
        )

        fun storeU64Dynamic(programCounter: ProgramCounter, src: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = offset
        )

        fun storeImmU16Basic(programCounter: ProgramCounter, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = offset,
            a2 = value
        )

        fun storeImmU16Dynamic(programCounter: ProgramCounter, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = offset,
            a2 = value
        )

        fun storeImmU32Basic(programCounter: ProgramCounter, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = offset,
            a2 = value
        )

        fun storeImmU32Dynamic(programCounter: ProgramCounter, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = offset,
            a2 = value
        )

        fun storeImmU64Basic(programCounter: ProgramCounter, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = offset,
            a2 = value
        )

        fun storeImmU64Dynamic(programCounter: ProgramCounter, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = offset,
            a2 = value
        )

        fun storeU8Basic(programCounter: ProgramCounter, src: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = offset
        )

        fun storeU8Dynamic(programCounter: ProgramCounter, src: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = offset
        )

        fun storeImmIndirectU8Basic(programCounter: ProgramCounter, base: RawReg, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = base.toU32(),
            a2 = offset,
            a3 = value
        )

        fun storeImmIndirectU8Dynamic(programCounter: ProgramCounter, base: RawReg, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = base.toU32(),
            a2 = offset,
            a3 = value
        )

        fun storeImmIndirectU16Basic(programCounter: ProgramCounter, base: RawReg, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = base.toU32(),
            a2 = offset,
            a3 = value
        )

        fun storeImmIndirectU16Dynamic(programCounter: ProgramCounter, base: RawReg, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = base.toU32(),
            a2 = offset,
            a3 = value
        )

        fun storeImmIndirectU32Basic(programCounter: ProgramCounter, base: RawReg, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = base.toU32(),
            a2 = offset,
            a3 = value
        )

        fun storeImmIndirectU32Dynamic(programCounter: ProgramCounter, base: RawReg, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = base.toU32(),
            a2 = offset,
            a3 = value
        )

        fun storeImmIndirectU64Basic(programCounter: ProgramCounter, base: RawReg, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = base.toU32(),
            a2 = offset,
            a3 = value
        )

        fun storeImmIndirectU64Dynamic(programCounter: ProgramCounter, base: RawReg, offset: UInt, value: UInt) = Args(
            a0 = programCounter.value,
            a1 = base.toU32(),
            a2 = offset,
            a3 = value
        )

        fun storeIndirectU8Basic(programCounter: ProgramCounter, src: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun storeIndirectU8Dynamic(programCounter: ProgramCounter, src: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun storeIndirectU16Basic(programCounter: ProgramCounter, src: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun storeIndirectU16Dynamic(programCounter: ProgramCounter, src: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun storeIndirectU32Basic(programCounter: ProgramCounter, src: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun storeIndirectU32Dynamic(programCounter: ProgramCounter, src: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun storeIndirectU64Basic(programCounter: ProgramCounter, src: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )

        fun storeIndirectU64Dynamic(programCounter: ProgramCounter, src: RawReg, base: RawReg, offset: UInt) = Args(
            a0 = programCounter.value,
            a1 = src.toU32(),
            a2 = base.toU32(),
            a3 = offset
        )
    }
}
