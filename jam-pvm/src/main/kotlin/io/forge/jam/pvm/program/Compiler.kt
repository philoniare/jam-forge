package io.forge.jam.pvm.program

import io.forge.jam.pvm.Handler
import io.forge.jam.pvm.PvmLogger
import io.forge.jam.pvm.RawHandlers
import io.forge.jam.pvm.Target
import io.forge.jam.pvm.engine.*

class Compiler(
    private val programCounter: ProgramCounter,
    private var nextProgramCounter: ProgramCounter,
    private val compiledHandlers: MutableList<Handler>,
    private val compiledArgs: MutableList<Args>,
    private val module: Module,
    private val instance: InterpretedInstance
) : InstructionVisitor<Unit> {

    companion object {
        private val logger = PvmLogger(Compiler::class.java)
        const val TARGET_OUT_OF_RANGE = 0u
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

        fun notEnoughGasImpl(visitor: Visitor, programCounter: ProgramCounter, newGas: Long): Target? {
            with(visitor.inner) {
                gas = newGas
                when (module.gasMetering()) {
                    GasMeteringKind.Sync -> {
                        this.programCounter = programCounter
                        programCounterValid = true
                        nextProgramCounter = programCounter
                        nextProgramCounterChanged = false
                    }

                    null -> TODO()
                }
                interrupt = InterruptKind.NotEnoughGas
            }
            return null
        }
    }

    fun nextProgramCounter(): ProgramCounter = nextProgramCounter

    override fun invalid() {
        trap()
    }

    override fun trap() {
        instance.emit(
            RawHandlers.trap,
            Args.trap(programCounter), "trap"
        )
    }

    override fun fallthrough() {
        TODO("Not yet implemented")
    }

    override fun jumpIndirect(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadImm(reg: RawReg, imm: UInt) {
        instance.emit(
            RawHandlers.loadImm,
            Args.loadImm(reg, imm), "loadImm"
        )
    }

    override fun loadU8(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadI8(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadU16(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadI16(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadU32(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadI32(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadU64(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeU8(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeU16(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeU32(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeU64(reg: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadImmAndJump(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchEq(s1: RawReg, s2: RawReg, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchEq,
                Args.unresolvedBranchEq(s1, s2, targetTrue, targetFalse), "unresolvedBranchEq"
            )
        }
    }

    override fun branchEqImm(s1: RawReg, s2: UInt, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchEqImm,
                Args.unresolvedBranchEqImm(s1, s2, targetTrue, targetFalse), "unresolvedBranchEqImm"
            )
        }
    }

    override fun branchNotEqImm(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchLessUnsignedImm(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchLessSignedImm(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchGreaterOrEqualUnsignedImm(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchGreaterOrEqualSignedImm(s1: RawReg, s2: UInt, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchGreaterOrEqualSignedImm,
                Args.unresolvedBranchGreaterOrEqualSignedImm(s1, s2, targetTrue, targetFalse), "unresolvedBranchEqImm"
            )
        }
    }

    override fun branchLessOrEqualSignedImm(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchLessOrEqualUnsignedImm(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchGreaterSignedImm(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchGreaterUnsignedImm(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeImmIndirectU8(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeImmIndirectU16(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeImmIndirectU32(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeImmIndirectU64(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeIndirectU8(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeIndirectU16(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeIndirectU32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeIndirectU64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadIndirectU8(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadIndirectI8(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadIndirectU16(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadIndirectI16(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadIndirectU32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadIndirectI32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadIndirectU64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun addImm32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        instance.emit(
            RawHandlers.addImm32,
            Args.addImm32(reg1, reg2, imm), "addImm32"
        )
    }

    override fun addImm64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun andImm(reg1: RawReg, reg2: RawReg, imm: UInt) {
        instance.emit(
            RawHandlers.andImm,
            Args.andImm(reg1, reg2, imm), "andImm"
        )
    }

    override fun xorImm(reg1: RawReg, reg2: RawReg, imm: UInt) {
        instance.emit(
            RawHandlers.xorImm,
            Args.xorImm(reg1, reg2, imm), "xorImm"
        )
    }

    override fun orImm(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun mulImm32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun mulImm64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun setLessThanUnsignedImm(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun setLessThanSignedImm(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalLeftImm32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalLeftImm64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalRightImm32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalRightImm64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftArithmeticRightImm32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftArithmeticRightImm64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun negateAndAddImm32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun negateAndAddImm64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun setGreaterThanUnsignedImm(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun setGreaterThanSignedImm(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalRightImmAlt32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalRightImmAlt64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftArithmeticRightImmAlt32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftArithmeticRightImmAlt64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalLeftImmAlt32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalLeftImmAlt64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun cmovIfZeroImm(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun cmovIfNotZeroImm(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchNotEq(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchLessUnsigned(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchLessSigned(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchGreaterOrEqualUnsigned(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchGreaterOrEqualSigned(s1: RawReg, s2: RawReg, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchGreaterOrEqualSigned,
                Args.unresolvedBranchGreaterOrEqualSigned(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchGreaterOrEqualSigned"
            )
        }
    }

    override fun add32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.add32,
            Args.add32(reg1, reg2, reg3), "add32"
        )
    }

    override fun add64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun sub32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.sub32,
            Args.sub32(reg1, reg2, reg3), "sub32"
        )
    }

    override fun sub64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun and(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.and,
            Args.and(reg1, reg2, reg3), "and"
        )
    }

    override fun xor(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.xor,
            Args.xor(reg1, reg2, reg3), "xor"
        )
    }

    override fun or(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun mul32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun mul64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun mulUpperSignedSigned(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun mulUpperUnsignedUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun mulUpperSignedUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun setLessThanUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun setLessThanSigned(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalLeft32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalLeft64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalRight32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun shiftLogicalRight64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun shiftArithmeticRight32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun shiftArithmeticRight64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun divUnsigned32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun divUnsigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun divSigned32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun divSigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun remUnsigned32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun remUnsigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun remSigned32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun remSigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun cmovIfZero(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun cmovIfNotZero(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        TODO("Not yet implemented")
    }

    override fun jump(imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun ecalli(imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeImmU8(imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeImmU16(imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeImmU32(imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeImmU64(imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun moveReg(reg1: RawReg, reg2: RawReg) {
        instance.emit(
            RawHandlers.moveReg,
            Args.moveReg(reg1, reg2), "move_reg"
        )
    }

    override fun sbrk(reg1: RawReg, reg2: RawReg) {
        TODO("Not yet implemented")
    }

    override fun loadImmAndJumpIndirect(reg1: RawReg, reg2: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
    }

    override fun loadImm64(reg: RawReg, imm: ULong) {
        TODO("Not yet implemented")
    }


}
