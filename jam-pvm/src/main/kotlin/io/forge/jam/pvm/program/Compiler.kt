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
        instance.emit(
            RawHandlers.jumpIndirect,
            Args.jumpIndirect(programCounter, reg, imm), "jumpIndirect"
        )
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
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadI8Basic,
                Args.loadI8Basic(programCounter, reg, imm), "loadI8Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadI8Dynamic,
                Args.loadI8Dynamic(programCounter, reg, imm), "loadI8Dynamic"
            )
        }
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

    override fun branchNotEqImm(s1: RawReg, s2: UInt, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchNotEqImm,
                Args.unresolvedBranchNotEqImm(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchNotEqImm"
            )
        }
    }

    override fun branchLessUnsignedImm(s1: RawReg, s2: UInt, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchLessUnsignedImm,
                Args.unresolvedBranchLessUnsignedImm(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchLessUnsignedImm"
            )
        }
    }

    override fun branchLessSignedImm(s1: RawReg, s2: UInt, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchLessSignedImm,
                Args.unresolvedBranchLessSignedImm(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchLessSignedImm"
            )
        }
    }

    override fun branchGreaterOrEqualUnsignedImm(s1: RawReg, s2: UInt, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchGreaterOrEqualUnsignedImm,
                Args.unresolvedBranchGreaterOrEqualUnsignedImm(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchGreaterOrEqualUnsignedImm"
            )
        }
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

    override fun branchLessOrEqualSignedImm(s1: RawReg, s2: UInt, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchLessOrEqualSignedImm,
                Args.unresolvedBranchLessOrEqualSignedImm(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchLessOrEqualSignedImm"
            )
        }
    }

    override fun branchLessOrEqualUnsignedImm(reg: RawReg, imm1: UInt, imm2: UInt) {
        TODO("Not yet implemented")
//        val targetTrue = ProgramCounter(i)
//        if (!module.isJumpTargetValid(targetTrue)) {
//            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
//        } else {
//            val targetFalse = nextProgramCounter
//            instance.emit(
//                RawHandlers.unresolvedBranchLessOrEqualUnsignedImm,
//                Args.unresolvedBranchLessOrEqualUnsignedImm(s1, s2, targetTrue, targetFalse),
//                "unresolvedBranchLessOrEqualUnsignedImm"
//            )
//        }
    }

    override fun branchGreaterSignedImm(s1: RawReg, s2: UInt, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchGreaterSignedImm,
                Args.unresolvedBranchGreaterSignedImm(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchGreaterSignedImm"
            )
        }
    }

    override fun branchGreaterUnsignedImm(s1: RawReg, s2: UInt, i: UInt) {
        TODO("Not yet implemented")
//        val targetTrue = ProgramCounter(i)
//        if (!module.isJumpTargetValid(targetTrue)) {
//            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
//        } else {
//            val targetFalse = nextProgramCounter
//            instance.emit(
//                RawHandlers.unresolvedBranchGreaterUnsignedImm,
//                Args.unresolvedBranchGreaterUnsignedImm(s1, s2, targetTrue, targetFalse),
//                "unresolvedBranchGreaterUnsignedImm"
//            )
//        }
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
        instance.emit(
            RawHandlers.orImm,
            Args.orImm(reg1, reg2, imm), "orImm"
        )
    }

    override fun mulImm32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        instance.emit(
            RawHandlers.mulImm32,
            Args.mulImm32(reg1, reg2, imm),
            "mulImm32"
        )
    }

    override fun mulImm64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        instance.emit(
            RawHandlers.mulImm64,
            Args.mulImm64(reg1, reg2, imm),
            "mulImm64"
        )
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
        instance.emit(
            RawHandlers.negateAndAddImm32,
            Args.negateAndAddImm32(reg1, reg2, imm),
            "negateAndAddImm32"
        )
    }

    override fun negateAndAddImm64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        instance.emit(
            RawHandlers.negateAndAddImm64,
            Args.negateAndAddImm64(reg1, reg2, imm),
            "negateAndAddImm64"
        )
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
        instance.emit(
            RawHandlers.cmovIfZeroImm,
            Args.cmovIfZeroImm(reg1, reg2, imm), "cmovIfZeroImm"
        )
    }

    override fun cmovIfNotZeroImm(reg1: RawReg, reg2: RawReg, imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun branchNotEq(s1: RawReg, s2: RawReg, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchNotEq,
                Args.unresolvedBranchNotEq(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchNotEq"
            )
        }
    }

    override fun branchLessUnsigned(s1: RawReg, s2: RawReg, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchLessUnsigned,
                Args.unresolvedBranchLessUnsigned(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchLessUnsigned"
            )
        }
    }

    override fun branchLessSigned(s1: RawReg, s2: RawReg, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchLessSigned,
                Args.unresolvedBranchLessSigned(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchLessSigned"
            )
        }
    }

    override fun branchGreaterOrEqualUnsigned(s1: RawReg, s2: RawReg, i: UInt) {
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchGreaterOrEqualUnsigned,
                Args.unresolvedBranchGreaterOrEqualUnsigned(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchGreaterOrEqualUnsigned"
            )
        }
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
        instance.emit(
            RawHandlers.or,
            Args.or(reg1, reg2, reg3), "or"
        )
    }

    override fun mul32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.mul32,
            Args.mul32(reg1, reg2, reg3),
            "mul32"
        )
    }

    override fun mul64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.mul64,
            Args.mul64(reg1, reg2, reg3),
            "mul64"
        )
    }

    override fun mulUpperSignedSigned(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        if (module.blob().is64Bit) {
            instance.emit(
                RawHandlers.mulUpperSignedSigned64,
                Args.mulUpperSignedSigned64(reg1, reg2, reg3),
                "mulUpperSignedSigned64"
            )
        } else {
            instance.emit(
                RawHandlers.mulUpperSignedSigned32,
                Args.mulUpperSignedSigned32(reg1, reg2, reg3),
                "mulUpperSignedSigned32"
            )
        }
    }

    override fun mulUpperUnsignedUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        if (module.blob().is64Bit) {
            instance.emit(
                RawHandlers.mulUpperUnsignedUnsigned64,
                Args.mulUpperUnsignedUnsigned64(reg1, reg2, reg3),
                "mulUpperUnsignedUnsigned64"
            )
        } else {
            instance.emit(
                RawHandlers.mulUpperUnsignedUnsigned32,
                Args.mulUpperUnsignedUnsigned32(reg1, reg2, reg3),
                "mulUpperUnsignedUnsigned32"
            )
        }
    }

    override fun mulUpperSignedUnsigned(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        if (module.blob().is64Bit) {
            instance.emit(
                RawHandlers.mulUpperSignedUnsigned64,
                Args.mulUpperSignedUnsigned64(reg1, reg2, reg3),
                "mulUpperSignedUnsigned64"
            )
        } else {
            instance.emit(
                RawHandlers.mulUpperSignedUnsigned32,
                Args.mulUpperSignedUnsigned32(reg1, reg2, reg3),
                "mulUpperSignedUnsigned32"
            )
        }
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
        instance.emit(
            RawHandlers.divUnsigned32,
            Args.divUnsigned32(reg1, reg2, reg3),
            "div_unsigned_32"
        )
    }

    override fun divUnsigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.divUnsigned64,
            Args.divUnsigned64(reg1, reg2, reg3),
            "div_unsigned_64"
        )
    }

    override fun divSigned32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.divSigned32,
            Args.divSigned32(reg1, reg2, reg3),
            "div_signed_32"
        )
    }

    override fun divSigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.divSigned64,
            Args.divSigned64(reg1, reg2, reg3),
            "div_signed_64"
        )
    }

    override fun remUnsigned32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.remUnsigned32,
            Args.remUnsigned32(reg1, reg2, reg3),
            "remUnsigned32"
        )
    }

    override fun remUnsigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.remUnsigned64,
            Args.remUnsigned64(reg1, reg2, reg3),
            "remUnsigned64"
        )
    }

    override fun remSigned32(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.remSigned32,
            Args.remSigned32(reg1, reg2, reg3),
            "remSigned32"
        )
    }

    override fun remSigned64(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.remSigned64,
            Args.remSigned64(reg1, reg2, reg3),
            "remSigned64"
        )
    }

    override fun cmovIfZero(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.cmovIfZero,
            Args.cmovIfZero(reg1, reg2, reg3), "cmovIfZero"
        )
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

    override fun loadImmAndJumpIndirect(ra: RawReg, base: RawReg, value: UInt, offset: UInt) {
        instance.emit(
            RawHandlers.loadImmAndJumpIndirect,
            Args.loadImmAndJumpIndirect(programCounter, ra, base, value, offset),
            "loadImmAndJumpIndirect"
        )
    }

    override fun loadImm64(reg: RawReg, imm: ULong) {
        TODO("Not yet implemented")
    }


}
