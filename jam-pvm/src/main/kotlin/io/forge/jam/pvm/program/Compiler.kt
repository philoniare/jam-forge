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
        val target = nextProgramCounter()
        logger.debug("Target: $target")
        instance.emit(
            RawHandlers.unresolvedFallthrough,
            Args.unresolvedFallthrough(target),
            "unresolvedFallthrough"
        )
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
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadU8Basic,
                Args.loadU8Basic(programCounter, reg, imm),
                "loadU8Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadU8Dynamic,
                Args.loadU8Dynamic(programCounter, reg, imm),
                "loadU8Dynamic"
            )
        }
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
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadU16Basic,
                Args.loadU16Basic(programCounter, reg, imm),
                "loadU16Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadU16Dynamic,
                Args.loadU16Dynamic(programCounter, reg, imm),
                "loadU16Dynamic"
            )
        }
    }

    override fun loadI16(reg: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadI16Basic,
                Args.loadI16Basic(programCounter, reg, imm), "loadI16Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadI16Dynamic,
                Args.loadI16Dynamic(programCounter, reg, imm), "loadI16Dynamic"
            )
        }
    }

    override fun loadU32(reg: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadU32Basic,
                Args.loadU32Basic(programCounter, reg, imm),
                "loadU32Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadU32Dynamic,
                Args.loadU32Dynamic(programCounter, reg, imm),
                "loadU32Dynamic"
            )
        }
    }

    override fun loadI32(reg: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadI32Basic,
                Args.loadI32Basic(programCounter, reg, imm),
                "loadI32Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadI32Dynamic,
                Args.loadI32Dynamic(programCounter, reg, imm),
                "loadI32Dynamic"
            )
        }
    }

    override fun loadU64(reg: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadU64Basic,
                Args.loadU64Basic(programCounter, reg, imm),
                "loadU64Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadU64Dynamic,
                Args.loadU64Dynamic(programCounter, reg, imm),
                "loadU64Dynamic"
            )
        }
    }

    override fun storeU8(reg: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.storeU8Basic,
                Args.storeU8Basic(programCounter, reg, imm),
                "storeU8Basic"
            )
        } else {
            instance.emit(
                RawHandlers.storeU8Dynamic,
                Args.storeU8Dynamic(programCounter, reg, imm),
                "storeU8Dynamic"
            )
        }
    }

    override fun storeU16(reg: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.storeU16Basic,
                Args.storeU16Basic(programCounter, reg, imm),
                "storeU16Basic"
            )
        } else {
            instance.emit(
                RawHandlers.storeU16Dynamic,
                Args.storeU16Dynamic(programCounter, reg, imm),
                "storeU16Dynamic"
            )
        }
    }

    override fun storeU32(reg: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.storeU32Basic,
                Args.storeU32Basic(programCounter, reg, imm),
                "storeU32Basic"
            )
        } else {
            instance.emit(
                RawHandlers.storeU32Dynamic,
                Args.storeU32Dynamic(programCounter, reg, imm),
                "storeU32Dynamic"
            )
        }
    }

    override fun storeU64(reg: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.storeU64Basic,
                Args.storeU64Basic(programCounter, reg, imm),
                "storeU64Basic"
            )
        } else {
            instance.emit(
                RawHandlers.storeU64Dynamic,
                Args.storeU64Dynamic(programCounter, reg, imm),
                "storeU64Dynamic"
            )
        }
    }

    override fun loadImmAndJump(reg: RawReg, imm1: UInt, imm2: UInt) {
        instance.emit(
            RawHandlers.loadImm,
            Args.loadImm(reg, imm1), "loadImmAndJump"
        )
        instance.emit(
            RawHandlers.unresolvedJump,
            Args.unresolvedJump(programCounter, ProgramCounter(imm2)),
            "unresolvedJump"
        )
    }

    override fun branchEq(reg1: RawReg, reg2: RawReg, imm: UInt) {
        val targetTrue = ProgramCounter(imm)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchEq,
                Args.unresolvedBranchEq(reg1, reg2, targetTrue, targetFalse), "unresolvedBranchEq"
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
        val targetTrue = ProgramCounter(imm2)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchLessOrEqualUnsignedImm,
                Args.unresolvedBranchLessOrEqualUnsignedImm(reg, imm1, targetTrue, targetFalse),
                "unresolvedBranchLessOrEqualUnsignedImm"
            )
        }
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
        val targetTrue = ProgramCounter(i)
        if (!module.isJumpTargetValid(targetTrue)) {
            instance.emit(RawHandlers.invalidBranch, Args.invalidBranch(programCounter), "invalidBranch")
        } else {
            val targetFalse = nextProgramCounter
            instance.emit(
                RawHandlers.unresolvedBranchGreaterUnsignedImm,
                Args.unresolvedBranchGreaterUnsignedImm(s1, s2, targetTrue, targetFalse),
                "unresolvedBranchGreaterUnsignedImm"
            )
        }
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
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadIndirectU8Basic,
                Args.loadIndirectU8Basic(programCounter, reg1, reg2, imm),
                "loadIndirectU8Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadIndirectU8Dynamic,
                Args.loadIndirectU8Dynamic(programCounter, reg1, reg2, imm),
                "loadIndirectU8Dynamic"
            )
        }
    }

    override fun loadIndirectI8(reg1: RawReg, reg2: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadIndirectI8Basic,
                Args.loadIndirectI8Basic(programCounter, reg1, reg2, imm),
                "loadIndirectI8Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadIndirectI8Dynamic,
                Args.loadIndirectI8Dynamic(programCounter, reg1, reg2, imm),
                "loadIndirectI8Dynamic"
            )
        }
    }

    override fun loadIndirectU16(reg1: RawReg, reg2: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadIndirectU16Basic,
                Args.loadIndirectU16Basic(programCounter, reg1, reg2, imm),
                "loadIndirectU16Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadIndirectU16Dynamic,
                Args.loadIndirectU16Dynamic(programCounter, reg1, reg2, imm),
                "loadIndirectU16Dynamic"
            )
        }
    }

    override fun loadIndirectI16(reg1: RawReg, reg2: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadIndirectI16Basic,
                Args.loadIndirectI16Basic(programCounter, reg1, reg2, imm),
                "loadIndirectI16Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadIndirectI16Dynamic,
                Args.loadIndirectI16Dynamic(programCounter, reg1, reg2, imm),
                "loadIndirectI16Dynamic"
            )
        }
    }

    override fun loadIndirectU32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadIndirectU32Basic,
                Args.loadIndirectU32Basic(programCounter, reg1, reg2, imm),
                "loadIndirectU32Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadIndirectU32Dynamic,
                Args.loadIndirectU32Dynamic(programCounter, reg1, reg2, imm),
                "loadIndirectU32Dynamic"
            )
        }
    }

    override fun loadIndirectI32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadIndirectI32Basic,
                Args.loadIndirectI32Basic(programCounter, reg1, reg2, imm),
                "loadIndirectI32Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadIndirectI32Dynamic,
                Args.loadIndirectI32Dynamic(programCounter, reg1, reg2, imm),
                "loadIndirectI32Dynamic"
            )
        }
    }

    override fun loadIndirectU64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.loadIndirectU64Basic,
                Args.loadIndirectU64Basic(programCounter, reg1, reg2, imm),
                "loadIndirectU64Basic"
            )
        } else {
            instance.emit(
                RawHandlers.loadIndirectU64Dynamic,
                Args.loadIndirectU64Dynamic(programCounter, reg1, reg2, imm),
                "loadIndirectU64Dynamic"
            )
        }
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

    override fun setLessThanUnsignedImm(d: RawReg, s1: RawReg, s2: UInt) {
        instance.emit(
            RawHandlers.setLessThanUnsignedImm,
            Args.setLessThanUnsignedImm(d, s1, s2),
            "setLessThanUnsignedImm"
        )
    }

    override fun setLessThanSignedImm(d: RawReg, s1: RawReg, s2: UInt) {
        instance.emit(
            RawHandlers.setLessThanSignedImm,
            Args.setLessThanSignedImm(d, s1, s2),
            "setLessThanSignedImm"
        )
    }

    override fun shiftLogicalLeftImm32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        instance.emit(
            RawHandlers.shiftLogicalLeftImm32,
            Args.shiftLogicalLeftImm32(reg1, reg2, imm),
            "shiftLogicalLeftImm32"
        )
    }

    override fun shiftLogicalLeftImm64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        instance.emit(
            RawHandlers.shiftLogicalLeftImm64,
            Args.shiftLogicalLeftImm64(reg1, reg2, imm),
            "shiftLogicalLeftImm64"
        )
    }

    override fun shiftLogicalRightImm32(d: RawReg, s1: RawReg, s2: UInt) {
        instance.emit(
            RawHandlers.shiftLogicalRightImm32,
            Args.shiftLogicalRightImm32(d, s1, s2),
            "shiftLogicalRightImm32"
        )
    }

    override fun shiftLogicalRightImm64(d: RawReg, s1: RawReg, s2: UInt) {
        instance.emit(
            RawHandlers.shiftLogicalRightImm64,
            Args.shiftLogicalRightImm64(d, s1, s2),
            "shiftLogicalRightImm64"
        )
    }

    override fun shiftArithmeticRightImm32(d: RawReg, s1: RawReg, s2: UInt) {
        instance.emit(
            RawHandlers.shiftArithmeticRightImm32,
            Args.shiftArithmeticRightImm32(d, s1, s2),
            "shiftArithmeticRightImm32"
        )
    }

    override fun shiftArithmeticRightImm64(d: RawReg, s1: RawReg, s2: UInt) {
        instance.emit(
            RawHandlers.shiftArithmeticRightImm64,
            Args.shiftArithmeticRightImm64(d, s1, s2),
            "shiftArithmeticRightImm64"
        )
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

    override fun setGreaterThanUnsignedImm(d: RawReg, s1: RawReg, s2: UInt) {
        instance.emit(
            RawHandlers.setGreaterThanUnsignedImm,
            Args.setGreaterThanUnsignedImm(d, s1, s2),
            "setGreaterThanUnsignedImm"
        )
    }

    override fun setGreaterThanSignedImm(d: RawReg, s1: RawReg, s2: UInt) {
        instance.emit(
            RawHandlers.setGreaterThanSignedImm,
            Args.setGreaterThanSignedImm(d, s1, s2),
            "setGreaterThanSignedImm"
        )
    }

    override fun shiftLogicalRightImmAlt32(d: RawReg, s2: RawReg, s1: UInt) {
        instance.emit(
            RawHandlers.shiftLogicalRightImmAlt32,
            Args.shiftLogicalRightImmAlt32(d, s2, s1),
            "shiftLogicalRightImmAlt32"
        )
    }

    override fun shiftLogicalRightImmAlt64(d: RawReg, s2: RawReg, s1: UInt) {
        instance.emit(
            RawHandlers.shiftLogicalRightImmAlt64,
            Args.shiftLogicalRightImmAlt64(d, s2, s1),
            "shiftLogicalRightImmAlt64"
        )
    }

    override fun shiftArithmeticRightImmAlt32(d: RawReg, s2: RawReg, s1: UInt) {
        instance.emit(
            RawHandlers.shiftArithmeticRightImmAlt32,
            Args.shiftArithmeticRightImmAlt32(d, s2, s1),
            "shiftArithmeticRightImmAlt32"
        )
    }

    override fun shiftArithmeticRightImmAlt64(d: RawReg, s2: RawReg, s1: UInt) {
        instance.emit(
            RawHandlers.shiftArithmeticRightImmAlt64,
            Args.shiftArithmeticRightImmAlt64(d, s2, s1),
            "shiftArithmeticRightImmAlt64"
        )
    }

    override fun shiftLogicalLeftImmAlt32(reg1: RawReg, reg2: RawReg, imm: UInt) {
        instance.emit(
            RawHandlers.shiftLogicalLeftImmAlt32,
            Args.shiftLogicalLeftImmAlt32(reg1, reg2, imm),
            "shiftLogicalLeftImmAlt32"
        )
    }

    override fun shiftLogicalLeftImmAlt64(reg1: RawReg, reg2: RawReg, imm: UInt) {
        instance.emit(
            RawHandlers.shiftLogicalLeftImmAlt64,
            Args.shiftLogicalLeftImmAlt64(reg1, reg2, imm),
            "shiftLogicalLeftImmAlt64"
        )
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
        instance.emit(
            RawHandlers.setLessThanUnsigned,
            Args.setLessThanUnsigned(reg1, reg2, reg3),
            "setLessThanUnsigned"
        )
    }

    override fun setLessThanSigned(reg1: RawReg, reg2: RawReg, reg3: RawReg) {
        instance.emit(
            RawHandlers.setLessThanSigned,
            Args.setLessThanSigned(reg1, reg2, reg3),
            "setLessThanSigned"
        )
    }

    override fun shiftLogicalLeft32(d: RawReg, s1: RawReg, s2: RawReg) {
        instance.emit(
            RawHandlers.shiftLogicalLeft32,
            Args.shiftLogicalLeft32(d, s1, s2),
            "shiftLogicalLeft32"
        )
    }

    override fun shiftLogicalLeft64(d: RawReg, s1: RawReg, s2: RawReg) {
        instance.emit(
            RawHandlers.shiftLogicalLeft64,
            Args.shiftLogicalLeft64(d, s1, s2),
            "shiftLogicalLeft64"
        )
    }

    override fun shiftLogicalRight32(d: RawReg, s1: RawReg, s2: RawReg) {
        instance.emit(
            RawHandlers.shiftLogicalRight32,
            Args.shiftLogicalRight32(d, s1, s2),
            "shiftLogicalRight32"
        )
    }

    override fun shiftLogicalRight64(d: RawReg, s1: RawReg, s2: RawReg) {
        instance.emit(
            RawHandlers.shiftLogicalRight64,
            Args.shiftLogicalRight64(d, s1, s2),
            "shiftLogicalRight64"
        )
    }

    override fun shiftArithmeticRight32(d: RawReg, s1: RawReg, s2: RawReg) {
        instance.emit(
            RawHandlers.shiftArithmeticRight32,
            Args.shiftArithmeticRight32(d, s1, s2),
            "shiftArithmeticRight32"
        )
    }

    override fun shiftArithmeticRight64(d: RawReg, s1: RawReg, s2: RawReg) {
        instance.emit(
            RawHandlers.shiftArithmeticRight64,
            Args.shiftArithmeticRight64(d, s1, s2),
            "shiftArithmeticRight64"
        )
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
        instance.emit(
            RawHandlers.unresolvedJump,
            Args.unresolvedJump(programCounter, ProgramCounter(imm)),
            "unresolvedJump"
        )
    }

    override fun ecalli(imm: UInt) {
        TODO("Not yet implemented")
    }

    override fun storeImmU8(imm1: UInt, imm2: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.storeImmU8Basic,
                Args.storeImmU8Basic(programCounter, imm1, imm2),
                "storeImmU8Basic"
            )
        } else {
            instance.emit(
                RawHandlers.storeImmU8Dynamic,
                Args.storeImmU8Dynamic(programCounter, imm1, imm2),
                "storeImmU8Dynamic"
            )
        }
    }

    override fun storeImmU16(imm1: UInt, imm2: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.storeImmU16Basic,
                Args.storeImmU16Basic(programCounter, imm1, imm2),
                "storeImmU16Basic"
            )
        } else {
            instance.emit(
                RawHandlers.storeImmU16Dynamic,
                Args.storeImmU16Dynamic(programCounter, imm1, imm2),
                "storeImmU16Dynamic"
            )
        }
    }

    override fun storeImmU32(imm1: UInt, imm2: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.storeImmU32Basic,
                Args.storeImmU32Basic(programCounter, imm1, imm2),
                "storeImmU32Basic"
            )
        } else {
            instance.emit(
                RawHandlers.storeImmU32Dynamic,
                Args.storeImmU32Dynamic(programCounter, imm1, imm2),
                "storeImmU32Dynamic"
            )
        }
    }

    override fun storeImmU64(imm1: UInt, imm2: UInt) {
        if (!module.isDynamicPaging()) {
            instance.emit(
                RawHandlers.storeImmU64Basic,
                Args.storeImmU64Basic(programCounter, imm1, imm2),
                "storeImmU64Basic"
            )
        } else {
            instance.emit(
                RawHandlers.storeImmU64Dynamic,
                Args.storeImmU64Dynamic(programCounter, imm1, imm2),
                "storeImmU64Dynamic"
            )
        }
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
        instance.emit(
            RawHandlers.loadImm64,
            Args.loadImm64(reg, Cast(imm).ulongTruncateToU32(), Cast(imm shr 32).ulongTruncateToU32()),
            "loadImm64"
        )
    }


}
