package io.forge.jam.pvm.program

import io.forge.jam.pvm.engine.InstructionSet

data class EnumVisitor<I : InstructionSet>(override val instructionSet: I, override val state: Unit) :
    OpcodeVisitor<Instruction, I> {
    override fun dispatch(opcode: UInt, chunk: ULong, offset: UInt, skip: UInt): Instruction {
        if (instructionSet.opcodeFromU8(opcode.toUByte()) == null) {
            return Instruction.Invalid
        }

        return when (opcode.toInt()) {
            0 -> Instruction.Trap
            17 -> Instruction.Fallthrough
            19 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.JumpIndirect(reg, imm)
            }

            4 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.LoadImm(reg, imm)
            }

            60 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.LoadU8(reg, imm)
            }

            74 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.LoadI8(reg, imm)
            }

            76 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.LoadU16(reg, imm)
            }

            66 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.LoadI16(reg, imm)
            }

            10 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.LoadU32(reg, imm)
            }

            102 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.LoadI32(reg, imm)
            }

            95 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.LoadU64(reg, imm)
            }

            71 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.StoreU8(reg, imm)
            }

            69 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.StoreU16(reg, imm)
            }

            22 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.StoreU32(reg, imm)
            }

            96 -> {
                val (reg, imm) = Program.readArgsRegImm(chunk, skip)
                Instruction.StoreU64(reg, imm)
            }

            6 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.LoadImmAndJump(reg, imm1, imm2)
            }

            7 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.BranchEqImm(reg, imm1, imm2)
            }

            15 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.BranchNotEqImm(reg, imm1, imm2)
            }

            44 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.BranchLessUnsignedImm(reg, imm1, imm2)
            }

            32 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.BranchLessSignedImm(reg, imm1, imm2)
            }

            52 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.BranchGreaterOrEqualUnsignedImm(reg, imm1, imm2)
            }

            45 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.BranchGreaterOrEqualSignedImm(reg, imm1, imm2)
            }

            46 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.BranchLessOrEqualSignedImm(reg, imm1, imm2)
            }

            59 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.BranchLessOrEqualUnsignedImm(reg, imm1, imm2)
            }

            53 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.BranchGreaterSignedImm(reg, imm1, imm2)
            }

            50 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.BranchGreaterUnsignedImm(reg, imm1, imm2)
            }

            26 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.StoreImmIndirectU8(reg, imm1, imm2)
            }

            54 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.StoreImmIndirectU16(reg, imm1, imm2)
            }

            13 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.StoreImmIndirectU32(reg, imm1, imm2)
            }

            93 -> {
                val (reg, imm1, imm2) = Program.readArgsRegImmOffset(chunk, offset, skip)
                Instruction.StoreImmIndirectU64(reg, imm1, imm2)
            }

            16 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.StoreIndirectU8(reg1, reg2, imm)
            }

            29 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.StoreIndirectU16(reg1, reg2, imm)
            }

            3 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.StoreIndirectU32(reg1, reg2, imm)
            }

            90 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.StoreIndirectU64(reg1, reg2, imm)
            }

            11 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.LoadIndirectU8(reg1, reg2, imm)
            }

            21 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.LoadIndirectI8(reg1, reg2, imm)
            }

            37 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.LoadIndirectU16(reg1, reg2, imm)
            }

            33 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.LoadIndirectI16(reg1, reg2, imm)
            }

            99 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.LoadIndirectI32(reg1, reg2, imm)
            }

            1 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.LoadIndirectU32(reg1, reg2, imm)
            }

            91 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.LoadIndirectU64(reg1, reg2, imm)
            }

            2 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.AddImm32(reg1, reg2, imm)
            }

            104 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.AddImm64(reg1, reg2, imm)
            }

            18 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.AndImm(reg1, reg2, imm)
            }

            31 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.XorImm(reg1, reg2, imm)
            }

            49 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.OrImm(reg1, reg2, imm)
            }

            35 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.MulImm32(reg1, reg2, imm)
            }

            121 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.MulImm64(reg1, reg2, imm)
            }

            27 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.SetLessThanUnsignedImm(reg1, reg2, imm)
            }

            56 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.SetLessThanSignedImm(reg1, reg2, imm)
            }

            9 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftLogicalLeftImm32(reg1, reg2, imm)
            }

            105 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftLogicalLeftImm64(reg1, reg2, imm)
            }

            14 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftLogicalRightImm32(reg1, reg2, imm)
            }

            106 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftLogicalLeftImm64(reg1, reg2, imm)
            }

            25 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftArithmeticRightImm32(reg1, reg2, imm)
            }

            107 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftArithmeticRightImm64(reg1, reg2, imm)
            }

            40 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.NegateAndAddImm32(reg1, reg2, imm)
            }

            136 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.NegateAndAddImm64(reg1, reg2, imm)
            }

            39 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.SetGreaterThanUnsignedImm(reg1, reg2, imm)
            }

            61 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.SetGreaterThanSignedImm(reg1, reg2, imm)
            }

            72 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftLogicalRightImmAlt32(reg1, reg2, imm)
            }

            103 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftLogicalRightImmAlt64(reg1, reg2, imm)
            }

            80 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftArithmeticRightImmAlt32(reg1, reg2, imm)
            }

            111 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftArithmeticRightImmAlt64(reg1, reg2, imm)
            }

            75 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftLogicalLeftImmAlt32(reg1, reg2, imm)
            }

            110 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.ShiftLogicalLeftImmAlt64(reg1, reg2, imm)
            }

            85 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.CmovIfZeroImm(reg1, reg2, imm)
            }

            86 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Imm(chunk, skip)
                Instruction.CmovIfNotZeroImm(reg1, reg2, imm)
            }

            24 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Offset(chunk, offset, skip)
                Instruction.BranchEq(reg1, reg2, imm)
            }

            30 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Offset(chunk, offset, skip)
                Instruction.BranchNotEq(reg1, reg2, imm)
            }

            47 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Offset(chunk, offset, skip)
                Instruction.BranchLessUnsigned(reg1, reg2, imm)
            }

            48 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Offset(chunk, offset, skip)
                Instruction.BranchLessSigned(reg1, reg2, imm)
            }

            41 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Offset(chunk, offset, skip)
                Instruction.BranchGreaterOrEqualUnsigned(reg1, reg2, imm)
            }

            43 -> {
                val (reg1, reg2, imm) = Program.readArgsRegs2Offset(chunk, offset, skip)
                Instruction.BranchGreaterOrEqualSigned(reg1, reg2, imm)
            }

            8 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.Add32(reg1, reg2, reg3)
            }

            101 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.Add64(reg1, reg2, reg3)
            }

            20 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.Sub32(reg1, reg2, reg3)
            }

            112 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.Sub64(reg1, reg2, reg3)
            }

            23 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.And(reg1, reg2, reg3)
            }

            28 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.Xor(reg1, reg2, reg3)
            }

            12 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.Or(reg1, reg2, reg3)
            }

            34 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.Mul32(reg1, reg2, reg3)
            }

            113 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.Mul64(reg1, reg2, reg3)
            }

            67 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.MulUpperSignedSigned(reg1, reg2, reg3)
            }

            57 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.MulUpperUnsignedUnsigned(reg1, reg2, reg3)
            }

            81 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.MulUpperSignedUnsigned(reg1, reg2, reg3)
            }

            36 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.SetLessThanUnsigned(reg1, reg2, reg3)
            }

            58 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.SetLessThanSigned(reg1, reg2, reg3)
            }

            55 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.ShiftLogicalLeft32(reg1, reg2, reg3)
            }

            100 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.ShiftLogicalLeft64(reg1, reg2, reg3)
            }

            51 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.ShiftLogicalRight32(reg1, reg2, reg3)
            }

            108 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.ShiftLogicalRight64(reg1, reg2, reg3)
            }

            77 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.ShiftArithmeticRight32(reg1, reg2, reg3)
            }

            109 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.ShiftArithmeticRight64(reg1, reg2, reg3)
            }

            68 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.DivUnsigned32(reg1, reg2, reg3)
            }

            114 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.DivUnsigned64(reg1, reg2, reg3)
            }

            64 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.DivSigned32(reg1, reg2, reg3)
            }

            115 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.DivSigned64(reg1, reg2, reg3)
            }

            73 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.RemUnsigned32(reg1, reg2, reg3)
            }

            116 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.RemUnsigned64(reg1, reg2, reg3)
            }

            70 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.RemSigned32(reg1, reg2, reg3)
            }

            117 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.RemSigned64(reg1, reg2, reg3)
            }

            83 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.CmovIfZero(reg1, reg2, reg3)
            }

            84 -> {
                val (reg1, reg2, reg3) = Program.readArgsRegs3(chunk)
                Instruction.CmovIfNotZero(reg1, reg2, reg3)
            }

            5 -> {
                val imm = Program.readArgsOffset(chunk, offset, skip)
                Instruction.Jump(imm)
            }

            78 -> {
                val imm = Program.readArgsImm(chunk, skip)
                Instruction.Ecalli(imm)
            }

            62 -> {
                val (imm1, imm2) = Program.readArgsImm2(chunk, skip)
                Instruction.StoreImmU8(imm1, imm2)
            }

            79 -> {
                val (imm1, imm2) = Program.readArgsImm2(chunk, skip)
                Instruction.StoreImmU16(imm1, imm2)
            }

            38 -> {
                val (imm1, imm2) = Program.readArgsImm2(chunk, skip)
                Instruction.StoreImmU32(imm1, imm2)
            }

            98 -> {
                val (imm1, imm2) = Program.readArgsImm2(chunk, skip)
                Instruction.StoreImmU64(imm1, imm2)
            }

            82 -> {
                val (reg1, reg2) = Program.readArgsRegs2(chunk)
                Instruction.MoveReg(reg1, reg2)
            }

            87 -> {
                val (reg1, reg2) = Program.readArgsRegs2(chunk)
                Instruction.Sbrk(reg1, reg2)
            }

            42 -> {
                val (reg1, reg2, imm1, imm2) = Program.readArgsRegs2Imm2(chunk, skip)
                Instruction.LoadImmAndJumpIndirect(reg1, reg2, imm1, imm2)
            }

            118 -> {
                val (reg, imm) = Program.readArgsRegImm64(chunk, skip)
                Instruction.LoadImm64(reg, imm)
            }

            else -> Instruction.Invalid

        }
    }
}
