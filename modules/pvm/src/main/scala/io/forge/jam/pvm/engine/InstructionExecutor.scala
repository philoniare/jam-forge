package io.forge.jam.pvm.engine

import scala.annotation.switch
import spire.math.UInt
import io.forge.jam.pvm.types.*

/** Executes PVM instructions.
  *
  * Dispatch is a single `tableswitch` on the opcode byte, and every arm is a
  * direct call to a small per-opcode method that C2 can inline. Operands are
  * read from the primitive fields of [[CompiledInstruction]], filled once
  * when a block is compiled, so execution never touches the decoded
  * [[io.forge.jam.pvm.Instruction]] object.
  *
  * The arms call methods rather than holding their bodies so `execute` stays
  * far below HotSpot's 8000-byte `HugeMethodLimit`, above which a method is
  * never JIT-compiled.
  */
object InstructionExecutor:

  /** Executes a single instruction.
    *
    * @return
    *   the next compiled offset to continue execution, or a negative sentinel
    */
  def execute(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    (ci.opcodeValue: @switch) match
      case 0 => opPanic(ci, ctx)
      case 1 => opFallthrough(ci, ctx)
      case 2 => opUnlikely(ctx)
      case 40 => opJump(ci, ctx)
      case 50 => opJumpIndirect(ci, ctx)
      case 10 => opEcalli(ci, ctx)
      case 51 => opLoadImm(ci, ctx)
      case 20 => opLoadImm64(ci, ctx)
      case 80 => opLoadImmAndJump(ci, ctx)
      case 180 => opLoadImmAndJumpIndirect(ci, ctx)
      case 52 => opLoadU8(ci, ctx)
      case 53 => opLoadI8(ci, ctx)
      case 54 => opLoadU16(ci, ctx)
      case 55 => opLoadI16(ci, ctx)
      case 56 => opLoadU32(ci, ctx)
      case 57 => opLoadI32(ci, ctx)
      case 58 => opLoadU64(ci, ctx)
      case 124 => opLoadIndirectU8(ci, ctx)
      case 125 => opLoadIndirectI8(ci, ctx)
      case 126 => opLoadIndirectU16(ci, ctx)
      case 127 => opLoadIndirectI16(ci, ctx)
      case 128 => opLoadIndirectU32(ci, ctx)
      case 129 => opLoadIndirectI32(ci, ctx)
      case 130 => opLoadIndirectU64(ci, ctx)
      case 59 => opStoreU8(ci, ctx)
      case 60 => opStoreU16(ci, ctx)
      case 61 => opStoreU32(ci, ctx)
      case 62 => opStoreU64(ci, ctx)
      case 30 => opStoreImmU8(ci, ctx)
      case 31 => opStoreImmU16(ci, ctx)
      case 32 => opStoreImmU32(ci, ctx)
      case 33 => opStoreImmU64(ci, ctx)
      case 120 => opStoreIndirectU8(ci, ctx)
      case 121 => opStoreIndirectU16(ci, ctx)
      case 122 => opStoreIndirectU32(ci, ctx)
      case 123 => opStoreIndirectU64(ci, ctx)
      case 70 => opStoreImmIndirectU8(ci, ctx)
      case 71 => opStoreImmIndirectU16(ci, ctx)
      case 72 => opStoreImmIndirectU32(ci, ctx)
      case 73 => opStoreImmIndirectU64(ci, ctx)
      case 100 => opMoveReg(ci, ctx)
      case 104 => opCountLeadingZeroBits32(ci, ctx)
      case 103 => opCountLeadingZeroBits64(ci, ctx)
      case 106 => opCountTrailingZeroBits32(ci, ctx)
      case 105 => opCountTrailingZeroBits64(ci, ctx)
      case 102 => opCountSetBits32(ci, ctx)
      case 101 => opCountSetBits64(ci, ctx)
      case 107 => opSignExtend8(ci, ctx)
      case 108 => opSignExtend16(ci, ctx)
      case 109 => opZeroExtend16(ci, ctx)
      case 110 => opReverseByte(ci, ctx)
      case 131 => opAddImm32(ci, ctx)
      case 141 => opNegateAndAddImm32(ci, ctx)
      case 135 => opMulImm32(ci, ctx)
      case 149 => opAddImm64(ci, ctx)
      case 154 => opNegateAndAddImm64(ci, ctx)
      case 150 => opMulImm64(ci, ctx)
      case 132 => opAndImm(ci, ctx)
      case 134 => opOrImm(ci, ctx)
      case 133 => opXorImm(ci, ctx)
      case 138 => opShiftLogicalLeftImm32(ci, ctx)
      case 139 => opShiftLogicalRightImm32(ci, ctx)
      case 140 => opShiftArithmeticRightImm32(ci, ctx)
      case 144 => opShiftLogicalLeftImmAlt32(ci, ctx)
      case 145 => opShiftLogicalRightImmAlt32(ci, ctx)
      case 146 => opShiftArithmeticRightImmAlt32(ci, ctx)
      case 151 => opShiftLogicalLeftImm64(ci, ctx)
      case 152 => opShiftLogicalRightImm64(ci, ctx)
      case 153 => opShiftArithmeticRightImm64(ci, ctx)
      case 155 => opShiftLogicalLeftImmAlt64(ci, ctx)
      case 156 => opShiftLogicalRightImmAlt64(ci, ctx)
      case 157 => opShiftArithmeticRightImmAlt64(ci, ctx)
      case 160 => opRotateRightImm32(ci, ctx)
      case 158 => opRotateRightImm64(ci, ctx)
      case 161 => opRotateRightImmAlt32(ci, ctx)
      case 159 => opRotateRightImmAlt64(ci, ctx)
      case 136 => opSetLessThanUnsignedImm(ci, ctx)
      case 137 => opSetLessThanSignedImm(ci, ctx)
      case 142 => opSetGreaterThanUnsignedImm(ci, ctx)
      case 143 => opSetGreaterThanSignedImm(ci, ctx)
      case 147 => opCmovIfZeroImm(ci, ctx)
      case 148 => opCmovIfNotZeroImm(ci, ctx)
      case 81 => opBranchEqImm(ci, ctx)
      case 82 => opBranchNotEqImm(ci, ctx)
      case 83 => opBranchLessUnsignedImm(ci, ctx)
      case 87 => opBranchLessSignedImm(ci, ctx)
      case 85 => opBranchGreaterOrEqualUnsignedImm(ci, ctx)
      case 89 => opBranchGreaterOrEqualSignedImm(ci, ctx)
      case 84 => opBranchLessOrEqualUnsignedImm(ci, ctx)
      case 88 => opBranchLessOrEqualSignedImm(ci, ctx)
      case 90 => opBranchGreaterSignedImm(ci, ctx)
      case 86 => opBranchGreaterUnsignedImm(ci, ctx)
      case 170 => opBranchEq(ci, ctx)
      case 171 => opBranchNotEq(ci, ctx)
      case 172 => opBranchLessUnsigned(ci, ctx)
      case 173 => opBranchLessSigned(ci, ctx)
      case 174 => opBranchGreaterOrEqualUnsigned(ci, ctx)
      case 175 => opBranchGreaterOrEqualSigned(ci, ctx)
      case 190 => opAdd32(ci, ctx)
      case 191 => opSub32(ci, ctx)
      case 192 => opMul32(ci, ctx)
      case 193 => opDivUnsigned32(ci, ctx)
      case 194 => opDivSigned32(ci, ctx)
      case 195 => opRemUnsigned32(ci, ctx)
      case 196 => opRemSigned32(ci, ctx)
      case 200 => opAdd64(ci, ctx)
      case 201 => opSub64(ci, ctx)
      case 202 => opMul64(ci, ctx)
      case 203 => opDivUnsigned64(ci, ctx)
      case 204 => opDivSigned64(ci, ctx)
      case 205 => opRemUnsigned64(ci, ctx)
      case 206 => opRemSigned64(ci, ctx)
      case 210 => opAnd(ci, ctx)
      case 212 => opOr(ci, ctx)
      case 211 => opXor(ci, ctx)
      case 224 => opAndInverted(ci, ctx)
      case 225 => opOrInverted(ci, ctx)
      case 226 => opXnor(ci, ctx)
      case 197 => opShiftLogicalLeft32(ci, ctx)
      case 198 => opShiftLogicalRight32(ci, ctx)
      case 199 => opShiftArithmeticRight32(ci, ctx)
      case 207 => opShiftLogicalLeft64(ci, ctx)
      case 208 => opShiftLogicalRight64(ci, ctx)
      case 209 => opShiftArithmeticRight64(ci, ctx)
      case 221 => opRotateLeft32(ci, ctx)
      case 220 => opRotateLeft64(ci, ctx)
      case 223 => opRotateRight32(ci, ctx)
      case 222 => opRotateRight64(ci, ctx)
      case 216 => opSetLessThanUnsigned(ci, ctx)
      case 217 => opSetLessThanSigned(ci, ctx)
      case 218 => opCmovIfZero(ci, ctx)
      case 219 => opCmovIfNotZero(ci, ctx)
      case 227 => opMaximum(ci, ctx)
      case 228 => opMaximumUnsigned(ci, ctx)
      case 229 => opMinimum(ci, ctx)
      case 230 => opMinimumUnsigned(ci, ctx)
      case 213 => opMulUpperSignedSigned(ci, ctx)
      case 214 => opMulUpperUnsignedUnsigned(ci, ctx)
      case 215 => opMulUpperSignedUnsigned(ci, ctx)
      case _ => ctx.panic(ci.pc)

  private def opPanic(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.panic(pc)

  private def opFallthrough(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    val r = ctx.resolveJump(nextPc)
    if r < 0 then ctx.panic(pc) else r

  private def opUnlikely(ctx: InterpreterCore): Int =
    ctx.advance()

  private def opJump(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val target = ci.x
    val r = ctx.resolveJump(ProgramCounter(target.toInt))
    if r < 0 then ctx.panic(pc) else r

  private def opJumpIndirect(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.a) + ci.x).toInt
    ctx.jumpIndirectInt(pc, addr)

  private def opEcalli(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    val hostId = ci.x
    ctx.ecalli(pc, nextPc, UInt(hostId.toInt))

  private def opLoadImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.setReg32Int(ci.a, ci.x.toInt)
    ctx.advance()

  private def opLoadImm64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.setReg64(ci.a, ci.x)
    ctx.advance()

  private def opLoadImmAndJump(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.setReg32Int(ci.a, ci.x.toInt)
    val r = ctx.resolveJump(ProgramCounter(ci.y.toInt))
    if r < 0 then ctx.panic(pc) else r

  private def opLoadImmAndJumpIndirect(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.y).toInt
    ctx.setReg32Int(ci.a, ci.x.toInt)
    ctx.jumpIndirectInt(pc, addr)

  private def opLoadU8(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.loadU8Int(pc, ci.a, ci.x.toInt)

  private def opLoadI8(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.loadI8Int(pc, ci.a, ci.x.toInt)

  private def opLoadU16(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.loadU16Int(pc, ci.a, ci.x.toInt)

  private def opLoadI16(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.loadI16Int(pc, ci.a, ci.x.toInt)

  private def opLoadU32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.loadU32Int(pc, ci.a, ci.x.toInt)

  private def opLoadI32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.loadI32Int(pc, ci.a, ci.x.toInt)

  private def opLoadU64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.loadU64Int(pc, ci.a, ci.x.toInt)

  private def opLoadIndirectU8(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.x).toInt
    ctx.loadU8Int(pc, ci.a, addr)

  private def opLoadIndirectI8(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.x).toInt
    ctx.loadI8Int(pc, ci.a, addr)

  private def opLoadIndirectU16(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.x).toInt
    ctx.loadU16Int(pc, ci.a, addr)

  private def opLoadIndirectI16(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.x).toInt
    ctx.loadI16Int(pc, ci.a, addr)

  private def opLoadIndirectU32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.x).toInt
    ctx.loadU32Int(pc, ci.a, addr)

  private def opLoadIndirectI32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.x).toInt
    ctx.loadI32Int(pc, ci.a, addr)

  private def opLoadIndirectU64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.x).toInt
    ctx.loadU64Int(pc, ci.a, addr)

  private def opStoreU8(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.storeU8Int(pc, ci.a, ci.x.toInt)

  private def opStoreU16(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.storeU16Int(pc, ci.a, ci.x.toInt)

  private def opStoreU32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.storeU32Int(pc, ci.a, ci.x.toInt)

  private def opStoreU64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.storeU64Int(pc, ci.a, ci.x.toInt)

  private def opStoreImmU8(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.storeImmU8Int(pc, ci.x.toInt, ci.y.toByte)

  private def opStoreImmU16(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.storeImmU16Int(pc, ci.x.toInt, ci.y.toShort)

  private def opStoreImmU32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.storeImmU32Int(pc, ci.x.toInt, ci.y.toInt)

  private def opStoreImmU64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    ctx.storeImmU64Int(pc, ci.x.toInt, ci.y)

  private def opStoreIndirectU8(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.x).toInt
    ctx.storeU8Int(pc, ci.a, addr)

  private def opStoreIndirectU16(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.x).toInt
    ctx.storeU16Int(pc, ci.a, addr)

  private def opStoreIndirectU32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.x).toInt
    ctx.storeU32Int(pc, ci.a, addr)

  private def opStoreIndirectU64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.b) + ci.x).toInt
    ctx.storeU64Int(pc, ci.a, addr)

  private def opStoreImmIndirectU8(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.a) + ci.x).toInt
    ctx.storeImmU8Int(pc, addr, ci.y.toByte)

  private def opStoreImmIndirectU16(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.a) + ci.x).toInt
    ctx.storeImmU16Int(pc, addr, ci.y.toShort)

  private def opStoreImmIndirectU32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.a) + ci.x).toInt
    ctx.storeImmU32Int(pc, addr, ci.y.toInt)

  private def opStoreImmIndirectU64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val addr = (ctx.getReg(ci.a) + ci.x).toInt
    ctx.storeImmU64Int(pc, addr, ci.y)

  private def opMoveReg(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.setReg64(ci.a, ctx.getReg(ci.b))
    ctx.advance()

  private def opCountLeadingZeroBits32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b).toInt
    ctx.setReg32Int(ci.a, Integer.numberOfLeadingZeros(v))
    ctx.advance()

  private def opCountLeadingZeroBits64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b)
    ctx.setReg64(ci.a, java.lang.Long.numberOfLeadingZeros(v))
    ctx.advance()

  private def opCountTrailingZeroBits32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b).toInt
    ctx.setReg32Int(ci.a, Integer.numberOfTrailingZeros(v))
    ctx.advance()

  private def opCountTrailingZeroBits64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b)
    ctx.setReg64(ci.a, java.lang.Long.numberOfTrailingZeros(v))
    ctx.advance()

  private def opCountSetBits32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b).toInt
    ctx.setReg32Int(ci.a, Integer.bitCount(v))
    ctx.advance()

  private def opCountSetBits64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b)
    ctx.setReg64(ci.a, java.lang.Long.bitCount(v))
    ctx.advance()

  private def opSignExtend8(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b).toByte.toLong
    ctx.setReg64(ci.a, v)
    ctx.advance()

  private def opSignExtend16(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b).toShort.toLong
    ctx.setReg64(ci.a, v)
    ctx.advance()

  private def opZeroExtend16(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b) & 0xffffL
    ctx.setReg64(ci.a, v)
    ctx.advance()

  private def opReverseByte(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b)
    ctx.setReg64(ci.a, java.lang.Long.reverseBytes(v))
    ctx.advance()

  private def opAddImm32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm32(ci.a, ci.b, ci.x.toInt)(_ + _)

  private def opNegateAndAddImm32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm32(ci.a, ci.b, ci.x.toInt)((s, imm) => imm - s)

  private def opMulImm32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm32(ci.a, ci.b, ci.x.toInt)(_ * _)

  private def opAddImm64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.addImm64(ci.a, ci.b, ci.x)

  private def opNegateAndAddImm64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm64(ci.a, ci.b, ci.x)((s, imm) => imm - s)

  private def opMulImm64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm64(ci.a, ci.b, ci.x)(_ * _)

  private def opAndImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.andImm(ci.a, ci.b, ci.x)

  private def opOrImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.orImm(ci.a, ci.b, ci.x)

  private def opXorImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.xorImm(ci.a, ci.b, ci.x)

  private def opShiftLogicalLeftImm32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm32(ci.a, ci.b, ci.x.toInt & 31)((v, s) => v << s)

  private def opShiftLogicalRightImm32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm32(ci.a, ci.b, ci.x.toInt & 31)((v, s) => v >>> s)

  private def opShiftArithmeticRightImm32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm32(ci.a, ci.b, ci.x.toInt & 31)((v, s) => v >> s)

  private def opShiftLogicalLeftImmAlt32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm32(ci.a, ci.b, ci.x.toInt)((shift, v) => v << (shift & 31))

  private def opShiftLogicalRightImmAlt32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm32(ci.a, ci.b, ci.x.toInt)((shift, v) => v >>> (shift & 31))

  private def opShiftArithmeticRightImmAlt32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm32(ci.a, ci.b, ci.x.toInt)((shift, v) => v >> (shift & 31))

  private def opShiftLogicalLeftImm64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm64(ci.a, ci.b, ci.x & 63)((v, s) => v << s.toInt)

  private def opShiftLogicalRightImm64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm64(ci.a, ci.b, ci.x & 63)((v, s) => v >>> s.toInt)

  private def opShiftArithmeticRightImm64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm64(ci.a, ci.b, ci.x & 63)((v, s) => v >> s.toInt)

  private def opShiftLogicalLeftImmAlt64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm64(ci.a, ci.b, ci.x)((shift, v) => v << (shift.toInt & 63))

  private def opShiftLogicalRightImmAlt64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm64(ci.a, ci.b, ci.x)((shift, v) => v >>> (shift.toInt & 63))

  private def opShiftArithmeticRightImmAlt64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op2Imm64(ci.a, ci.b, ci.x)((shift, v) => v >> (shift.toInt & 63))

  private def opRotateRightImm32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b).toInt
    val shift = ci.x.toInt & 31
    val result = Integer.rotateRight(v, shift)
    ctx.setReg32Int(ci.a, result)
    ctx.advance()

  private def opRotateRightImm64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b)
    val shift = ci.x.toInt & 63
    val result = java.lang.Long.rotateRight(v, shift)
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opRotateRightImmAlt32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val shift = ctx.getReg(ci.b).toInt & 31
    val v = ci.x.toInt
    val result = Integer.rotateRight(v, shift)
    ctx.setReg32Int(ci.a, result)
    ctx.advance()

  private def opRotateRightImmAlt64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val shift = ctx.getReg(ci.b).toInt & 63
    val result = java.lang.Long.rotateRight(ci.x, shift)
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opSetLessThanUnsignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val result =
      if java.lang.Long.compareUnsigned(v1, ci.x) < 0 then 1L else 0L
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opSetLessThanSignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val result = if v1 < ci.x then 1L else 0L
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opSetGreaterThanUnsignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val result =
      if java.lang.Long.compareUnsigned(v1, ci.x) > 0 then 1L else 0L
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opSetGreaterThanSignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val result = if v1 > ci.x then 1L else 0L
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opCmovIfZeroImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    if ctx.getReg(ci.b) == 0L then ctx.setReg64(ci.a, ci.x)
    ctx.advance()

  private def opCmovIfNotZeroImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    if ctx.getReg(ci.b) != 0L then ctx.setReg64(ci.a, ci.x)
    ctx.advance()

  private def opBranchEqImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, ctx.getReg(ci.a) == ci.x, pc, ci.y.toInt, nextPc)

  private def opBranchNotEqImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, ctx.getReg(ci.a) != ci.x, pc, ci.y.toInt, nextPc)

  private def opBranchLessUnsignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, 
      java.lang.Long.compareUnsigned(ctx.getReg(ci.a), ci.x) < 0,
      pc,
      ci.y.toInt,
      nextPc
    )

  private def opBranchLessSignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, ctx.getReg(ci.a) < ci.x, pc, ci.y.toInt, nextPc)

  private def opBranchGreaterOrEqualUnsignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, 
      java.lang.Long.compareUnsigned(ctx.getReg(ci.a), ci.x) >= 0,
      pc,
      ci.y.toInt,
      nextPc
    )

  private def opBranchGreaterOrEqualSignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, ctx.getReg(ci.a) >= ci.x, pc, ci.y.toInt, nextPc)

  private def opBranchLessOrEqualUnsignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, 
      java.lang.Long.compareUnsigned(ctx.getReg(ci.a), ci.x) <= 0,
      pc,
      ci.y.toInt,
      nextPc
    )

  private def opBranchLessOrEqualSignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, ctx.getReg(ci.a) <= ci.x, pc, ci.y.toInt, nextPc)

  private def opBranchGreaterSignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, ctx.getReg(ci.a) > ci.x, pc, ci.y.toInt, nextPc)

  private def opBranchGreaterUnsignedImm(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, 
      java.lang.Long.compareUnsigned(ctx.getReg(ci.a), ci.x) > 0,
      pc,
      ci.y.toInt,
      nextPc
    )

  private def opBranchEq(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, 
      ctx.getReg(ci.a) == ctx.getReg(ci.b),
      pc,
      ci.x.toInt,
      nextPc
    )

  private def opBranchNotEq(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, 
      ctx.getReg(ci.a) != ctx.getReg(ci.b),
      pc,
      ci.x.toInt,
      nextPc
    )

  private def opBranchLessUnsigned(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, 
      java.lang.Long.compareUnsigned(ctx.getReg(ci.a), ctx.getReg(ci.b)) < 0,
      pc,
      ci.x.toInt,
      nextPc
    )

  private def opBranchLessSigned(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, 
      ctx.getReg(ci.a) < ctx.getReg(ci.b),
      pc,
      ci.x.toInt,
      nextPc
    )

  private def opBranchGreaterOrEqualUnsigned(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, 
      java.lang.Long.compareUnsigned(ctx.getReg(ci.a), ctx.getReg(ci.b)) >= 0,
      pc,
      ci.x.toInt,
      nextPc
    )

  private def opBranchGreaterOrEqualSigned(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val pc = ci.pc
    val nextPc = ci.nextPc
    ctx.branchCompiled(ci, 
      ctx.getReg(ci.a) >= ctx.getReg(ci.b),
      pc,
      ci.x.toInt,
      nextPc
    )

  private def opAdd32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.add32(ci.a, ci.b, ci.c)

  private def opSub32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.sub32(ci.a, ci.b, ci.c)

  private def opMul32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.mul32(ci.a, ci.b, ci.c)

  private def opDivUnsigned32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b).toInt
    val v2 = ctx.getReg(ci.c).toInt
    val result = if v2 == 0 then -1 else Integer.divideUnsigned(v1, v2)
    ctx.setReg32Int(ci.a, result)
    ctx.advance()

  private def opDivSigned32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b).toInt
    val v2 = ctx.getReg(ci.c).toInt
    val result =
      if v2 == 0 then -1
      else if v1 == Int.MinValue && v2 == -1 then Int.MinValue
      else v1 / v2
    ctx.setReg32Int(ci.a, result)
    ctx.advance()

  private def opRemUnsigned32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b).toInt
    val v2 = ctx.getReg(ci.c).toInt
    val result = if v2 == 0 then v1 else Integer.remainderUnsigned(v1, v2)
    ctx.setReg32Int(ci.a, result)
    ctx.advance()

  private def opRemSigned32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b).toInt
    val v2 = ctx.getReg(ci.c).toInt
    val result =
      if v2 == 0 then v1
      else if v1 == Int.MinValue && v2 == -1 then 0
      else v1 % v2
    ctx.setReg32Int(ci.a, result)
    ctx.advance()

  private def opAdd64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.add64(ci.a, ci.b, ci.c)

  private def opSub64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.sub64(ci.a, ci.b, ci.c)

  private def opMul64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.mul64(ci.a, ci.b, ci.c)

  private def opDivUnsigned64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val v2 = ctx.getReg(ci.c)
    val result =
      if v2 == 0L then -1L else java.lang.Long.divideUnsigned(v1, v2)
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opDivSigned64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val v2 = ctx.getReg(ci.c)
    val result =
      if v2 == 0L then -1L
      else if v1 == Long.MinValue && v2 == -1L then Long.MinValue
      else v1 / v2
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opRemUnsigned64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val v2 = ctx.getReg(ci.c)
    val result =
      if v2 == 0L then v1 else java.lang.Long.remainderUnsigned(v1, v2)
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opRemSigned64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val v2 = ctx.getReg(ci.c)
    val result =
      if v2 == 0L then v1
      else if v1 == Long.MinValue && v2 == -1L then 0L
      else v1 % v2
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opAnd(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.and64(ci.a, ci.b, ci.c)

  private def opOr(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.or64(ci.a, ci.b, ci.c)

  private def opXor(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.xor64(ci.a, ci.b, ci.c)

  private def opAndInverted(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op3_64(ci.a, ci.b, ci.c)((a, b) => a & ~b)

  private def opOrInverted(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op3_64(ci.a, ci.b, ci.c)((a, b) => a | ~b)

  private def opXnor(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.op3_64(ci.a, ci.b, ci.c)((a, b) => ~(a ^ b))

  private def opShiftLogicalLeft32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.shl32(ci.a, ci.b, ci.c)

  private def opShiftLogicalRight32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.shr32(ci.a, ci.b, ci.c)

  private def opShiftArithmeticRight32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.sar32(ci.a, ci.b, ci.c)

  private def opShiftLogicalLeft64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.shl64(ci.a, ci.b, ci.c)

  private def opShiftLogicalRight64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.shr64(ci.a, ci.b, ci.c)

  private def opShiftArithmeticRight64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    ctx.sar64(ci.a, ci.b, ci.c)

  private def opRotateLeft32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b).toInt
    val shift = ctx.getReg(ci.c).toInt & 31
    ctx.setReg32Int(ci.a, Integer.rotateLeft(v, shift))
    ctx.advance()

  private def opRotateLeft64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b)
    val shift = ctx.getReg(ci.c).toInt & 63
    ctx.setReg64(ci.a, java.lang.Long.rotateLeft(v, shift))
    ctx.advance()

  private def opRotateRight32(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b).toInt
    val shift = ctx.getReg(ci.c).toInt & 31
    ctx.setReg32Int(ci.a, Integer.rotateRight(v, shift))
    ctx.advance()

  private def opRotateRight64(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v = ctx.getReg(ci.b)
    val shift = ctx.getReg(ci.c).toInt & 63
    ctx.setReg64(ci.a, java.lang.Long.rotateRight(v, shift))
    ctx.advance()

  private def opSetLessThanUnsigned(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val result =
      if java.lang.Long
          .compareUnsigned(ctx.getReg(ci.b), ctx.getReg(ci.c)) < 0
      then 1L
      else 0L
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opSetLessThanSigned(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val result = if ctx.getReg(ci.b) < ctx.getReg(ci.c) then 1L else 0L
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opCmovIfZero(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    if ctx.getReg(ci.c) == 0L then ctx.setReg64(ci.a, ctx.getReg(ci.b))
    ctx.advance()

  private def opCmovIfNotZero(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    if ctx.getReg(ci.c) != 0L then ctx.setReg64(ci.a, ctx.getReg(ci.b))
    ctx.advance()

  private def opMaximum(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val v2 = ctx.getReg(ci.c)
    ctx.setReg64(ci.a, if v1 > v2 then v1 else v2)
    ctx.advance()

  private def opMaximumUnsigned(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val v2 = ctx.getReg(ci.c)
    ctx.setReg64(
      ci.a,
      if java.lang.Long.compareUnsigned(v1, v2) > 0 then v1 else v2
    )
    ctx.advance()

  private def opMinimum(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val v2 = ctx.getReg(ci.c)
    ctx.setReg64(ci.a, if v1 < v2 then v1 else v2)
    ctx.advance()

  private def opMinimumUnsigned(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val v1 = ctx.getReg(ci.b)
    val v2 = ctx.getReg(ci.c)
    ctx.setReg64(
      ci.a,
      if java.lang.Long.compareUnsigned(v1, v2) < 0 then v1 else v2
    )
    ctx.advance()

  private def opMulUpperSignedSigned(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val result =
      UInt128.mulUpperSignedSigned(ctx.getReg(ci.b), ctx.getReg(ci.c))
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opMulUpperUnsignedUnsigned(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val result = UInt128.mulUpperUnsigned(ctx.getReg(ci.b), ctx.getReg(ci.c))
    ctx.setReg64(ci.a, result)
    ctx.advance()

  private def opMulUpperSignedUnsigned(ci: CompiledInstruction, ctx: InterpreterCore): Int =
    val result =
      UInt128.mulUpperSignedUnsigned(ctx.getReg(ci.b), ctx.getReg(ci.c))
    ctx.setReg64(ci.a, result)
    ctx.advance()
