package io.forge.jam.pvm.engine

import spire.math.{UInt, ULong}
import io.forge.jam.pvm.{Instruction, InterruptKind, SegfaultInfo}
import io.forge.jam.pvm.types.*

/**
 * Executes PVM instructions using pattern matching on the Instruction ADT.
 */
object InstructionExecutor:

  /**
   * Executes a single instruction.
   *
   * @param instruction The instruction to execute
   * @param ctx The execution context
   * @param pc The current program counter
   * @param nextPc The next program counter (for fallthrough)
   * @return Some(target) to continue execution, None to interrupt
   */
  def execute(
    instruction: Instruction,
    ctx: ExecutionContext,
    pc: ProgramCounter,
    nextPc: ProgramCounter
  ): Option[UInt] =
    instruction match
      // ========================================================================
      // Argless Instructions
      // ========================================================================
      case Instruction.Panic =>
        ctx.panic(pc)

      case Instruction.Fallthrough =>
        ctx.resolveFallthrough(nextPc)

      case Instruction.Memset =>
        // TODO: Implement memset
        ctx.advance()

      case Instruction.Invalid =>
        ctx.panic(pc)

      // ========================================================================
      // Jump Instructions
      // ========================================================================
      case Instruction.Jump(target) =>
        ctx.resolveJump(ProgramCounter(target.toInt))

      case Instruction.JumpIndirect(reg, offset) =>
        val addr = (ctx.getReg(reg) + offset).toInt
        ctx.jumpIndirect(pc, UInt(addr))

      case Instruction.Ecalli(hostId) =>
        ctx.ecalli(pc, nextPc, UInt(hostId.toInt))

      // ========================================================================
      // Load Immediate Instructions
      // ========================================================================
      case Instruction.LoadImm(reg, imm) =>
        ctx.setReg32(reg, UInt(imm.toInt))
        ctx.advance()

      case Instruction.LoadImm64(reg, imm) =>
        ctx.setReg64(reg, imm)
        ctx.advance()

      case Instruction.LoadImmAndJump(reg, imm, target) =>
        ctx.setReg32(reg, UInt(imm.toInt))
        ctx.resolveJump(ProgramCounter(target.toInt))

      case Instruction.LoadImmAndJumpIndirect(dst, base, imm, offset) =>
        ctx.setReg32(dst, UInt(imm.toInt))
        val addr = (ctx.getReg(base) + offset).toInt
        ctx.jumpIndirect(pc, UInt(addr))

      // ========================================================================
      // Memory Load Instructions (Direct)
      // ========================================================================
      case Instruction.LoadU8(reg, address) =>
        ctx.loadU8(pc, reg, UInt(address.toInt))

      case Instruction.LoadI8(reg, address) =>
        ctx.loadI8(pc, reg, UInt(address.toInt))

      case Instruction.LoadU16(reg, address) =>
        ctx.loadU16(pc, reg, UInt(address.toInt))

      case Instruction.LoadI16(reg, address) =>
        ctx.loadI16(pc, reg, UInt(address.toInt))

      case Instruction.LoadU32(reg, address) =>
        ctx.loadU32(pc, reg, UInt(address.toInt))

      case Instruction.LoadI32(reg, address) =>
        ctx.loadI32(pc, reg, UInt(address.toInt))

      case Instruction.LoadU64(reg, address) =>
        ctx.loadU64(pc, reg, UInt(address.toInt))

      // ========================================================================
      // Memory Load Instructions (Indirect)
      // ========================================================================
      case Instruction.LoadIndirectU8(dst, base, offset) =>
        val addr = UInt((ctx.getReg(base) + offset).toInt)
        ctx.loadU8(pc, dst, addr)

      case Instruction.LoadIndirectI8(dst, base, offset) =>
        val addr = UInt((ctx.getReg(base) + offset).toInt)
        ctx.loadI8(pc, dst, addr)

      case Instruction.LoadIndirectU16(dst, base, offset) =>
        val addr = UInt((ctx.getReg(base) + offset).toInt)
        ctx.loadU16(pc, dst, addr)

      case Instruction.LoadIndirectI16(dst, base, offset) =>
        val addr = UInt((ctx.getReg(base) + offset).toInt)
        ctx.loadI16(pc, dst, addr)

      case Instruction.LoadIndirectU32(dst, base, offset) =>
        val addr = UInt((ctx.getReg(base) + offset).toInt)
        ctx.loadU32(pc, dst, addr)

      case Instruction.LoadIndirectI32(dst, base, offset) =>
        val addr = UInt((ctx.getReg(base) + offset).toInt)
        ctx.loadI32(pc, dst, addr)

      case Instruction.LoadIndirectU64(dst, base, offset) =>
        val addr = UInt((ctx.getReg(base) + offset).toInt)
        ctx.loadU64(pc, dst, addr)

      // ========================================================================
      // Memory Store Instructions (Direct)
      // ========================================================================
      case Instruction.StoreU8(reg, address) =>
        ctx.storeU8(pc, reg, UInt(address.toInt))

      case Instruction.StoreU16(reg, address) =>
        ctx.storeU16(pc, reg, UInt(address.toInt))

      case Instruction.StoreU32(reg, address) =>
        ctx.storeU32(pc, reg, UInt(address.toInt))

      case Instruction.StoreU64(reg, address) =>
        ctx.storeU64(pc, reg, UInt(address.toInt))

      // ========================================================================
      // Memory Store Instructions (Immediate)
      // ========================================================================
      case Instruction.StoreImmU8(address, value) =>
        ctx.storeImmU8(pc, UInt(address.toInt), value.toByte)

      case Instruction.StoreImmU16(address, value) =>
        ctx.storeImmU16(pc, UInt(address.toInt), value.toShort)

      case Instruction.StoreImmU32(address, value) =>
        ctx.storeImmU32(pc, UInt(address.toInt), value.toInt)

      case Instruction.StoreImmU64(address, value) =>
        ctx.storeImmU64(pc, UInt(address.toInt), value)

      // ========================================================================
      // Memory Store Instructions (Indirect)
      // ========================================================================
      case Instruction.StoreIndirectU8(src, base, offset) =>
        val addr = UInt((ctx.getReg(base) + offset).toInt)
        ctx.storeU8(pc, src, addr)

      case Instruction.StoreIndirectU16(src, base, offset) =>
        val addr = UInt((ctx.getReg(base) + offset).toInt)
        ctx.storeU16(pc, src, addr)

      case Instruction.StoreIndirectU32(src, base, offset) =>
        val addr = UInt((ctx.getReg(base) + offset).toInt)
        ctx.storeU32(pc, src, addr)

      case Instruction.StoreIndirectU64(src, base, offset) =>
        val addr = UInt((ctx.getReg(base) + offset).toInt)
        ctx.storeU64(pc, src, addr)

      // ========================================================================
      // Memory Store Instructions (Immediate Indirect)
      // ========================================================================
      case Instruction.StoreImmIndirectU8(reg, offset, value) =>
        val addr = UInt((ctx.getReg(reg) + offset).toInt)
        ctx.storeImmU8(pc, addr, value.toByte)

      case Instruction.StoreImmIndirectU16(reg, offset, value) =>
        val addr = UInt((ctx.getReg(reg) + offset).toInt)
        ctx.storeImmU16(pc, addr, value.toShort)

      case Instruction.StoreImmIndirectU32(reg, offset, value) =>
        val addr = UInt((ctx.getReg(reg) + offset).toInt)
        ctx.storeImmU32(pc, addr, value.toInt)

      case Instruction.StoreImmIndirectU64(reg, offset, value) =>
        val addr = UInt((ctx.getReg(reg) + offset).toInt)
        ctx.storeImmU64(pc, addr, value)

      // ========================================================================
      // Register Move and Special
      // ========================================================================
      case Instruction.MoveReg(dst, src) =>
        ctx.setReg64(dst, ctx.getReg(src))
        ctx.advance()

      case Instruction.Sbrk(dst, src) =>
        val size = UInt(ctx.getReg(src).toInt)
        ctx.sbrk(dst, size)

      // ========================================================================
      // Bit Counting Instructions
      // ========================================================================
      case Instruction.CountLeadingZeroBits32(dst, src) =>
        val v = ctx.getReg(src).toInt
        ctx.setReg32(dst, UInt(Integer.numberOfLeadingZeros(v)))
        ctx.advance()

      case Instruction.CountLeadingZeroBits64(dst, src) =>
        val v = ctx.getReg(src)
        ctx.setReg64(dst, java.lang.Long.numberOfLeadingZeros(v))
        ctx.advance()

      case Instruction.CountTrailingZeroBits32(dst, src) =>
        val v = ctx.getReg(src).toInt
        ctx.setReg32(dst, UInt(Integer.numberOfTrailingZeros(v)))
        ctx.advance()

      case Instruction.CountTrailingZeroBits64(dst, src) =>
        val v = ctx.getReg(src)
        ctx.setReg64(dst, java.lang.Long.numberOfTrailingZeros(v))
        ctx.advance()

      case Instruction.CountSetBits32(dst, src) =>
        val v = ctx.getReg(src).toInt
        ctx.setReg32(dst, UInt(Integer.bitCount(v)))
        ctx.advance()

      case Instruction.CountSetBits64(dst, src) =>
        val v = ctx.getReg(src)
        ctx.setReg64(dst, java.lang.Long.bitCount(v))
        ctx.advance()

      // ========================================================================
      // Sign/Zero Extension
      // ========================================================================
      case Instruction.SignExtend8(dst, src) =>
        val v = ctx.getReg(src).toByte.toLong
        ctx.setReg64(dst, v)
        ctx.advance()

      case Instruction.SignExtend16(dst, src) =>
        val v = ctx.getReg(src).toShort.toLong
        ctx.setReg64(dst, v)
        ctx.advance()

      case Instruction.ZeroExtend16(dst, src) =>
        val v = ctx.getReg(src) & 0xFFFFL
        ctx.setReg64(dst, v)
        ctx.advance()

      case Instruction.ReverseByte(dst, src) =>
        val v = ctx.getReg(src)
        ctx.setReg64(dst, java.lang.Long.reverseBytes(v))
        ctx.advance()

      // ========================================================================
      // Arithmetic with Immediate (32-bit)
      // ========================================================================
      case Instruction.AddImm32(dst, src, imm) =>
        ctx.op2Imm32(dst, src, imm.toInt)(_ + _)

      case Instruction.NegateAndAddImm32(dst, src, imm) =>
        ctx.op2Imm32(dst, src, imm.toInt)((s, i) => i - s)

      case Instruction.MulImm32(dst, src, imm) =>
        ctx.op2Imm32(dst, src, imm.toInt)(_ * _)

      // ========================================================================
      // Arithmetic with Immediate (64-bit)
      // ========================================================================
      case Instruction.AddImm64(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm)(_ + _)

      case Instruction.NegateAndAddImm64(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm)((s, i) => i - s)

      case Instruction.MulImm64(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm)(_ * _)

      // ========================================================================
      // Bitwise with Immediate
      // ========================================================================
      case Instruction.AndImm(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm)(_ & _)

      case Instruction.OrImm(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm)(_ | _)

      case Instruction.XorImm(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm)(_ ^ _)

      // ========================================================================
      // Shift with Immediate (32-bit)
      // ========================================================================
      case Instruction.ShiftLogicalLeftImm32(dst, src, imm) =>
        ctx.op2Imm32(dst, src, imm.toInt & 31)((v, s) => v << s)

      case Instruction.ShiftLogicalRightImm32(dst, src, imm) =>
        ctx.op2Imm32(dst, src, imm.toInt & 31)((v, s) => v >>> s)

      case Instruction.ShiftArithmeticRightImm32(dst, src, imm) =>
        ctx.op2Imm32(dst, src, imm.toInt & 31)((v, s) => v >> s)

      case Instruction.ShiftLogicalLeftImmAlt32(dst, src, imm) =>
        ctx.op2Imm32(dst, src, imm.toInt & 31)((s, v) => v << s)

      case Instruction.ShiftLogicalRightImmAlt32(dst, src, imm) =>
        ctx.op2Imm32(dst, src, imm.toInt & 31)((s, v) => v >>> s)

      case Instruction.ShiftArithmeticRightImmAlt32(dst, src, imm) =>
        ctx.op2Imm32(dst, src, imm.toInt & 31)((s, v) => v >> s)

      // ========================================================================
      // Shift with Immediate (64-bit)
      // ========================================================================
      case Instruction.ShiftLogicalLeftImm64(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm & 63)((v, s) => v << s.toInt)

      case Instruction.ShiftLogicalRightImm64(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm & 63)((v, s) => v >>> s.toInt)

      case Instruction.ShiftArithmeticRightImm64(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm & 63)((v, s) => v >> s.toInt)

      case Instruction.ShiftLogicalLeftImmAlt64(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm & 63)((s, v) => v << s.toInt)

      case Instruction.ShiftLogicalRightImmAlt64(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm & 63)((s, v) => v >>> s.toInt)

      case Instruction.ShiftArithmeticRightImmAlt64(dst, src, imm) =>
        ctx.op2Imm64(dst, src, imm & 63)((s, v) => v >> s.toInt)

      // ========================================================================
      // Rotate with Immediate
      // ========================================================================
      case Instruction.RotateRightImm32(dst, src, imm) =>
        val v = ctx.getReg(src).toInt
        val shift = imm.toInt & 31
        val result = Integer.rotateRight(v, shift)
        ctx.setReg32(dst, UInt(result))
        ctx.advance()

      case Instruction.RotateRightImm64(dst, src, imm) =>
        val v = ctx.getReg(src)
        val shift = imm.toInt & 63
        val result = java.lang.Long.rotateRight(v, shift)
        ctx.setReg64(dst, result)
        ctx.advance()

      case Instruction.RotateRightImmAlt32(dst, src, imm) =>
        val shift = ctx.getReg(src).toInt & 31
        val v = imm.toInt
        val result = Integer.rotateRight(v, shift)
        ctx.setReg32(dst, UInt(result))
        ctx.advance()

      case Instruction.RotateRightImmAlt64(dst, src, imm) =>
        val shift = ctx.getReg(src).toInt & 63
        val result = java.lang.Long.rotateRight(imm, shift)
        ctx.setReg64(dst, result)
        ctx.advance()

      // ========================================================================
      // Comparison with Immediate
      // ========================================================================
      case Instruction.SetLessThanUnsignedImm(dst, src, imm) =>
        val v1 = ctx.getReg(src)
        val result = if java.lang.Long.compareUnsigned(v1, imm) < 0 then 1L else 0L
        ctx.setReg64(dst, result)
        ctx.advance()

      case Instruction.SetLessThanSignedImm(dst, src, imm) =>
        val v1 = ctx.getReg(src)
        val result = if v1 < imm then 1L else 0L
        ctx.setReg64(dst, result)
        ctx.advance()

      case Instruction.SetGreaterThanUnsignedImm(dst, src, imm) =>
        val v1 = ctx.getReg(src)
        val result = if java.lang.Long.compareUnsigned(v1, imm) > 0 then 1L else 0L
        ctx.setReg64(dst, result)
        ctx.advance()

      case Instruction.SetGreaterThanSignedImm(dst, src, imm) =>
        val v1 = ctx.getReg(src)
        val result = if v1 > imm then 1L else 0L
        ctx.setReg64(dst, result)
        ctx.advance()

      // ========================================================================
      // Conditional Move with Immediate
      // ========================================================================
      case Instruction.CmovIfZeroImm(dst, src, imm) =>
        if ctx.getReg(src) == 0L then
          ctx.setReg64(dst, imm)
        ctx.advance()

      case Instruction.CmovIfNotZeroImm(dst, src, imm) =>
        if ctx.getReg(src) != 0L then
          ctx.setReg64(dst, imm)
        ctx.advance()

      // ========================================================================
      // Branch Instructions with Immediate
      // ========================================================================
      case Instruction.BranchEqImm(reg, imm, offset) =>
        ctx.branch(ctx.getReg(reg) == imm, pc, offset.toInt, nextPc)

      case Instruction.BranchNotEqImm(reg, imm, offset) =>
        ctx.branch(ctx.getReg(reg) != imm, pc, offset.toInt, nextPc)

      case Instruction.BranchLessUnsignedImm(reg, imm, offset) =>
        ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(reg), imm) < 0, pc, offset.toInt, nextPc)

      case Instruction.BranchLessSignedImm(reg, imm, offset) =>
        ctx.branch(ctx.getReg(reg) < imm, pc, offset.toInt, nextPc)

      case Instruction.BranchGreaterOrEqualUnsignedImm(reg, imm, offset) =>
        ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(reg), imm) >= 0, pc, offset.toInt, nextPc)

      case Instruction.BranchGreaterOrEqualSignedImm(reg, imm, offset) =>
        ctx.branch(ctx.getReg(reg) >= imm, pc, offset.toInt, nextPc)

      case Instruction.BranchLessOrEqualUnsignedImm(reg, imm, offset) =>
        ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(reg), imm) <= 0, pc, offset.toInt, nextPc)

      case Instruction.BranchLessOrEqualSignedImm(reg, imm, offset) =>
        ctx.branch(ctx.getReg(reg) <= imm, pc, offset.toInt, nextPc)

      case Instruction.BranchGreaterSignedImm(reg, imm, offset) =>
        ctx.branch(ctx.getReg(reg) > imm, pc, offset.toInt, nextPc)

      case Instruction.BranchGreaterUnsignedImm(reg, imm, offset) =>
        ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(reg), imm) > 0, pc, offset.toInt, nextPc)

      // ========================================================================
      // Branch Instructions with Register
      // ========================================================================
      case Instruction.BranchEq(r1, r2, offset) =>
        ctx.branch(ctx.getReg(r1) == ctx.getReg(r2), pc, offset.toInt, nextPc)

      case Instruction.BranchNotEq(r1, r2, offset) =>
        ctx.branch(ctx.getReg(r1) != ctx.getReg(r2), pc, offset.toInt, nextPc)

      case Instruction.BranchLessUnsigned(r1, r2, offset) =>
        ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(r1), ctx.getReg(r2)) < 0, pc, offset.toInt, nextPc)

      case Instruction.BranchLessSigned(r1, r2, offset) =>
        ctx.branch(ctx.getReg(r1) < ctx.getReg(r2), pc, offset.toInt, nextPc)

      case Instruction.BranchGreaterOrEqualUnsigned(r1, r2, offset) =>
        ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(r1), ctx.getReg(r2)) >= 0, pc, offset.toInt, nextPc)

      case Instruction.BranchGreaterOrEqualSigned(r1, r2, offset) =>
        ctx.branch(ctx.getReg(r1) >= ctx.getReg(r2), pc, offset.toInt, nextPc)

      // ========================================================================
      // Three-Register Arithmetic (32-bit)
      // ========================================================================
      case Instruction.Add32(d, s1, s2) =>
        ctx.op3_32(d, s1, s2)(_ + _)

      case Instruction.Sub32(d, s1, s2) =>
        ctx.op3_32(d, s1, s2)(_ - _)

      case Instruction.Mul32(d, s1, s2) =>
        ctx.op3_32(d, s1, s2)(_ * _)

      case Instruction.DivUnsigned32(d, s1, s2) =>
        val v1 = ctx.getReg(s1).toInt
        val v2 = ctx.getReg(s2).toInt
        val result = if v2 == 0 then -1 else Integer.divideUnsigned(v1, v2)
        ctx.setReg32(d, UInt(result))
        ctx.advance()

      case Instruction.DivSigned32(d, s1, s2) =>
        val v1 = ctx.getReg(s1).toInt
        val v2 = ctx.getReg(s2).toInt
        val result = if v2 == 0 then -1
          else if v1 == Int.MinValue && v2 == -1 then Int.MinValue
          else v1 / v2
        ctx.setReg32(d, UInt(result))
        ctx.advance()

      case Instruction.RemUnsigned32(d, s1, s2) =>
        val v1 = ctx.getReg(s1).toInt
        val v2 = ctx.getReg(s2).toInt
        val result = if v2 == 0 then v1 else Integer.remainderUnsigned(v1, v2)
        ctx.setReg32(d, UInt(result))
        ctx.advance()

      case Instruction.RemSigned32(d, s1, s2) =>
        val v1 = ctx.getReg(s1).toInt
        val v2 = ctx.getReg(s2).toInt
        val result = if v2 == 0 then v1
          else if v1 == Int.MinValue && v2 == -1 then 0
          else v1 % v2
        ctx.setReg32(d, UInt(result))
        ctx.advance()

      // ========================================================================
      // Three-Register Arithmetic (64-bit)
      // ========================================================================
      case Instruction.Add64(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)(_ + _)

      case Instruction.Sub64(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)(_ - _)

      case Instruction.Mul64(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)(_ * _)

      case Instruction.DivUnsigned64(d, s1, s2) =>
        val v1 = ctx.getReg(s1)
        val v2 = ctx.getReg(s2)
        val result = if v2 == 0L then -1L else java.lang.Long.divideUnsigned(v1, v2)
        ctx.setReg64(d, result)
        ctx.advance()

      case Instruction.DivSigned64(d, s1, s2) =>
        val v1 = ctx.getReg(s1)
        val v2 = ctx.getReg(s2)
        val result = if v2 == 0L then -1L
          else if v1 == Long.MinValue && v2 == -1L then Long.MinValue
          else v1 / v2
        ctx.setReg64(d, result)
        ctx.advance()

      case Instruction.RemUnsigned64(d, s1, s2) =>
        val v1 = ctx.getReg(s1)
        val v2 = ctx.getReg(s2)
        val result = if v2 == 0L then v1 else java.lang.Long.remainderUnsigned(v1, v2)
        ctx.setReg64(d, result)
        ctx.advance()

      case Instruction.RemSigned64(d, s1, s2) =>
        val v1 = ctx.getReg(s1)
        val v2 = ctx.getReg(s2)
        val result = if v2 == 0L then v1
          else if v1 == Long.MinValue && v2 == -1L then 0L
          else v1 % v2
        ctx.setReg64(d, result)
        ctx.advance()

      // ========================================================================
      // Three-Register Bitwise
      // ========================================================================
      case Instruction.And(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)(_ & _)

      case Instruction.Or(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)(_ | _)

      case Instruction.Xor(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)(_ ^ _)

      case Instruction.AndInverted(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)((a, b) => a & ~b)

      case Instruction.OrInverted(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)((a, b) => a | ~b)

      case Instruction.Xnor(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)((a, b) => ~(a ^ b))

      // ========================================================================
      // Three-Register Shifts (32-bit)
      // ========================================================================
      case Instruction.ShiftLogicalLeft32(d, s1, s2) =>
        ctx.op3_32(d, s1, s2)((v, s) => v << (s & 31))

      case Instruction.ShiftLogicalRight32(d, s1, s2) =>
        ctx.op3_32(d, s1, s2)((v, s) => v >>> (s & 31))

      case Instruction.ShiftArithmeticRight32(d, s1, s2) =>
        ctx.op3_32(d, s1, s2)((v, s) => v >> (s & 31))

      // ========================================================================
      // Three-Register Shifts (64-bit)
      // ========================================================================
      case Instruction.ShiftLogicalLeft64(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)((v, s) => v << (s.toInt & 63))

      case Instruction.ShiftLogicalRight64(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)((v, s) => v >>> (s.toInt & 63))

      case Instruction.ShiftArithmeticRight64(d, s1, s2) =>
        ctx.op3_64(d, s1, s2)((v, s) => v >> (s.toInt & 63))

      // ========================================================================
      // Three-Register Rotates
      // ========================================================================
      case Instruction.RotateLeft32(d, s1, s2) =>
        val v = ctx.getReg(s1).toInt
        val shift = ctx.getReg(s2).toInt & 31
        ctx.setReg32(d, UInt(Integer.rotateLeft(v, shift)))
        ctx.advance()

      case Instruction.RotateLeft64(d, s1, s2) =>
        val v = ctx.getReg(s1)
        val shift = ctx.getReg(s2).toInt & 63
        ctx.setReg64(d, java.lang.Long.rotateLeft(v, shift))
        ctx.advance()

      case Instruction.RotateRight32(d, s1, s2) =>
        val v = ctx.getReg(s1).toInt
        val shift = ctx.getReg(s2).toInt & 31
        ctx.setReg32(d, UInt(Integer.rotateRight(v, shift)))
        ctx.advance()

      case Instruction.RotateRight64(d, s1, s2) =>
        val v = ctx.getReg(s1)
        val shift = ctx.getReg(s2).toInt & 63
        ctx.setReg64(d, java.lang.Long.rotateRight(v, shift))
        ctx.advance()

      // ========================================================================
      // Three-Register Comparisons
      // ========================================================================
      case Instruction.SetLessThanUnsigned(d, s1, s2) =>
        val result = if java.lang.Long.compareUnsigned(ctx.getReg(s1), ctx.getReg(s2)) < 0 then 1L else 0L
        ctx.setReg64(d, result)
        ctx.advance()

      case Instruction.SetLessThanSigned(d, s1, s2) =>
        val result = if ctx.getReg(s1) < ctx.getReg(s2) then 1L else 0L
        ctx.setReg64(d, result)
        ctx.advance()

      // ========================================================================
      // Three-Register Conditional Moves
      // ========================================================================
      case Instruction.CmovIfZero(d, s1, s2) =>
        if ctx.getReg(s2) == 0L then
          ctx.setReg64(d, ctx.getReg(s1))
        ctx.advance()

      case Instruction.CmovIfNotZero(d, s1, s2) =>
        if ctx.getReg(s2) != 0L then
          ctx.setReg64(d, ctx.getReg(s1))
        ctx.advance()

      // ========================================================================
      // Three-Register Min/Max
      // ========================================================================
      case Instruction.Maximum(d, s1, s2) =>
        val v1 = ctx.getReg(s1)
        val v2 = ctx.getReg(s2)
        ctx.setReg64(d, if v1 > v2 then v1 else v2)
        ctx.advance()

      case Instruction.MaximumUnsigned(d, s1, s2) =>
        val v1 = ctx.getReg(s1)
        val v2 = ctx.getReg(s2)
        ctx.setReg64(d, if java.lang.Long.compareUnsigned(v1, v2) > 0 then v1 else v2)
        ctx.advance()

      case Instruction.Minimum(d, s1, s2) =>
        val v1 = ctx.getReg(s1)
        val v2 = ctx.getReg(s2)
        ctx.setReg64(d, if v1 < v2 then v1 else v2)
        ctx.advance()

      case Instruction.MinimumUnsigned(d, s1, s2) =>
        val v1 = ctx.getReg(s1)
        val v2 = ctx.getReg(s2)
        ctx.setReg64(d, if java.lang.Long.compareUnsigned(v1, v2) < 0 then v1 else v2)
        ctx.advance()

      // ========================================================================
      // Three-Register Multiply Upper
      // ========================================================================
      case Instruction.MulUpperSignedSigned(d, s1, s2) =>
        val v1 = BigInt(ctx.getReg(s1))
        val v2 = BigInt(ctx.getReg(s2))
        val result = ((v1 * v2) >> 64).toLong
        ctx.setReg64(d, result)
        ctx.advance()

      case Instruction.MulUpperUnsignedUnsigned(d, s1, s2) =>
        val v1 = BigInt(ctx.getReg(s1)) & BigInt("FFFFFFFFFFFFFFFF", 16)
        val v2 = BigInt(ctx.getReg(s2)) & BigInt("FFFFFFFFFFFFFFFF", 16)
        val result = ((v1 * v2) >> 64).toLong
        ctx.setReg64(d, result)
        ctx.advance()

      case Instruction.MulUpperSignedUnsigned(d, s1, s2) =>
        val v1 = BigInt(ctx.getReg(s1))
        val v2 = BigInt(ctx.getReg(s2)) & BigInt("FFFFFFFFFFFFFFFF", 16)
        val result = ((v1 * v2) >> 64).toLong
        ctx.setReg64(d, result)
        ctx.advance()
