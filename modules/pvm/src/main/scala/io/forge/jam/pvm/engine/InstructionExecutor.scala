package io.forge.jam.pvm.engine

import spire.math.UInt
import io.forge.jam.pvm.{Instruction, Opcode}
import io.forge.jam.pvm.types.*

/**
 * Executes PVM instructions using an indexed handler array for O(1) dispatch.
 */
object InstructionExecutor:
  private type Handler = (Instruction, ExecutionContext, ProgramCounter, ProgramCounter) => Option[UInt]
  private val handlers: Array[Handler] =
    val arr = new Array[Handler](256)

    // Initialize all slots with panic handler (for invalid opcodes)
    val panicHandler: Handler = (_, ctx, pc, _) => ctx.panic(pc)
    java.util.Arrays.fill(arr.asInstanceOf[Array[AnyRef]], panicHandler)

    // Register all instruction handlers by opcode value

    // ========================================================================
    // Argless Instructions
    // ========================================================================
    arr(Opcode.Panic.value) = (_, ctx, pc, _) => ctx.panic(pc)
    arr(Opcode.Fallthrough.value) = (_, ctx, _, nextPc) => ctx.resolveFallthrough(nextPc)
    arr(Opcode.Memset.value) = (_, ctx, _, _) => ctx.advance() // TODO: Implement memset

    // ========================================================================
    // Jump Instructions
    // ========================================================================
    arr(Opcode.Jump.value) = (instr, ctx, _, _) =>
      val target = instr.asInstanceOf[Instruction.Jump].target
      ctx.resolveJump(ProgramCounter(target.toInt))

    arr(Opcode.JumpIndirect.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.JumpIndirect]
      val addr = (ctx.getReg(i.reg) + i.offset).toInt
      ctx.jumpIndirectInt(pc, addr)

    arr(Opcode.Ecalli.value) = (instr, ctx, pc, nextPc) =>
      val hostId = instr.asInstanceOf[Instruction.Ecalli].hostId
      ctx.ecalli(pc, nextPc, UInt(hostId.toInt))

    // ========================================================================
    // Load Immediate Instructions
    // ========================================================================
    arr(Opcode.LoadImm.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.LoadImm]
      ctx.setReg32Int(i.reg, i.imm.toInt)
      ctx.advance()

    arr(Opcode.LoadImm64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.LoadImm64]
      ctx.setReg64(i.reg, i.imm)
      ctx.advance()

    arr(Opcode.LoadImmAndJump.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.LoadImmAndJump]
      ctx.setReg32Int(i.reg, i.imm.toInt)
      ctx.resolveJump(ProgramCounter(i.target.toInt))

    arr(Opcode.LoadImmAndJumpIndirect.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadImmAndJumpIndirect]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.setReg32Int(i.dst, i.imm.toInt)
      ctx.jumpIndirectInt(pc, addr)

    // ========================================================================
    // Memory Load Instructions (Direct)
    // ========================================================================
    arr(Opcode.LoadU8.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadU8]
      ctx.loadU8Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadI8.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadI8]
      ctx.loadI8Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadU16.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadU16]
      ctx.loadU16Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadI16.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadI16]
      ctx.loadI16Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadU32.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadU32]
      ctx.loadU32Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadI32.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadI32]
      ctx.loadI32Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadU64.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadU64]
      ctx.loadU64Int(pc, i.reg, i.address.toInt)

    // ========================================================================
    // Memory Load Instructions (Indirect)
    // ========================================================================
    arr(Opcode.LoadIndirectU8.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadIndirectU8]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadU8Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectI8.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadIndirectI8]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadI8Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectU16.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadIndirectU16]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadU16Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectI16.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadIndirectI16]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadI16Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectU32.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadIndirectU32]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadU32Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectI32.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadIndirectI32]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadI32Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectU64.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.LoadIndirectU64]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadU64Int(pc, i.dst, addr)

    // ========================================================================
    // Memory Store Instructions (Direct)
    // ========================================================================
    arr(Opcode.StoreU8.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreU8]
      ctx.storeU8Int(pc, i.reg, i.address.toInt)

    arr(Opcode.StoreU16.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreU16]
      ctx.storeU16Int(pc, i.reg, i.address.toInt)

    arr(Opcode.StoreU32.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreU32]
      ctx.storeU32Int(pc, i.reg, i.address.toInt)

    arr(Opcode.StoreU64.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreU64]
      ctx.storeU64Int(pc, i.reg, i.address.toInt)

    // ========================================================================
    // Memory Store Instructions (Immediate)
    // ========================================================================
    arr(Opcode.StoreImmU8.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreImmU8]
      ctx.storeImmU8Int(pc, i.address.toInt, i.value.toByte)

    arr(Opcode.StoreImmU16.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreImmU16]
      ctx.storeImmU16Int(pc, i.address.toInt, i.value.toShort)

    arr(Opcode.StoreImmU32.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreImmU32]
      ctx.storeImmU32Int(pc, i.address.toInt, i.value.toInt)

    arr(Opcode.StoreImmU64.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreImmU64]
      ctx.storeImmU64Int(pc, i.address.toInt, i.value)

    // ========================================================================
    // Memory Store Instructions (Indirect)
    // ========================================================================
    arr(Opcode.StoreIndirectU8.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreIndirectU8]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.storeU8Int(pc, i.src, addr)

    arr(Opcode.StoreIndirectU16.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreIndirectU16]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.storeU16Int(pc, i.src, addr)

    arr(Opcode.StoreIndirectU32.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreIndirectU32]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.storeU32Int(pc, i.src, addr)

    arr(Opcode.StoreIndirectU64.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreIndirectU64]
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.storeU64Int(pc, i.src, addr)

    // ========================================================================
    // Memory Store Instructions (Immediate Indirect)
    // ========================================================================
    arr(Opcode.StoreImmIndirectU8.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreImmIndirectU8]
      val addr = (ctx.getReg(i.reg) + i.offset).toInt
      ctx.storeImmU8Int(pc, addr, i.value.toByte)

    arr(Opcode.StoreImmIndirectU16.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreImmIndirectU16]
      val addr = (ctx.getReg(i.reg) + i.offset).toInt
      ctx.storeImmU16Int(pc, addr, i.value.toShort)

    arr(Opcode.StoreImmIndirectU32.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreImmIndirectU32]
      val addr = (ctx.getReg(i.reg) + i.offset).toInt
      ctx.storeImmU32Int(pc, addr, i.value.toInt)

    arr(Opcode.StoreImmIndirectU64.value) = (instr, ctx, pc, _) =>
      val i = instr.asInstanceOf[Instruction.StoreImmIndirectU64]
      val addr = (ctx.getReg(i.reg) + i.offset).toInt
      ctx.storeImmU64Int(pc, addr, i.value)

    // ========================================================================
    // Register Move and Special
    // ========================================================================
    arr(Opcode.MoveReg.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.MoveReg]
      ctx.setReg64(i.dst, ctx.getReg(i.src))
      ctx.advance()

    arr(Opcode.Sbrk.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Sbrk]
      val size = UInt(ctx.getReg(i.src).toInt)
      ctx.sbrk(i.dst, size)

    // ========================================================================
    // Bit Counting Instructions
    // ========================================================================
    arr(Opcode.CountLeadingZeroBits32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.CountLeadingZeroBits32]
      val v = ctx.getReg(i.src).toInt
      ctx.setReg32Int(i.dst, Integer.numberOfLeadingZeros(v))
      ctx.advance()

    arr(Opcode.CountLeadingZeroBits64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.CountLeadingZeroBits64]
      val v = ctx.getReg(i.src)
      ctx.setReg64(i.dst, java.lang.Long.numberOfLeadingZeros(v))
      ctx.advance()

    arr(Opcode.CountTrailingZeroBits32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.CountTrailingZeroBits32]
      val v = ctx.getReg(i.src).toInt
      ctx.setReg32Int(i.dst, Integer.numberOfTrailingZeros(v))
      ctx.advance()

    arr(Opcode.CountTrailingZeroBits64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.CountTrailingZeroBits64]
      val v = ctx.getReg(i.src)
      ctx.setReg64(i.dst, java.lang.Long.numberOfTrailingZeros(v))
      ctx.advance()

    arr(Opcode.CountSetBits32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.CountSetBits32]
      val v = ctx.getReg(i.src).toInt
      ctx.setReg32Int(i.dst, Integer.bitCount(v))
      ctx.advance()

    arr(Opcode.CountSetBits64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.CountSetBits64]
      val v = ctx.getReg(i.src)
      ctx.setReg64(i.dst, java.lang.Long.bitCount(v))
      ctx.advance()

    // ========================================================================
    // Sign/Zero Extension
    // ========================================================================
    arr(Opcode.SignExtend8.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.SignExtend8]
      val v = ctx.getReg(i.src).toByte.toLong
      ctx.setReg64(i.dst, v)
      ctx.advance()

    arr(Opcode.SignExtend16.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.SignExtend16]
      val v = ctx.getReg(i.src).toShort.toLong
      ctx.setReg64(i.dst, v)
      ctx.advance()

    arr(Opcode.ZeroExtend16.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ZeroExtend16]
      val v = ctx.getReg(i.src) & 0xffffL
      ctx.setReg64(i.dst, v)
      ctx.advance()

    arr(Opcode.ReverseByte.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ReverseByte]
      val v = ctx.getReg(i.src)
      ctx.setReg64(i.dst, java.lang.Long.reverseBytes(v))
      ctx.advance()

    // ========================================================================
    // Arithmetic with Immediate (32-bit)
    // ========================================================================
    arr(Opcode.AddImm32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.AddImm32]
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)(_ + _)

    arr(Opcode.NegateAndAddImm32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.NegateAndAddImm32]
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)((s, imm) => imm - s)

    arr(Opcode.MulImm32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.MulImm32]
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)(_ * _)

    // ========================================================================
    // Arithmetic with Immediate (64-bit)
    // ========================================================================
    arr(Opcode.AddImm64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.AddImm64]
      ctx.addImm64(i.dst, i.src, i.imm)

    arr(Opcode.NegateAndAddImm64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.NegateAndAddImm64]
      ctx.op2Imm64(i.dst, i.src, i.imm)((s, imm) => imm - s)

    arr(Opcode.MulImm64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.MulImm64]
      ctx.op2Imm64(i.dst, i.src, i.imm)(_ * _)

    // ========================================================================
    // Bitwise with Immediate
    // ========================================================================
    arr(Opcode.AndImm.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.AndImm]
      ctx.andImm(i.dst, i.src, i.imm)

    arr(Opcode.OrImm.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.OrImm]
      ctx.orImm(i.dst, i.src, i.imm)

    arr(Opcode.XorImm.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.XorImm]
      ctx.xorImm(i.dst, i.src, i.imm)

    // ========================================================================
    // Shift with Immediate (32-bit)
    // ========================================================================
    arr(Opcode.ShiftLogicalLeftImm32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalLeftImm32]
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt & 31)((v, s) => v << s)

    arr(Opcode.ShiftLogicalRightImm32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalRightImm32]
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt & 31)((v, s) => v >>> s)

    arr(Opcode.ShiftArithmeticRightImm32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftArithmeticRightImm32]
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt & 31)((v, s) => v >> s)

    arr(Opcode.ShiftLogicalLeftImmAlt32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalLeftImmAlt32]
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)((shift, v) => v << (shift & 31))

    arr(Opcode.ShiftLogicalRightImmAlt32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalRightImmAlt32]
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)((shift, v) => v >>> (shift & 31))

    arr(Opcode.ShiftArithmeticRightImmAlt32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftArithmeticRightImmAlt32]
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)((shift, v) => v >> (shift & 31))

    // ========================================================================
    // Shift with Immediate (64-bit)
    // ========================================================================
    arr(Opcode.ShiftLogicalLeftImm64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalLeftImm64]
      ctx.op2Imm64(i.dst, i.src, i.imm & 63)((v, s) => v << s.toInt)

    arr(Opcode.ShiftLogicalRightImm64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalRightImm64]
      ctx.op2Imm64(i.dst, i.src, i.imm & 63)((v, s) => v >>> s.toInt)

    arr(Opcode.ShiftArithmeticRightImm64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftArithmeticRightImm64]
      ctx.op2Imm64(i.dst, i.src, i.imm & 63)((v, s) => v >> s.toInt)

    arr(Opcode.ShiftLogicalLeftImmAlt64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalLeftImmAlt64]
      ctx.op2Imm64(i.dst, i.src, i.imm)((shift, v) => v << (shift.toInt & 63))

    arr(Opcode.ShiftLogicalRightImmAlt64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalRightImmAlt64]
      ctx.op2Imm64(i.dst, i.src, i.imm)((shift, v) => v >>> (shift.toInt & 63))

    arr(Opcode.ShiftArithmeticRightImmAlt64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftArithmeticRightImmAlt64]
      ctx.op2Imm64(i.dst, i.src, i.imm)((shift, v) => v >> (shift.toInt & 63))

    // ========================================================================
    // Rotate with Immediate
    // ========================================================================
    arr(Opcode.RotateRightImm32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RotateRightImm32]
      val v = ctx.getReg(i.src).toInt
      val shift = i.imm.toInt & 31
      val result = Integer.rotateRight(v, shift)
      ctx.setReg32Int(i.dst, result)
      ctx.advance()

    arr(Opcode.RotateRightImm64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RotateRightImm64]
      val v = ctx.getReg(i.src)
      val shift = i.imm.toInt & 63
      val result = java.lang.Long.rotateRight(v, shift)
      ctx.setReg64(i.dst, result)
      ctx.advance()

    arr(Opcode.RotateRightImmAlt32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RotateRightImmAlt32]
      val shift = ctx.getReg(i.src).toInt & 31
      val v = i.imm.toInt
      val result = Integer.rotateRight(v, shift)
      ctx.setReg32Int(i.dst, result)
      ctx.advance()

    arr(Opcode.RotateRightImmAlt64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RotateRightImmAlt64]
      val shift = ctx.getReg(i.src).toInt & 63
      val result = java.lang.Long.rotateRight(i.imm, shift)
      ctx.setReg64(i.dst, result)
      ctx.advance()

    // ========================================================================
    // Comparison with Immediate
    // ========================================================================
    arr(Opcode.SetLessThanUnsignedImm.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.SetLessThanUnsignedImm]
      val v1 = ctx.getReg(i.src)
      val result = if java.lang.Long.compareUnsigned(v1, i.imm) < 0 then 1L else 0L
      ctx.setReg64(i.dst, result)
      ctx.advance()

    arr(Opcode.SetLessThanSignedImm.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.SetLessThanSignedImm]
      val v1 = ctx.getReg(i.src)
      val result = if v1 < i.imm then 1L else 0L
      ctx.setReg64(i.dst, result)
      ctx.advance()

    arr(Opcode.SetGreaterThanUnsignedImm.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.SetGreaterThanUnsignedImm]
      val v1 = ctx.getReg(i.src)
      val result = if java.lang.Long.compareUnsigned(v1, i.imm) > 0 then 1L else 0L
      ctx.setReg64(i.dst, result)
      ctx.advance()

    arr(Opcode.SetGreaterThanSignedImm.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.SetGreaterThanSignedImm]
      val v1 = ctx.getReg(i.src)
      val result = if v1 > i.imm then 1L else 0L
      ctx.setReg64(i.dst, result)
      ctx.advance()

    // ========================================================================
    // Conditional Move with Immediate
    // ========================================================================
    arr(Opcode.CmovIfZeroImm.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.CmovIfZeroImm]
      if ctx.getReg(i.src) == 0L then
        ctx.setReg64(i.dst, i.imm)
      ctx.advance()

    arr(Opcode.CmovIfNotZeroImm.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.CmovIfNotZeroImm]
      if ctx.getReg(i.src) != 0L then
        ctx.setReg64(i.dst, i.imm)
      ctx.advance()

    // ========================================================================
    // Branch Instructions with Immediate
    // ========================================================================
    arr(Opcode.BranchEqImm.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchEqImm]
      ctx.branch(ctx.getReg(i.reg) == i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchNotEqImm.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchNotEqImm]
      ctx.branch(ctx.getReg(i.reg) != i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchLessUnsignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchLessUnsignedImm]
      ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(i.reg), i.imm) < 0, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchLessSignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchLessSignedImm]
      ctx.branch(ctx.getReg(i.reg) < i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchGreaterOrEqualUnsignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchGreaterOrEqualUnsignedImm]
      ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(i.reg), i.imm) >= 0, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchGreaterOrEqualSignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchGreaterOrEqualSignedImm]
      ctx.branch(ctx.getReg(i.reg) >= i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchLessOrEqualUnsignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchLessOrEqualUnsignedImm]
      ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(i.reg), i.imm) <= 0, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchLessOrEqualSignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchLessOrEqualSignedImm]
      ctx.branch(ctx.getReg(i.reg) <= i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchGreaterSignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchGreaterSignedImm]
      ctx.branch(ctx.getReg(i.reg) > i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchGreaterUnsignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchGreaterUnsignedImm]
      ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(i.reg), i.imm) > 0, pc, i.offset.toInt, nextPc)

    // ========================================================================
    // Branch Instructions with Register
    // ========================================================================
    arr(Opcode.BranchEq.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchEq]
      ctx.branch(ctx.getReg(i.r1) == ctx.getReg(i.r2), pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchNotEq.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchNotEq]
      ctx.branch(ctx.getReg(i.r1) != ctx.getReg(i.r2), pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchLessUnsigned.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchLessUnsigned]
      ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(i.r1), ctx.getReg(i.r2)) < 0, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchLessSigned.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchLessSigned]
      ctx.branch(ctx.getReg(i.r1) < ctx.getReg(i.r2), pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchGreaterOrEqualUnsigned.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchGreaterOrEqualUnsigned]
      ctx.branch(java.lang.Long.compareUnsigned(ctx.getReg(i.r1), ctx.getReg(i.r2)) >= 0, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchGreaterOrEqualSigned.value) = (instr, ctx, pc, nextPc) =>
      val i = instr.asInstanceOf[Instruction.BranchGreaterOrEqualSigned]
      ctx.branch(ctx.getReg(i.r1) >= ctx.getReg(i.r2), pc, i.offset.toInt, nextPc)

    // ========================================================================
    // Three-Register Arithmetic (32-bit)
    // ========================================================================
    arr(Opcode.Add32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Add32]
      ctx.add32(i.d, i.s1, i.s2)

    arr(Opcode.Sub32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Sub32]
      ctx.sub32(i.d, i.s1, i.s2)

    arr(Opcode.Mul32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Mul32]
      ctx.mul32(i.d, i.s1, i.s2)

    arr(Opcode.DivUnsigned32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.DivUnsigned32]
      val v1 = ctx.getReg(i.s1).toInt
      val v2 = ctx.getReg(i.s2).toInt
      val result = if v2 == 0 then -1 else Integer.divideUnsigned(v1, v2)
      ctx.setReg32Int(i.d, result)
      ctx.advance()

    arr(Opcode.DivSigned32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.DivSigned32]
      val v1 = ctx.getReg(i.s1).toInt
      val v2 = ctx.getReg(i.s2).toInt
      val result = if v2 == 0 then -1
      else if v1 == Int.MinValue && v2 == -1 then Int.MinValue
      else v1 / v2
      ctx.setReg32Int(i.d, result)
      ctx.advance()

    arr(Opcode.RemUnsigned32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RemUnsigned32]
      val v1 = ctx.getReg(i.s1).toInt
      val v2 = ctx.getReg(i.s2).toInt
      val result = if v2 == 0 then v1 else Integer.remainderUnsigned(v1, v2)
      ctx.setReg32Int(i.d, result)
      ctx.advance()

    arr(Opcode.RemSigned32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RemSigned32]
      val v1 = ctx.getReg(i.s1).toInt
      val v2 = ctx.getReg(i.s2).toInt
      val result = if v2 == 0 then v1
      else if v1 == Int.MinValue && v2 == -1 then 0
      else v1 % v2
      ctx.setReg32Int(i.d, result)
      ctx.advance()

    // ========================================================================
    // Three-Register Arithmetic (64-bit)
    // ========================================================================
    arr(Opcode.Add64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Add64]
      ctx.add64(i.d, i.s1, i.s2)

    arr(Opcode.Sub64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Sub64]
      ctx.sub64(i.d, i.s1, i.s2)

    arr(Opcode.Mul64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Mul64]
      ctx.mul64(i.d, i.s1, i.s2)

    arr(Opcode.DivUnsigned64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.DivUnsigned64]
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      val result = if v2 == 0L then -1L else java.lang.Long.divideUnsigned(v1, v2)
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.DivSigned64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.DivSigned64]
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      val result = if v2 == 0L then -1L
      else if v1 == Long.MinValue && v2 == -1L then Long.MinValue
      else v1 / v2
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.RemUnsigned64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RemUnsigned64]
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      val result = if v2 == 0L then v1 else java.lang.Long.remainderUnsigned(v1, v2)
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.RemSigned64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RemSigned64]
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      val result = if v2 == 0L then v1
      else if v1 == Long.MinValue && v2 == -1L then 0L
      else v1 % v2
      ctx.setReg64(i.d, result)
      ctx.advance()

    // ========================================================================
    // Three-Register Bitwise
    // ========================================================================
    arr(Opcode.And.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.And]
      ctx.and64(i.d, i.s1, i.s2)

    arr(Opcode.Or.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Or]
      ctx.or64(i.d, i.s1, i.s2)

    arr(Opcode.Xor.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Xor]
      ctx.xor64(i.d, i.s1, i.s2)

    arr(Opcode.AndInverted.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.AndInverted]
      ctx.op3_64(i.d, i.s1, i.s2)((a, b) => a & ~b)

    arr(Opcode.OrInverted.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.OrInverted]
      ctx.op3_64(i.d, i.s1, i.s2)((a, b) => a | ~b)

    arr(Opcode.Xnor.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Xnor]
      ctx.op3_64(i.d, i.s1, i.s2)((a, b) => ~(a ^ b))

    // ========================================================================
    // Three-Register Shifts (32-bit)
    // ========================================================================
    arr(Opcode.ShiftLogicalLeft32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalLeft32]
      ctx.shl32(i.d, i.s1, i.s2)

    arr(Opcode.ShiftLogicalRight32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalRight32]
      ctx.shr32(i.d, i.s1, i.s2)

    arr(Opcode.ShiftArithmeticRight32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftArithmeticRight32]
      ctx.sar32(i.d, i.s1, i.s2)

    // ========================================================================
    // Three-Register Shifts (64-bit)
    // ========================================================================
    arr(Opcode.ShiftLogicalLeft64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalLeft64]
      ctx.shl64(i.d, i.s1, i.s2)

    arr(Opcode.ShiftLogicalRight64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftLogicalRight64]
      ctx.shr64(i.d, i.s1, i.s2)

    arr(Opcode.ShiftArithmeticRight64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.ShiftArithmeticRight64]
      ctx.sar64(i.d, i.s1, i.s2)

    // ========================================================================
    // Three-Register Rotates
    // ========================================================================
    arr(Opcode.RotateLeft32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RotateLeft32]
      val v = ctx.getReg(i.s1).toInt
      val shift = ctx.getReg(i.s2).toInt & 31
      ctx.setReg32Int(i.d, Integer.rotateLeft(v, shift))
      ctx.advance()

    arr(Opcode.RotateLeft64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RotateLeft64]
      val v = ctx.getReg(i.s1)
      val shift = ctx.getReg(i.s2).toInt & 63
      ctx.setReg64(i.d, java.lang.Long.rotateLeft(v, shift))
      ctx.advance()

    arr(Opcode.RotateRight32.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RotateRight32]
      val v = ctx.getReg(i.s1).toInt
      val shift = ctx.getReg(i.s2).toInt & 31
      ctx.setReg32Int(i.d, Integer.rotateRight(v, shift))
      ctx.advance()

    arr(Opcode.RotateRight64.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.RotateRight64]
      val v = ctx.getReg(i.s1)
      val shift = ctx.getReg(i.s2).toInt & 63
      ctx.setReg64(i.d, java.lang.Long.rotateRight(v, shift))
      ctx.advance()

    // ========================================================================
    // Three-Register Comparisons
    // ========================================================================
    arr(Opcode.SetLessThanUnsigned.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.SetLessThanUnsigned]
      val result = if java.lang.Long.compareUnsigned(ctx.getReg(i.s1), ctx.getReg(i.s2)) < 0 then 1L else 0L
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.SetLessThanSigned.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.SetLessThanSigned]
      val result = if ctx.getReg(i.s1) < ctx.getReg(i.s2) then 1L else 0L
      ctx.setReg64(i.d, result)
      ctx.advance()

    // ========================================================================
    // Three-Register Conditional Moves
    // ========================================================================
    arr(Opcode.CmovIfZero.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.CmovIfZero]
      if ctx.getReg(i.s2) == 0L then
        ctx.setReg64(i.d, ctx.getReg(i.s1))
      ctx.advance()

    arr(Opcode.CmovIfNotZero.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.CmovIfNotZero]
      if ctx.getReg(i.s2) != 0L then
        ctx.setReg64(i.d, ctx.getReg(i.s1))
      ctx.advance()

    // ========================================================================
    // Three-Register Min/Max
    // ========================================================================
    arr(Opcode.Maximum.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Maximum]
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      ctx.setReg64(i.d, if v1 > v2 then v1 else v2)
      ctx.advance()

    arr(Opcode.MaximumUnsigned.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.MaximumUnsigned]
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      ctx.setReg64(i.d, if java.lang.Long.compareUnsigned(v1, v2) > 0 then v1 else v2)
      ctx.advance()

    arr(Opcode.Minimum.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.Minimum]
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      ctx.setReg64(i.d, if v1 < v2 then v1 else v2)
      ctx.advance()

    arr(Opcode.MinimumUnsigned.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.MinimumUnsigned]
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      ctx.setReg64(i.d, if java.lang.Long.compareUnsigned(v1, v2) < 0 then v1 else v2)
      ctx.advance()

    // ========================================================================
    // Three-Register Multiply Upper
    // ========================================================================
    arr(Opcode.MulUpperSignedSigned.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.MulUpperSignedSigned]
      val result = UInt128.mulUpperSignedSigned(ctx.getReg(i.s1), ctx.getReg(i.s2))
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.MulUpperUnsignedUnsigned.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.MulUpperUnsignedUnsigned]
      val result = UInt128.mulUpperUnsigned(ctx.getReg(i.s1), ctx.getReg(i.s2))
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.MulUpperSignedUnsigned.value) = (instr, ctx, _, _) =>
      val i = instr.asInstanceOf[Instruction.MulUpperSignedUnsigned]
      val result = UInt128.mulUpperSignedUnsigned(ctx.getReg(i.s1), ctx.getReg(i.s2))
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr

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
    handlers(instruction.opcode.value)(instruction, ctx, pc, nextPc)
