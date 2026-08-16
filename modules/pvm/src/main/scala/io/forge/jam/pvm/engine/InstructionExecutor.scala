package io.forge.jam.pvm.engine

import spire.math.UInt
import io.forge.jam.pvm.{Instruction, Opcode}
import io.forge.jam.pvm.types.*

/** Executes PVM instructions using an indexed handler array for O(1) dispatch.
  */
object InstructionExecutor:
  private trait Handler:
    def run(
        instr: Instruction,
        ctx: ExecutionContext,
        pc: ProgramCounter,
        nextPc: ProgramCounter
    ): Int
  private inline def expect[I <: Instruction](instr: Instruction): I =
    instr.asInstanceOf[I]

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
    arr(Opcode.Fallthrough.value) = (_, ctx, _, nextPc) =>
      ctx.resolveFallthrough(nextPc)

    // ========================================================================
    // Jump Instructions
    // ========================================================================
    arr(Opcode.Jump.value) = (instr, ctx, _, _) =>
      val target = expect[Instruction.Jump](instr).target
      ctx.resolveJump(ProgramCounter(target.toInt))

    arr(Opcode.JumpIndirect.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.JumpIndirect](instr)
      val addr = (ctx.getReg(i.reg) + i.offset).toInt
      ctx.jumpIndirectInt(pc, addr)

    arr(Opcode.Ecalli.value) = (instr, ctx, pc, nextPc) =>
      val hostId = expect[Instruction.Ecalli](instr).hostId
      ctx.ecalli(pc, nextPc, UInt(hostId.toInt))

    // ========================================================================
    // Load Immediate Instructions
    // ========================================================================
    arr(Opcode.LoadImm.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.LoadImm](instr)
      ctx.setReg32Int(i.reg, i.imm.toInt)
      ctx.advance()

    arr(Opcode.LoadImm64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.LoadImm64](instr)
      ctx.setReg64(i.reg, i.imm)
      ctx.advance()

    arr(Opcode.LoadImmAndJump.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.LoadImmAndJump](instr)
      ctx.setReg32Int(i.reg, i.imm.toInt)
      ctx.resolveJump(ProgramCounter(i.target.toInt))

    arr(Opcode.LoadImmAndJumpIndirect.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadImmAndJumpIndirect](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.setReg32Int(i.dst, i.imm.toInt)
      ctx.jumpIndirectInt(pc, addr)

    // ========================================================================
    // Memory Load Instructions (Direct)
    // ========================================================================
    arr(Opcode.LoadU8.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadU8](instr)
      ctx.loadU8Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadI8.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadI8](instr)
      ctx.loadI8Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadU16.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadU16](instr)
      ctx.loadU16Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadI16.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadI16](instr)
      ctx.loadI16Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadU32.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadU32](instr)
      ctx.loadU32Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadI32.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadI32](instr)
      ctx.loadI32Int(pc, i.reg, i.address.toInt)

    arr(Opcode.LoadU64.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadU64](instr)
      ctx.loadU64Int(pc, i.reg, i.address.toInt)

    // ========================================================================
    // Memory Load Instructions (Indirect)
    // ========================================================================
    arr(Opcode.LoadIndirectU8.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadIndirectU8](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadU8Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectI8.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadIndirectI8](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadI8Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectU16.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadIndirectU16](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadU16Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectI16.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadIndirectI16](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadI16Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectU32.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadIndirectU32](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadU32Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectI32.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadIndirectI32](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadI32Int(pc, i.dst, addr)

    arr(Opcode.LoadIndirectU64.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.LoadIndirectU64](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.loadU64Int(pc, i.dst, addr)

    // ========================================================================
    // Memory Store Instructions (Direct)
    // ========================================================================
    arr(Opcode.StoreU8.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreU8](instr)
      ctx.storeU8Int(pc, i.reg, i.address.toInt)

    arr(Opcode.StoreU16.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreU16](instr)
      ctx.storeU16Int(pc, i.reg, i.address.toInt)

    arr(Opcode.StoreU32.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreU32](instr)
      ctx.storeU32Int(pc, i.reg, i.address.toInt)

    arr(Opcode.StoreU64.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreU64](instr)
      ctx.storeU64Int(pc, i.reg, i.address.toInt)

    // ========================================================================
    // Memory Store Instructions (Immediate)
    // ========================================================================
    arr(Opcode.StoreImmU8.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreImmU8](instr)
      ctx.storeImmU8Int(pc, i.address.toInt, i.value.toByte)

    arr(Opcode.StoreImmU16.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreImmU16](instr)
      ctx.storeImmU16Int(pc, i.address.toInt, i.value.toShort)

    arr(Opcode.StoreImmU32.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreImmU32](instr)
      ctx.storeImmU32Int(pc, i.address.toInt, i.value.toInt)

    arr(Opcode.StoreImmU64.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreImmU64](instr)
      ctx.storeImmU64Int(pc, i.address.toInt, i.value)

    // ========================================================================
    // Memory Store Instructions (Indirect)
    // ========================================================================
    arr(Opcode.StoreIndirectU8.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreIndirectU8](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.storeU8Int(pc, i.src, addr)

    arr(Opcode.StoreIndirectU16.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreIndirectU16](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.storeU16Int(pc, i.src, addr)

    arr(Opcode.StoreIndirectU32.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreIndirectU32](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.storeU32Int(pc, i.src, addr)

    arr(Opcode.StoreIndirectU64.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreIndirectU64](instr)
      val addr = (ctx.getReg(i.base) + i.offset).toInt
      ctx.storeU64Int(pc, i.src, addr)

    // ========================================================================
    // Memory Store Instructions (Immediate Indirect)
    // ========================================================================
    arr(Opcode.StoreImmIndirectU8.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreImmIndirectU8](instr)
      val addr = (ctx.getReg(i.reg) + i.offset).toInt
      ctx.storeImmU8Int(pc, addr, i.value.toByte)

    arr(Opcode.StoreImmIndirectU16.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreImmIndirectU16](instr)
      val addr = (ctx.getReg(i.reg) + i.offset).toInt
      ctx.storeImmU16Int(pc, addr, i.value.toShort)

    arr(Opcode.StoreImmIndirectU32.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreImmIndirectU32](instr)
      val addr = (ctx.getReg(i.reg) + i.offset).toInt
      ctx.storeImmU32Int(pc, addr, i.value.toInt)

    arr(Opcode.StoreImmIndirectU64.value) = (instr, ctx, pc, _) =>
      val i = expect[Instruction.StoreImmIndirectU64](instr)
      val addr = (ctx.getReg(i.reg) + i.offset).toInt
      ctx.storeImmU64Int(pc, addr, i.value)

    // ========================================================================
    // Register Move and Special
    // ========================================================================
    arr(Opcode.MoveReg.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.MoveReg](instr)
      ctx.setReg64(i.dst, ctx.getReg(i.src))
      ctx.advance()

    arr(Opcode.Sbrk.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Sbrk](instr)
      val size = UInt(ctx.getReg(i.src).toInt)
      ctx.sbrk(i.dst, size)

    // ========================================================================
    // Bit Counting Instructions
    // ========================================================================
    arr(Opcode.CountLeadingZeroBits32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.CountLeadingZeroBits32](instr)
      val v = ctx.getReg(i.src).toInt
      ctx.setReg32Int(i.dst, Integer.numberOfLeadingZeros(v))
      ctx.advance()

    arr(Opcode.CountLeadingZeroBits64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.CountLeadingZeroBits64](instr)
      val v = ctx.getReg(i.src)
      ctx.setReg64(i.dst, java.lang.Long.numberOfLeadingZeros(v))
      ctx.advance()

    arr(Opcode.CountTrailingZeroBits32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.CountTrailingZeroBits32](instr)
      val v = ctx.getReg(i.src).toInt
      ctx.setReg32Int(i.dst, Integer.numberOfTrailingZeros(v))
      ctx.advance()

    arr(Opcode.CountTrailingZeroBits64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.CountTrailingZeroBits64](instr)
      val v = ctx.getReg(i.src)
      ctx.setReg64(i.dst, java.lang.Long.numberOfTrailingZeros(v))
      ctx.advance()

    arr(Opcode.CountSetBits32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.CountSetBits32](instr)
      val v = ctx.getReg(i.src).toInt
      ctx.setReg32Int(i.dst, Integer.bitCount(v))
      ctx.advance()

    arr(Opcode.CountSetBits64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.CountSetBits64](instr)
      val v = ctx.getReg(i.src)
      ctx.setReg64(i.dst, java.lang.Long.bitCount(v))
      ctx.advance()

    // ========================================================================
    // Sign/Zero Extension
    // ========================================================================
    arr(Opcode.SignExtend8.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.SignExtend8](instr)
      val v = ctx.getReg(i.src).toByte.toLong
      ctx.setReg64(i.dst, v)
      ctx.advance()

    arr(Opcode.SignExtend16.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.SignExtend16](instr)
      val v = ctx.getReg(i.src).toShort.toLong
      ctx.setReg64(i.dst, v)
      ctx.advance()

    arr(Opcode.ZeroExtend16.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ZeroExtend16](instr)
      val v = ctx.getReg(i.src) & 0xffffL
      ctx.setReg64(i.dst, v)
      ctx.advance()

    arr(Opcode.ReverseByte.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ReverseByte](instr)
      val v = ctx.getReg(i.src)
      ctx.setReg64(i.dst, java.lang.Long.reverseBytes(v))
      ctx.advance()

    // ========================================================================
    // Arithmetic with Immediate (32-bit)
    // ========================================================================
    arr(Opcode.AddImm32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.AddImm32](instr)
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)(_ + _)

    arr(Opcode.NegateAndAddImm32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.NegateAndAddImm32](instr)
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)((s, imm) => imm - s)

    arr(Opcode.MulImm32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.MulImm32](instr)
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)(_ * _)

    // ========================================================================
    // Arithmetic with Immediate (64-bit)
    // ========================================================================
    arr(Opcode.AddImm64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.AddImm64](instr)
      ctx.addImm64(i.dst, i.src, i.imm)

    arr(Opcode.NegateAndAddImm64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.NegateAndAddImm64](instr)
      ctx.op2Imm64(i.dst, i.src, i.imm)((s, imm) => imm - s)

    arr(Opcode.MulImm64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.MulImm64](instr)
      ctx.op2Imm64(i.dst, i.src, i.imm)(_ * _)

    // ========================================================================
    // Bitwise with Immediate
    // ========================================================================
    arr(Opcode.AndImm.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.AndImm](instr)
      ctx.andImm(i.dst, i.src, i.imm)

    arr(Opcode.OrImm.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.OrImm](instr)
      ctx.orImm(i.dst, i.src, i.imm)

    arr(Opcode.XorImm.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.XorImm](instr)
      ctx.xorImm(i.dst, i.src, i.imm)

    // ========================================================================
    // Shift with Immediate (32-bit)
    // ========================================================================
    arr(Opcode.ShiftLogicalLeftImm32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalLeftImm32](instr)
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt & 31)((v, s) => v << s)

    arr(Opcode.ShiftLogicalRightImm32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalRightImm32](instr)
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt & 31)((v, s) => v >>> s)

    arr(Opcode.ShiftArithmeticRightImm32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftArithmeticRightImm32](instr)
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt & 31)((v, s) => v >> s)

    arr(Opcode.ShiftLogicalLeftImmAlt32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalLeftImmAlt32](instr)
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)((shift, v) => v << (shift & 31))

    arr(Opcode.ShiftLogicalRightImmAlt32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalRightImmAlt32](instr)
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)((shift, v) => v >>> (shift & 31))

    arr(Opcode.ShiftArithmeticRightImmAlt32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftArithmeticRightImmAlt32](instr)
      ctx.op2Imm32(i.dst, i.src, i.imm.toInt)((shift, v) => v >> (shift & 31))

    // ========================================================================
    // Shift with Immediate (64-bit)
    // ========================================================================
    arr(Opcode.ShiftLogicalLeftImm64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalLeftImm64](instr)
      ctx.op2Imm64(i.dst, i.src, i.imm & 63)((v, s) => v << s.toInt)

    arr(Opcode.ShiftLogicalRightImm64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalRightImm64](instr)
      ctx.op2Imm64(i.dst, i.src, i.imm & 63)((v, s) => v >>> s.toInt)

    arr(Opcode.ShiftArithmeticRightImm64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftArithmeticRightImm64](instr)
      ctx.op2Imm64(i.dst, i.src, i.imm & 63)((v, s) => v >> s.toInt)

    arr(Opcode.ShiftLogicalLeftImmAlt64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalLeftImmAlt64](instr)
      ctx.op2Imm64(i.dst, i.src, i.imm)((shift, v) => v << (shift.toInt & 63))

    arr(Opcode.ShiftLogicalRightImmAlt64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalRightImmAlt64](instr)
      ctx.op2Imm64(i.dst, i.src, i.imm)((shift, v) => v >>> (shift.toInt & 63))

    arr(Opcode.ShiftArithmeticRightImmAlt64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftArithmeticRightImmAlt64](instr)
      ctx.op2Imm64(i.dst, i.src, i.imm)((shift, v) => v >> (shift.toInt & 63))

    // ========================================================================
    // Rotate with Immediate
    // ========================================================================
    arr(Opcode.RotateRightImm32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RotateRightImm32](instr)
      val v = ctx.getReg(i.src).toInt
      val shift = i.imm.toInt & 31
      val result = Integer.rotateRight(v, shift)
      ctx.setReg32Int(i.dst, result)
      ctx.advance()

    arr(Opcode.RotateRightImm64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RotateRightImm64](instr)
      val v = ctx.getReg(i.src)
      val shift = i.imm.toInt & 63
      val result = java.lang.Long.rotateRight(v, shift)
      ctx.setReg64(i.dst, result)
      ctx.advance()

    arr(Opcode.RotateRightImmAlt32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RotateRightImmAlt32](instr)
      val shift = ctx.getReg(i.src).toInt & 31
      val v = i.imm.toInt
      val result = Integer.rotateRight(v, shift)
      ctx.setReg32Int(i.dst, result)
      ctx.advance()

    arr(Opcode.RotateRightImmAlt64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RotateRightImmAlt64](instr)
      val shift = ctx.getReg(i.src).toInt & 63
      val result = java.lang.Long.rotateRight(i.imm, shift)
      ctx.setReg64(i.dst, result)
      ctx.advance()

    // ========================================================================
    // Comparison with Immediate
    // ========================================================================
    arr(Opcode.SetLessThanUnsignedImm.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.SetLessThanUnsignedImm](instr)
      val v1 = ctx.getReg(i.src)
      val result =
        if java.lang.Long.compareUnsigned(v1, i.imm) < 0 then 1L else 0L
      ctx.setReg64(i.dst, result)
      ctx.advance()

    arr(Opcode.SetLessThanSignedImm.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.SetLessThanSignedImm](instr)
      val v1 = ctx.getReg(i.src)
      val result = if v1 < i.imm then 1L else 0L
      ctx.setReg64(i.dst, result)
      ctx.advance()

    arr(Opcode.SetGreaterThanUnsignedImm.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.SetGreaterThanUnsignedImm](instr)
      val v1 = ctx.getReg(i.src)
      val result =
        if java.lang.Long.compareUnsigned(v1, i.imm) > 0 then 1L else 0L
      ctx.setReg64(i.dst, result)
      ctx.advance()

    arr(Opcode.SetGreaterThanSignedImm.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.SetGreaterThanSignedImm](instr)
      val v1 = ctx.getReg(i.src)
      val result = if v1 > i.imm then 1L else 0L
      ctx.setReg64(i.dst, result)
      ctx.advance()

    // ========================================================================
    // Conditional Move with Immediate
    // ========================================================================
    arr(Opcode.CmovIfZeroImm.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.CmovIfZeroImm](instr)
      if ctx.getReg(i.src) == 0L then ctx.setReg64(i.dst, i.imm)
      ctx.advance()

    arr(Opcode.CmovIfNotZeroImm.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.CmovIfNotZeroImm](instr)
      if ctx.getReg(i.src) != 0L then ctx.setReg64(i.dst, i.imm)
      ctx.advance()

    // ========================================================================
    // Branch Instructions with Immediate
    // ========================================================================
    arr(Opcode.BranchEqImm.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchEqImm](instr)
      ctx.branch(ctx.getReg(i.reg) == i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchNotEqImm.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchNotEqImm](instr)
      ctx.branch(ctx.getReg(i.reg) != i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchLessUnsignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchLessUnsignedImm](instr)
      ctx.branch(
        java.lang.Long.compareUnsigned(ctx.getReg(i.reg), i.imm) < 0,
        pc,
        i.offset.toInt,
        nextPc
      )

    arr(Opcode.BranchLessSignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchLessSignedImm](instr)
      ctx.branch(ctx.getReg(i.reg) < i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchGreaterOrEqualUnsignedImm.value) =
      (instr, ctx, pc, nextPc) =>
        val i = expect[Instruction.BranchGreaterOrEqualUnsignedImm](instr)
        ctx.branch(
          java.lang.Long.compareUnsigned(ctx.getReg(i.reg), i.imm) >= 0,
          pc,
          i.offset.toInt,
          nextPc
        )

    arr(Opcode.BranchGreaterOrEqualSignedImm.value) =
      (instr, ctx, pc, nextPc) =>
        val i = expect[Instruction.BranchGreaterOrEqualSignedImm](instr)
        ctx.branch(ctx.getReg(i.reg) >= i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchLessOrEqualUnsignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchLessOrEqualUnsignedImm](instr)
      ctx.branch(
        java.lang.Long.compareUnsigned(ctx.getReg(i.reg), i.imm) <= 0,
        pc,
        i.offset.toInt,
        nextPc
      )

    arr(Opcode.BranchLessOrEqualSignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchLessOrEqualSignedImm](instr)
      ctx.branch(ctx.getReg(i.reg) <= i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchGreaterSignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchGreaterSignedImm](instr)
      ctx.branch(ctx.getReg(i.reg) > i.imm, pc, i.offset.toInt, nextPc)

    arr(Opcode.BranchGreaterUnsignedImm.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchGreaterUnsignedImm](instr)
      ctx.branch(
        java.lang.Long.compareUnsigned(ctx.getReg(i.reg), i.imm) > 0,
        pc,
        i.offset.toInt,
        nextPc
      )

    // ========================================================================
    // Branch Instructions with Register
    // ========================================================================
    arr(Opcode.BranchEq.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchEq](instr)
      ctx.branch(
        ctx.getReg(i.r1) == ctx.getReg(i.r2),
        pc,
        i.offset.toInt,
        nextPc
      )

    arr(Opcode.BranchNotEq.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchNotEq](instr)
      ctx.branch(
        ctx.getReg(i.r1) != ctx.getReg(i.r2),
        pc,
        i.offset.toInt,
        nextPc
      )

    arr(Opcode.BranchLessUnsigned.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchLessUnsigned](instr)
      ctx.branch(
        java.lang.Long.compareUnsigned(ctx.getReg(i.r1), ctx.getReg(i.r2)) < 0,
        pc,
        i.offset.toInt,
        nextPc
      )

    arr(Opcode.BranchLessSigned.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchLessSigned](instr)
      ctx.branch(
        ctx.getReg(i.r1) < ctx.getReg(i.r2),
        pc,
        i.offset.toInt,
        nextPc
      )

    arr(Opcode.BranchGreaterOrEqualUnsigned.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchGreaterOrEqualUnsigned](instr)
      ctx.branch(
        java.lang.Long.compareUnsigned(ctx.getReg(i.r1), ctx.getReg(i.r2)) >= 0,
        pc,
        i.offset.toInt,
        nextPc
      )

    arr(Opcode.BranchGreaterOrEqualSigned.value) = (instr, ctx, pc, nextPc) =>
      val i = expect[Instruction.BranchGreaterOrEqualSigned](instr)
      ctx.branch(
        ctx.getReg(i.r1) >= ctx.getReg(i.r2),
        pc,
        i.offset.toInt,
        nextPc
      )

    // ========================================================================
    // Three-Register Arithmetic (32-bit)
    // ========================================================================
    arr(Opcode.Add32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Add32](instr)
      ctx.add32(i.d, i.s1, i.s2)

    arr(Opcode.Sub32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Sub32](instr)
      ctx.sub32(i.d, i.s1, i.s2)

    arr(Opcode.Mul32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Mul32](instr)
      ctx.mul32(i.d, i.s1, i.s2)

    arr(Opcode.DivUnsigned32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.DivUnsigned32](instr)
      val v1 = ctx.getReg(i.s1).toInt
      val v2 = ctx.getReg(i.s2).toInt
      val result = if v2 == 0 then -1 else Integer.divideUnsigned(v1, v2)
      ctx.setReg32Int(i.d, result)
      ctx.advance()

    arr(Opcode.DivSigned32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.DivSigned32](instr)
      val v1 = ctx.getReg(i.s1).toInt
      val v2 = ctx.getReg(i.s2).toInt
      val result =
        if v2 == 0 then -1
        else if v1 == Int.MinValue && v2 == -1 then Int.MinValue
        else v1 / v2
      ctx.setReg32Int(i.d, result)
      ctx.advance()

    arr(Opcode.RemUnsigned32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RemUnsigned32](instr)
      val v1 = ctx.getReg(i.s1).toInt
      val v2 = ctx.getReg(i.s2).toInt
      val result = if v2 == 0 then v1 else Integer.remainderUnsigned(v1, v2)
      ctx.setReg32Int(i.d, result)
      ctx.advance()

    arr(Opcode.RemSigned32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RemSigned32](instr)
      val v1 = ctx.getReg(i.s1).toInt
      val v2 = ctx.getReg(i.s2).toInt
      val result =
        if v2 == 0 then v1
        else if v1 == Int.MinValue && v2 == -1 then 0
        else v1 % v2
      ctx.setReg32Int(i.d, result)
      ctx.advance()

    // ========================================================================
    // Three-Register Arithmetic (64-bit)
    // ========================================================================
    arr(Opcode.Add64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Add64](instr)
      ctx.add64(i.d, i.s1, i.s2)

    arr(Opcode.Sub64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Sub64](instr)
      ctx.sub64(i.d, i.s1, i.s2)

    arr(Opcode.Mul64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Mul64](instr)
      ctx.mul64(i.d, i.s1, i.s2)

    arr(Opcode.DivUnsigned64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.DivUnsigned64](instr)
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      val result =
        if v2 == 0L then -1L else java.lang.Long.divideUnsigned(v1, v2)
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.DivSigned64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.DivSigned64](instr)
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      val result =
        if v2 == 0L then -1L
        else if v1 == Long.MinValue && v2 == -1L then Long.MinValue
        else v1 / v2
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.RemUnsigned64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RemUnsigned64](instr)
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      val result =
        if v2 == 0L then v1 else java.lang.Long.remainderUnsigned(v1, v2)
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.RemSigned64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RemSigned64](instr)
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      val result =
        if v2 == 0L then v1
        else if v1 == Long.MinValue && v2 == -1L then 0L
        else v1 % v2
      ctx.setReg64(i.d, result)
      ctx.advance()

    // ========================================================================
    // Three-Register Bitwise
    // ========================================================================
    arr(Opcode.And.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.And](instr)
      ctx.and64(i.d, i.s1, i.s2)

    arr(Opcode.Or.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Or](instr)
      ctx.or64(i.d, i.s1, i.s2)

    arr(Opcode.Xor.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Xor](instr)
      ctx.xor64(i.d, i.s1, i.s2)

    arr(Opcode.AndInverted.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.AndInverted](instr)
      ctx.op3_64(i.d, i.s1, i.s2)((a, b) => a & ~b)

    arr(Opcode.OrInverted.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.OrInverted](instr)
      ctx.op3_64(i.d, i.s1, i.s2)((a, b) => a | ~b)

    arr(Opcode.Xnor.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Xnor](instr)
      ctx.op3_64(i.d, i.s1, i.s2)((a, b) => ~(a ^ b))

    // ========================================================================
    // Three-Register Shifts (32-bit)
    // ========================================================================
    arr(Opcode.ShiftLogicalLeft32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalLeft32](instr)
      ctx.shl32(i.d, i.s1, i.s2)

    arr(Opcode.ShiftLogicalRight32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalRight32](instr)
      ctx.shr32(i.d, i.s1, i.s2)

    arr(Opcode.ShiftArithmeticRight32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftArithmeticRight32](instr)
      ctx.sar32(i.d, i.s1, i.s2)

    // ========================================================================
    // Three-Register Shifts (64-bit)
    // ========================================================================
    arr(Opcode.ShiftLogicalLeft64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalLeft64](instr)
      ctx.shl64(i.d, i.s1, i.s2)

    arr(Opcode.ShiftLogicalRight64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftLogicalRight64](instr)
      ctx.shr64(i.d, i.s1, i.s2)

    arr(Opcode.ShiftArithmeticRight64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.ShiftArithmeticRight64](instr)
      ctx.sar64(i.d, i.s1, i.s2)

    // ========================================================================
    // Three-Register Rotates
    // ========================================================================
    arr(Opcode.RotateLeft32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RotateLeft32](instr)
      val v = ctx.getReg(i.s1).toInt
      val shift = ctx.getReg(i.s2).toInt & 31
      ctx.setReg32Int(i.d, Integer.rotateLeft(v, shift))
      ctx.advance()

    arr(Opcode.RotateLeft64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RotateLeft64](instr)
      val v = ctx.getReg(i.s1)
      val shift = ctx.getReg(i.s2).toInt & 63
      ctx.setReg64(i.d, java.lang.Long.rotateLeft(v, shift))
      ctx.advance()

    arr(Opcode.RotateRight32.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RotateRight32](instr)
      val v = ctx.getReg(i.s1).toInt
      val shift = ctx.getReg(i.s2).toInt & 31
      ctx.setReg32Int(i.d, Integer.rotateRight(v, shift))
      ctx.advance()

    arr(Opcode.RotateRight64.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.RotateRight64](instr)
      val v = ctx.getReg(i.s1)
      val shift = ctx.getReg(i.s2).toInt & 63
      ctx.setReg64(i.d, java.lang.Long.rotateRight(v, shift))
      ctx.advance()

    // ========================================================================
    // Three-Register Comparisons
    // ========================================================================
    arr(Opcode.SetLessThanUnsigned.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.SetLessThanUnsigned](instr)
      val result =
        if java.lang.Long
            .compareUnsigned(ctx.getReg(i.s1), ctx.getReg(i.s2)) < 0
        then 1L
        else 0L
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.SetLessThanSigned.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.SetLessThanSigned](instr)
      val result = if ctx.getReg(i.s1) < ctx.getReg(i.s2) then 1L else 0L
      ctx.setReg64(i.d, result)
      ctx.advance()

    // ========================================================================
    // Three-Register Conditional Moves
    // ========================================================================
    arr(Opcode.CmovIfZero.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.CmovIfZero](instr)
      if ctx.getReg(i.s2) == 0L then ctx.setReg64(i.d, ctx.getReg(i.s1))
      ctx.advance()

    arr(Opcode.CmovIfNotZero.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.CmovIfNotZero](instr)
      if ctx.getReg(i.s2) != 0L then ctx.setReg64(i.d, ctx.getReg(i.s1))
      ctx.advance()

    // ========================================================================
    // Three-Register Min/Max
    // ========================================================================
    arr(Opcode.Maximum.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Maximum](instr)
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      ctx.setReg64(i.d, if v1 > v2 then v1 else v2)
      ctx.advance()

    arr(Opcode.MaximumUnsigned.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.MaximumUnsigned](instr)
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      ctx.setReg64(
        i.d,
        if java.lang.Long.compareUnsigned(v1, v2) > 0 then v1 else v2
      )
      ctx.advance()

    arr(Opcode.Minimum.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.Minimum](instr)
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      ctx.setReg64(i.d, if v1 < v2 then v1 else v2)
      ctx.advance()

    arr(Opcode.MinimumUnsigned.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.MinimumUnsigned](instr)
      val v1 = ctx.getReg(i.s1)
      val v2 = ctx.getReg(i.s2)
      ctx.setReg64(
        i.d,
        if java.lang.Long.compareUnsigned(v1, v2) < 0 then v1 else v2
      )
      ctx.advance()

    // ========================================================================
    // Three-Register Multiply Upper
    // ========================================================================
    arr(Opcode.MulUpperSignedSigned.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.MulUpperSignedSigned](instr)
      val result =
        UInt128.mulUpperSignedSigned(ctx.getReg(i.s1), ctx.getReg(i.s2))
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.MulUpperUnsignedUnsigned.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.MulUpperUnsignedUnsigned](instr)
      val result = UInt128.mulUpperUnsigned(ctx.getReg(i.s1), ctx.getReg(i.s2))
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr(Opcode.MulUpperSignedUnsigned.value) = (instr, ctx, _, _) =>
      val i = expect[Instruction.MulUpperSignedUnsigned](instr)
      val result =
        UInt128.mulUpperSignedUnsigned(ctx.getReg(i.s1), ctx.getReg(i.s2))
      ctx.setReg64(i.d, result)
      ctx.advance()

    arr

  /** Executes a single instruction.
    *
    * @param opcodeValue
    *   The cached opcode ordinal used to index the handler array
    * @param instruction
    *   The instruction to execute
    * @param ctx
    *   The execution context
    * @param pc
    *   The current program counter
    * @param nextPc
    *   The next program counter (for fallthrough)
    * @return
    *   the next compiled offset to continue execution, or a negative sentinel
    */
  def execute(
      opcodeValue: Int,
      instruction: Instruction,
      ctx: ExecutionContext,
      pc: ProgramCounter,
      nextPc: ProgramCounter
  ): Int =
    handlers(opcodeValue).run(instruction, ctx, pc, nextPc)
