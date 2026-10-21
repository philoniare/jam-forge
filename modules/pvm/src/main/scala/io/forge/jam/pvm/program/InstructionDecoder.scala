package io.forge.jam.pvm.program

import io.forge.jam.pvm.{Instruction, Opcode, PvmConstants}
import PvmConstants.{NumGeneralRegisters, MaxInstructionSize}

/**
 * Decodes PVM instructions from bytecode.
 *
 * Uses map-based dispatch to reduce code duplication. Instructions are grouped
 * by their operand pattern and decoded using shared helper functions.
 */
object InstructionDecoder:

  // ============================================================================
  // Dispatch Tables - Group opcodes by their operand pattern
  // ============================================================================

  private val arglessOpcodes: Map[Opcode, Instruction] = Map(
    Opcode.Panic -> Instruction.Panic,
    Opcode.Fallthrough -> Instruction.Fallthrough,
    Opcode.Unlikely -> Instruction.Unlikely
  )

  private val regs2Opcodes: Map[Opcode, (Int, Int) => Instruction] = Map(
    Opcode.MoveReg -> Instruction.MoveReg.apply,
    Opcode.CountSetBits32 -> Instruction.CountSetBits32.apply,
    Opcode.CountSetBits64 -> Instruction.CountSetBits64.apply,
    Opcode.CountLeadingZeroBits32 -> Instruction.CountLeadingZeroBits32.apply,
    Opcode.CountLeadingZeroBits64 -> Instruction.CountLeadingZeroBits64.apply,
    Opcode.CountTrailingZeroBits32 -> Instruction.CountTrailingZeroBits32.apply,
    Opcode.CountTrailingZeroBits64 -> Instruction.CountTrailingZeroBits64.apply,
    Opcode.SignExtend8 -> Instruction.SignExtend8.apply,
    Opcode.SignExtend16 -> Instruction.SignExtend16.apply,
    Opcode.ZeroExtend16 -> Instruction.ZeroExtend16.apply,
    Opcode.ReverseByte -> Instruction.ReverseByte.apply
  )

  private val regs3Opcodes: Map[Opcode, (Int, Int, Int) => Instruction] = Map(
    Opcode.Add32 -> Instruction.Add32.apply, Opcode.Sub32 -> Instruction.Sub32.apply,
    Opcode.Mul32 -> Instruction.Mul32.apply, Opcode.DivUnsigned32 -> Instruction.DivUnsigned32.apply,
    Opcode.DivSigned32 -> Instruction.DivSigned32.apply, Opcode.RemUnsigned32 -> Instruction.RemUnsigned32.apply,
    Opcode.RemSigned32 -> Instruction.RemSigned32.apply, Opcode.ShiftLogicalLeft32 -> Instruction.ShiftLogicalLeft32.apply,
    Opcode.ShiftLogicalRight32 -> Instruction.ShiftLogicalRight32.apply, Opcode.ShiftArithmeticRight32 -> Instruction.ShiftArithmeticRight32.apply,
    Opcode.Add64 -> Instruction.Add64.apply, Opcode.Sub64 -> Instruction.Sub64.apply,
    Opcode.Mul64 -> Instruction.Mul64.apply, Opcode.DivUnsigned64 -> Instruction.DivUnsigned64.apply,
    Opcode.DivSigned64 -> Instruction.DivSigned64.apply, Opcode.RemUnsigned64 -> Instruction.RemUnsigned64.apply,
    Opcode.RemSigned64 -> Instruction.RemSigned64.apply, Opcode.ShiftLogicalLeft64 -> Instruction.ShiftLogicalLeft64.apply,
    Opcode.ShiftLogicalRight64 -> Instruction.ShiftLogicalRight64.apply, Opcode.ShiftArithmeticRight64 -> Instruction.ShiftArithmeticRight64.apply,
    Opcode.And -> Instruction.And.apply, Opcode.Xor -> Instruction.Xor.apply, Opcode.Or -> Instruction.Or.apply,
    Opcode.AndInverted -> Instruction.AndInverted.apply, Opcode.OrInverted -> Instruction.OrInverted.apply, Opcode.Xnor -> Instruction.Xnor.apply,
    Opcode.MulUpperSignedSigned -> Instruction.MulUpperSignedSigned.apply,
    Opcode.MulUpperUnsignedUnsigned -> Instruction.MulUpperUnsignedUnsigned.apply,
    Opcode.MulUpperSignedUnsigned -> Instruction.MulUpperSignedUnsigned.apply,
    Opcode.SetLessThanUnsigned -> Instruction.SetLessThanUnsigned.apply, Opcode.SetLessThanSigned -> Instruction.SetLessThanSigned.apply,
    Opcode.CmovIfZero -> Instruction.CmovIfZero.apply, Opcode.CmovIfNotZero -> Instruction.CmovIfNotZero.apply,
    Opcode.RotateRight32 -> Instruction.RotateRight32.apply, Opcode.RotateRight64 -> Instruction.RotateRight64.apply,
    Opcode.RotateLeft32 -> Instruction.RotateLeft32.apply, Opcode.RotateLeft64 -> Instruction.RotateLeft64.apply,
    Opcode.Maximum -> Instruction.Maximum.apply, Opcode.MaximumUnsigned -> Instruction.MaximumUnsigned.apply,
    Opcode.Minimum -> Instruction.Minimum.apply, Opcode.MinimumUnsigned -> Instruction.MinimumUnsigned.apply
  )

  private val regImmOpcodes: Map[Opcode, (Int, Long) => Instruction] = Map(
    Opcode.JumpIndirect -> Instruction.JumpIndirect.apply, Opcode.LoadImm -> Instruction.LoadImm.apply,
    Opcode.LoadU8 -> Instruction.LoadU8.apply, Opcode.LoadI8 -> Instruction.LoadI8.apply,
    Opcode.LoadU16 -> Instruction.LoadU16.apply, Opcode.LoadI16 -> Instruction.LoadI16.apply,
    Opcode.LoadU32 -> Instruction.LoadU32.apply, Opcode.LoadI32 -> Instruction.LoadI32.apply,
    Opcode.LoadU64 -> Instruction.LoadU64.apply, Opcode.StoreU8 -> Instruction.StoreU8.apply,
    Opcode.StoreU16 -> Instruction.StoreU16.apply, Opcode.StoreU32 -> Instruction.StoreU32.apply,
    Opcode.StoreU64 -> Instruction.StoreU64.apply
  )

  private val regs2ImmOpcodes: Map[Opcode, (Int, Int, Long) => Instruction] = Map(
    Opcode.AddImm32 -> Instruction.AddImm32.apply, Opcode.AddImm64 -> Instruction.AddImm64.apply,
    Opcode.MulImm32 -> Instruction.MulImm32.apply, Opcode.MulImm64 -> Instruction.MulImm64.apply,
    Opcode.NegateAndAddImm32 -> Instruction.NegateAndAddImm32.apply, Opcode.NegateAndAddImm64 -> Instruction.NegateAndAddImm64.apply,
    Opcode.AndImm -> Instruction.AndImm.apply, Opcode.XorImm -> Instruction.XorImm.apply, Opcode.OrImm -> Instruction.OrImm.apply,
    Opcode.SetLessThanUnsignedImm -> Instruction.SetLessThanUnsignedImm.apply, Opcode.SetLessThanSignedImm -> Instruction.SetLessThanSignedImm.apply,
    Opcode.SetGreaterThanUnsignedImm -> Instruction.SetGreaterThanUnsignedImm.apply, Opcode.SetGreaterThanSignedImm -> Instruction.SetGreaterThanSignedImm.apply,
    Opcode.ShiftLogicalLeftImm32 -> Instruction.ShiftLogicalLeftImm32.apply, Opcode.ShiftLogicalLeftImm64 -> Instruction.ShiftLogicalLeftImm64.apply,
    Opcode.ShiftLogicalRightImm32 -> Instruction.ShiftLogicalRightImm32.apply, Opcode.ShiftLogicalRightImm64 -> Instruction.ShiftLogicalRightImm64.apply,
    Opcode.ShiftArithmeticRightImm32 -> Instruction.ShiftArithmeticRightImm32.apply, Opcode.ShiftArithmeticRightImm64 -> Instruction.ShiftArithmeticRightImm64.apply,
    Opcode.ShiftLogicalLeftImmAlt32 -> Instruction.ShiftLogicalLeftImmAlt32.apply, Opcode.ShiftLogicalLeftImmAlt64 -> Instruction.ShiftLogicalLeftImmAlt64.apply,
    Opcode.ShiftLogicalRightImmAlt32 -> Instruction.ShiftLogicalRightImmAlt32.apply, Opcode.ShiftLogicalRightImmAlt64 -> Instruction.ShiftLogicalRightImmAlt64.apply,
    Opcode.ShiftArithmeticRightImmAlt32 -> Instruction.ShiftArithmeticRightImmAlt32.apply, Opcode.ShiftArithmeticRightImmAlt64 -> Instruction.ShiftArithmeticRightImmAlt64.apply,
    Opcode.CmovIfZeroImm -> Instruction.CmovIfZeroImm.apply, Opcode.CmovIfNotZeroImm -> Instruction.CmovIfNotZeroImm.apply,
    Opcode.RotateRightImm32 -> Instruction.RotateRightImm32.apply, Opcode.RotateRightImm64 -> Instruction.RotateRightImm64.apply,
    Opcode.RotateRightImmAlt32 -> Instruction.RotateRightImmAlt32.apply, Opcode.RotateRightImmAlt64 -> Instruction.RotateRightImmAlt64.apply,
    Opcode.StoreIndirectU8 -> Instruction.StoreIndirectU8.apply, Opcode.StoreIndirectU16 -> Instruction.StoreIndirectU16.apply,
    Opcode.StoreIndirectU32 -> Instruction.StoreIndirectU32.apply, Opcode.StoreIndirectU64 -> Instruction.StoreIndirectU64.apply,
    Opcode.LoadIndirectU8 -> Instruction.LoadIndirectU8.apply, Opcode.LoadIndirectI8 -> Instruction.LoadIndirectI8.apply,
    Opcode.LoadIndirectU16 -> Instruction.LoadIndirectU16.apply, Opcode.LoadIndirectI16 -> Instruction.LoadIndirectI16.apply,
    Opcode.LoadIndirectU32 -> Instruction.LoadIndirectU32.apply, Opcode.LoadIndirectI32 -> Instruction.LoadIndirectI32.apply,
    Opcode.LoadIndirectU64 -> Instruction.LoadIndirectU64.apply
  )

  private val regs2OffsetOpcodes: Map[Opcode, (Int, Int, Long) => Instruction] = Map(
    Opcode.BranchEq -> Instruction.BranchEq.apply, Opcode.BranchNotEq -> Instruction.BranchNotEq.apply,
    Opcode.BranchLessUnsigned -> Instruction.BranchLessUnsigned.apply, Opcode.BranchLessSigned -> Instruction.BranchLessSigned.apply,
    Opcode.BranchGreaterOrEqualUnsigned -> Instruction.BranchGreaterOrEqualUnsigned.apply,
    Opcode.BranchGreaterOrEqualSigned -> Instruction.BranchGreaterOrEqualSigned.apply
  )

  private val regImmOffsetOpcodes: Map[Opcode, (Int, Long, Long) => Instruction] = Map(
    Opcode.LoadImmAndJump -> Instruction.LoadImmAndJump.apply,
    Opcode.BranchEqImm -> Instruction.BranchEqImm.apply, Opcode.BranchNotEqImm -> Instruction.BranchNotEqImm.apply,
    Opcode.BranchLessUnsignedImm -> Instruction.BranchLessUnsignedImm.apply, Opcode.BranchLessSignedImm -> Instruction.BranchLessSignedImm.apply,
    Opcode.BranchGreaterOrEqualUnsignedImm -> Instruction.BranchGreaterOrEqualUnsignedImm.apply,
    Opcode.BranchGreaterOrEqualSignedImm -> Instruction.BranchGreaterOrEqualSignedImm.apply,
    Opcode.BranchLessOrEqualSignedImm -> Instruction.BranchLessOrEqualSignedImm.apply,
    Opcode.BranchLessOrEqualUnsignedImm -> Instruction.BranchLessOrEqualUnsignedImm.apply,
    Opcode.BranchGreaterUnsignedImm -> Instruction.BranchGreaterUnsignedImm.apply,
    Opcode.BranchGreaterSignedImm -> Instruction.BranchGreaterSignedImm.apply
  )

  private val storeImmOpcodes: Map[Opcode, (Long, Long) => Instruction] = Map(
    Opcode.StoreImmU8 -> ((a, v) => Instruction.StoreImmU8(a, v.toByte)),
    Opcode.StoreImmU16 -> ((a, v) => Instruction.StoreImmU16(a, v.toShort)),
    Opcode.StoreImmU32 -> ((a, v) => Instruction.StoreImmU32(a, v.toInt)),
    Opcode.StoreImmU64 -> ((a, v) => Instruction.StoreImmU64(a, v))
  )

  private val storeImmIndirectOpcodes: Map[Opcode, (Int, Long, Long) => Instruction] = Map(
    Opcode.StoreImmIndirectU8 -> ((r, o, v) => Instruction.StoreImmIndirectU8(r, o, v.toByte)),
    Opcode.StoreImmIndirectU16 -> ((r, o, v) => Instruction.StoreImmIndirectU16(r, o, v.toShort)),
    Opcode.StoreImmIndirectU32 -> ((r, o, v) => Instruction.StoreImmIndirectU32(r, o, v.toInt)),
    Opcode.StoreImmIndirectU64 -> ((r, o, v) => Instruction.StoreImmIndirectU64(r, o, v))
  )

  private final val KindNone = 0
  private final val KindArgless = 1
  private final val KindRegs2 = 2
  private final val KindRegs3 = 3
  private final val KindRegImm = 4
  private final val KindRegs2Imm = 5
  private final val KindRegs2Offset = 6
  private final val KindRegImmOffset = 7
  private final val KindStoreImm = 8
  private final val KindStoreImmIndirect = 9
  private final val KindEcalli = 10
  private final val KindJump = 11
  private final val KindLoadImm64 = 12
  private final val KindLoadImmAndJumpIndirect = 13

  // 256-entry operand-kind table: opcode byte -> Kind* ordinal.
  private val kindTable: Array[Int] = new Array[Int](256)
  // Parallel 256-entry constructor tables (only the slot matching the kind is used).
  private val arglessTable: Array[Instruction] = new Array[Instruction](256)
  private val regs2Table: Array[(Int, Int) => Instruction] = new Array(256)
  private val regs3Table: Array[(Int, Int, Int) => Instruction] = new Array(256)
  private val regImmTable: Array[(Int, Long) => Instruction] = new Array(256)
  private val regs2ImmTable: Array[(Int, Int, Long) => Instruction] = new Array(256)
  private val regs2OffsetTable: Array[(Int, Int, Long) => Instruction] = new Array(256)
  private val regImmOffsetTable: Array[(Int, Long, Long) => Instruction] = new Array(256)
  private val storeImmTable: Array[(Long, Long) => Instruction] = new Array(256)
  private val storeImmIndirectTable: Array[(Int, Long, Long) => Instruction] = new Array(256)

  // Populate the tables from the SAME Map groupings used by the original chain.
  // Iteration order mirrors the original guard chain's precedence; since each
  // opcode appears in at most one group, order does not affect the result.
  arglessOpcodes.foreach { (op, instr) =>
    kindTable(op.value) = KindArgless; arglessTable(op.value) = instr
  }
  regs2Opcodes.foreach { (op, f) =>
    kindTable(op.value) = KindRegs2; regs2Table(op.value) = f
  }
  regs3Opcodes.foreach { (op, f) =>
    kindTable(op.value) = KindRegs3; regs3Table(op.value) = f
  }
  regImmOpcodes.foreach { (op, f) =>
    kindTable(op.value) = KindRegImm; regImmTable(op.value) = f
  }
  regs2ImmOpcodes.foreach { (op, f) =>
    kindTable(op.value) = KindRegs2Imm; regs2ImmTable(op.value) = f
  }
  regs2OffsetOpcodes.foreach { (op, f) =>
    kindTable(op.value) = KindRegs2Offset; regs2OffsetTable(op.value) = f
  }
  regImmOffsetOpcodes.foreach { (op, f) =>
    kindTable(op.value) = KindRegImmOffset; regImmOffsetTable(op.value) = f
  }
  storeImmOpcodes.foreach { (op, f) =>
    kindTable(op.value) = KindStoreImm; storeImmTable(op.value) = f
  }
  storeImmIndirectOpcodes.foreach { (op, f) =>
    kindTable(op.value) = KindStoreImmIndirect; storeImmIndirectTable(op.value) = f
  }
  // Special-case opcodes (formerly the explicit `case Some(Opcode.X)` arms).
  kindTable(Opcode.Ecalli.value) = KindEcalli
  kindTable(Opcode.Jump.value) = KindJump
  kindTable(Opcode.LoadImm64.value) = KindLoadImm64
  kindTable(Opcode.LoadImmAndJumpIndirect.value) = KindLoadImmAndJumpIndirect

  // ============================================================================
  // Main Decode Function
  // ============================================================================

  /**
   * Decodes the instruction at `offset`, returning it with its distance to the next opcode (1 + skip).
   */
  def decode(code: Array[Byte], bitmask: Array[Byte], offset: Int): (Instruction, Int) =
    if offset >= code.length then return (Instruction.Panic, 1)

    val opcodeByte = code(offset) & 0xFF
    val skip = getSkip(bitmask, code.length, offset)
    val len = skip - 1
    val b1 = byteAt(code, offset + 1)

    val instruction = kindTable(opcodeByte) match
      case KindArgless => arglessTable(opcodeByte)
      case KindRegs2 => regs2Table(opcodeByte)(regLo(b1), regHi(b1))
      case KindRegs3 => regs3Table(opcodeByte)(clampReg(byteAt(code, offset + 2)), regLo(b1), regHi(b1))
      case KindRegImm =>
        regImmTable(opcodeByte)(regLo(b1), readSigned(code, offset + 2, math.min(4, math.max(0, len - 1))))
      case KindRegs2Imm =>
        regs2ImmTable(opcodeByte)(regLo(b1), regHi(b1), readSigned(code, offset + 2, math.min(4, math.max(0, len - 1))))
      case KindRegs2Offset =>
        val target = relative(offset, readSigned(code, offset + 2, math.min(4, math.max(0, len - 1))))
        regs2OffsetTable(opcodeByte)(regLo(b1), regHi(b1), target)
      case KindRegImmOffset =>
        val lX = math.min(4, (b1 >> 4) & 0x7); val lY = math.min(4, math.max(0, len - lX - 1))
        val imm = readSigned(code, offset + 2, lX)
        regImmOffsetTable(opcodeByte)(regLo(b1), imm, relative(offset, readSigned(code, offset + 2 + lX, lY)))
      case KindStoreImm =>
        val lX = math.min(4, b1 & 0x7); val lY = math.min(4, math.max(0, len - lX - 1))
        storeImmTable(opcodeByte)(readSigned(code, offset + 2, lX), readSigned(code, offset + 2 + lX, lY))
      case KindStoreImmIndirect =>
        val lX = math.min(4, (b1 >> 4) & 0x7); val lY = math.min(4, math.max(0, len - lX - 1))
        storeImmIndirectTable(opcodeByte)(
          regLo(b1),
          readSigned(code, offset + 2, lX),
          readSigned(code, offset + 2 + lX, lY)
        )
      case KindEcalli => Instruction.Ecalli(readSigned(code, offset + 1, math.min(4, len)))
      case KindJump => Instruction.Jump(relative(offset, readSigned(code, offset + 1, math.min(4, len))))
      // GP 0.8.0 §A.5: r_A = min(12, ζ_{ı+1} mod 16), ν_X = E⁻¹_8(ζ_{ı+2..+8})
      case KindLoadImm64 => Instruction.LoadImm64(regLo(b1), readUnsigned(code, offset + 2, 8))
      case KindLoadImmAndJumpIndirect =>
        val lX = math.min(4, byteAt(code, offset + 2) & 0x7); val lY = math.min(4, math.max(0, len - lX - 2))
        Instruction.LoadImmAndJumpIndirect(
          regLo(b1),
          regHi(b1),
          readSigned(code, offset + 3, lX),
          readSigned(code, offset + 3 + lX, lY)
        )
      case _ => Instruction.Panic

    (instruction, skip)

  // ============================================================================
  // Helper Functions
  // ============================================================================

  /** ζ_i: the code byte at `i`, or 0 past the end of the code (GP 0.8.0 §A.2 eq:instructions). */
  private inline def byteAt(code: Array[Byte], i: Int): Int = if i < code.length then code(i) & 0xFF else 0

  /** E⁻¹_n(ζ_{start..+n}) for n ≤ 8, little-endian. */
  private def readUnsigned(code: Array[Byte], start: Int, n: Int): Long =
    var result = 0L; var i = 0
    while i < n do { result |= byteAt(code, start + i).toLong << (i * 8); i += 1 }
    result

  /** X_n(E⁻¹_n(ζ_{start..+n})): an n-byte immediate sign-extended to 64 bits. */
  private def readSigned(code: Array[Byte], start: Int, n: Int): Long =
    signExtend(readUnsigned(code, start, n), n * 8)

  /** ı + offset, as the 32-bit target address the jump table and branch checks use. */
  private inline def relative(instrOffset: Int, rel: Long): Long = (instrOffset + rel.toInt).toLong & 0xFFFFFFFFL

  private[program] def getSkip(bitmask: Array[Byte], codeLength: Int, offset: Int): Int =
    var cur = offset + 1; var skip = 1
    while cur < codeLength && skip <= MaxInstructionSize do
      val byteIdx = cur >> 3; val bitIdx = cur & 7
      if byteIdx < bitmask.length && ((bitmask(byteIdx) >> bitIdx) & 1) == 1 then return skip
      cur += 1; skip += 1
    skip

  private def signExtend(value: Long, bits: Int): Long =
    if bits <= 0 then 0L else if bits >= 64 then value
    else { val s = 1L << (bits - 1); if (value & s) != 0 then value | (-1L << bits) else value & ((1L << bits) - 1) }

  /** Decode a register index: clamp to the 13 registers */
  private inline def clampReg(raw: Int): Int = math.min(12, raw)

  /** min(12, b mod 16) */
  private inline def regLo(b: Int): Int = clampReg(b & 0xF)

  /** min(12, ⌊b / 16⌋) */
  private inline def regHi(b: Int): Int = clampReg((b >> 4) & 0xF)
