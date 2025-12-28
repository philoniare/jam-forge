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
    Opcode.Memset -> Instruction.Memset
  )

  private val regs2Opcodes: Map[Opcode, (Int, Int) => Instruction] = Map(
    Opcode.MoveReg -> Instruction.MoveReg.apply,
    Opcode.Sbrk -> Instruction.Sbrk.apply,
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

  // ============================================================================
  // Main Decode Function
  // ============================================================================

  def decode(code: Array[Byte], bitmask: Array[Byte], offset: Int): (Instruction, Int) =
    if offset >= code.length then return (Instruction.Panic, 1)

    val opcodeByte = code(offset) & 0xFF
    val skip = getSkip(bitmask, code.length, offset)
    val chunk = readChunk(code, offset + 1, skip - 1)
    val len = skip - 1

    val instruction = Opcode.fromByte(opcodeByte) match
      case Some(op) if arglessOpcodes.contains(op) => arglessOpcodes(op)
      case Some(op) if regs2Opcodes.contains(op) => val (a, b) = readRegs2(chunk); regs2Opcodes(op)(a, b)
      case Some(op) if regs3Opcodes.contains(op) => val (a, b, c) = readRegs3(chunk); regs3Opcodes(op)(a, b, c)
      case Some(op) if regImmOpcodes.contains(op) => val (r, i) = readRegImm(chunk, len); regImmOpcodes(op)(r, i)
      case Some(op) if regs2ImmOpcodes.contains(op) => val (a, b, i) = readRegs2Imm(chunk, len); regs2ImmOpcodes(op)(a, b, i)
      case Some(op) if regs2OffsetOpcodes.contains(op) => val (a, b, o) = readRegs2Offset(chunk, offset, len); regs2OffsetOpcodes(op)(a, b, o)
      case Some(op) if regImmOffsetOpcodes.contains(op) => val (r, i, o) = readRegImmOffset(chunk, offset, len); regImmOffsetOpcodes(op)(r, i, o)
      case Some(op) if storeImmOpcodes.contains(op) => val (a, v) = readImmImm(chunk, len); storeImmOpcodes(op)(a, v)
      case Some(op) if storeImmIndirectOpcodes.contains(op) => val (r, o, v) = readRegImmImm(chunk, len); storeImmIndirectOpcodes(op)(r, o, v)
      case Some(Opcode.Ecalli) => Instruction.Ecalli(signExtend(chunk, len * 8))
      case Some(Opcode.Jump) => Instruction.Jump((offset + signExtend(chunk, len * 8).toInt).toLong & 0xFFFFFFFFL)
      case Some(Opcode.LoadImm64) => Instruction.LoadImm64((code(offset + 1) & 0xFF) % NumGeneralRegisters, readLong(code, offset + 2))
      case Some(Opcode.LoadImmAndJumpIndirect) => val (a, b, i1, i2) = readRegs2Imm2(chunk, len); Instruction.LoadImmAndJumpIndirect(a, b, i1, i2)
      case _ => Instruction.Panic

    (instruction, skip)

  // ============================================================================
  // Helper Functions
  // ============================================================================

  private def readChunk(code: Array[Byte], start: Int, length: Int): Long =
    var result = 0L; var i = 0
    while i < length && start + i < code.length do { result |= (code(start + i).toLong & 0xFF) << (i * 8); i += 1 }
    result

  private def readLong(code: Array[Byte], start: Int): Long =
    var result = 0L; var i = 0
    while i < 8 && start + i < code.length do { result |= (code(start + i).toLong & 0xFF) << (i * 8); i += 1 }
    result

  private def getSkip(bitmask: Array[Byte], codeLength: Int, offset: Int): Int =
    var cur = offset + 1; var skip = 1
    while cur < codeLength && skip < MaxInstructionSize do
      val byteIdx = cur >> 3; val bitIdx = cur & 7
      if byteIdx < bitmask.length && ((bitmask(byteIdx) >> bitIdx) & 1) == 1 then return skip
      cur += 1; skip += 1
    skip

  private def signExtend(value: Long, bits: Int): Long =
    if bits <= 0 then 0L else if bits >= 64 then value
    else { val s = 1L << (bits - 1); if (value & s) != 0 then value | (-1L << bits) else value & ((1L << bits) - 1) }

  private def readRegImm(chunk: Long, len: Int): (Int, Long) =
    ((chunk & 0xF).toInt % NumGeneralRegisters, signExtend(chunk >>> 8, len * 8 - 8))

  private def readRegs2(chunk: Long): (Int, Int) =
    ((chunk & 0xF).toInt % NumGeneralRegisters, ((chunk >> 4) & 0xF).toInt % NumGeneralRegisters)

  private def readRegs2Imm(chunk: Long, len: Int): (Int, Int, Long) =
    ((chunk & 0xF).toInt % NumGeneralRegisters, ((chunk >> 4) & 0xF).toInt % NumGeneralRegisters, signExtend(chunk >>> 8, len * 8 - 8))

  private def readRegs2Offset(chunk: Long, instrOffset: Int, len: Int): (Int, Int, Long) =
    val (r1, r2, imm) = readRegs2Imm(chunk, len); (r1, r2, (instrOffset + imm.toInt).toLong & 0xFFFFFFFFL)

  private def readRegs3(chunk: Long): (Int, Int, Int) =
    (((chunk >> 8) & 0xF).toInt % NumGeneralRegisters, (chunk & 0xF).toInt % NumGeneralRegisters, ((chunk >> 4) & 0xF).toInt % NumGeneralRegisters)

  private def readImmImm(chunk: Long, len: Int): (Long, Long) =
    val aux = (chunk & 0x7).toInt; val b1 = math.min(4, aux); val b2 = math.max(0, math.min(4, len - b1 - 1))
    val sc = chunk >>> 8; (signExtend(sc, b1 * 8), signExtend(sc >>> (b1 * 8), b2 * 8))

  private def readRegImmOffset(chunk: Long, instrOffset: Int, len: Int): (Int, Long, Long) =
    val reg = (chunk & 0xF).toInt % NumGeneralRegisters; val aux = ((chunk >> 4) & 0x7).toInt
    val b1 = math.min(4, aux); val b2 = math.max(0, math.min(4, len - b1 - 1))
    val sc = chunk >>> 8; (reg, signExtend(sc, b1 * 8), (instrOffset + signExtend(sc >>> (b1 * 8), b2 * 8).toInt).toLong & 0xFFFFFFFFL)

  private def readRegImmImm(chunk: Long, len: Int): (Int, Long, Long) =
    val reg = (chunk & 0xF).toInt % NumGeneralRegisters; val aux = ((chunk >> 4) & 0x7).toInt
    val b1 = math.min(4, aux); val b2 = math.max(0, math.min(4, len - b1 - 1))
    val sc = chunk >>> 8; (reg, signExtend(sc, b1 * 8), signExtend(sc >>> (b1 * 8), b2 * 8))

  private def readRegs2Imm2(chunk: Long, len: Int): (Int, Int, Long, Long) =
    val r1 = (chunk & 0xF).toInt % NumGeneralRegisters; val r2 = ((chunk >> 4) & 0xF).toInt % NumGeneralRegisters
    val aux = ((chunk >> 8) & 0x7).toInt; val b1 = math.min(4, aux); val b2 = math.max(0, math.min(4, len - b1 - 2))
    val sc = chunk >>> 16; (r1, r2, signExtend(sc, b1 * 8), signExtend(sc >>> (b1 * 8), b2 * 8))
