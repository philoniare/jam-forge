package io.forge.jam.pvm.recompiler

import io.forge.jam.pvm.Instruction
import io.forge.jam.pvm.program.{InstructionDecoder, ProgramBlob}

/**
 * The generic FFM instruction ABI: maps every production `Instruction` to a
 * `{opcode, a, b, c, imm, imm2}` tuple keyed by the REAL PVM opcode value
 * (`instr.opcode.value` — never a private renumbering), plus the program
 * preparation step (byte-offset decode -> instruction-index target
 * resolution) the recompiler needs before it can consume a blob.
 */
object RecompilerAbi:

  /** One generic FFI instruction tuple (mirrors Rust's `RawInstr`/Java's
    * `RAW_INSTR` layout: opcode, a, b, c, imm, imm2). */
  final case class RawInstr(opcode: Int, a: Int, b: Int, c: Int, imm: Long, imm2: Long)

  /** Sentinel `targetIndex` result for a byte offset that isn't a decoded
    * instruction's leader (invalid static control-flow target). */
  private val InvalidTarget: Long = -1L

  private enum StaticTargetField:
    case TargetInImm, TargetInImm2, NoStaticTarget

  private def staticTargetField(instr: Instruction): StaticTargetField = instr match
    case _: Instruction.LoadImmAndJump |
         _: Instruction.BranchEqImm | _: Instruction.BranchNotEqImm |
         _: Instruction.BranchLessUnsignedImm | _: Instruction.BranchLessSignedImm |
         _: Instruction.BranchGreaterOrEqualUnsignedImm | _: Instruction.BranchGreaterOrEqualSignedImm |
         _: Instruction.BranchLessOrEqualSignedImm | _: Instruction.BranchLessOrEqualUnsignedImm |
         _: Instruction.BranchGreaterSignedImm | _: Instruction.BranchGreaterUnsignedImm =>
      StaticTargetField.TargetInImm2
    case _: Instruction.Jump |
         _: Instruction.BranchEq | _: Instruction.BranchNotEq |
         _: Instruction.BranchLessUnsigned | _: Instruction.BranchLessSigned |
         _: Instruction.BranchGreaterOrEqualUnsigned | _: Instruction.BranchGreaterOrEqualSigned =>
      StaticTargetField.TargetInImm
    case _ => StaticTargetField.NoStaticTarget

  /** Map one decoded `Instruction` to its generic tuple. `targetIndex` resolves
    * a byte-offset control-flow target to an instruction index (already
    * computed by `prepareProgram`); instructions with no control-flow target
    * ignore it. Public: also the entry point for mapping a single instruction
    * whose control-flow target (if any) is ALREADY an instruction index
    * (pass `identity`), e.g. in tests. */
  def mapInstruction(instr: Instruction, targetIndex: Long => Long = identity): RawInstr =
    val op = instr.opcode.value
    instr match

      // ==========================================================================
      // Argless
      // ==========================================================================
      case Instruction.Panic | Instruction.Fallthrough | Instruction.Invalid =>
        RawInstr(op, 0, 0, 0, 0L, 0L)

      // ==========================================================================
      // Single Immediate
      // ==========================================================================
      case Instruction.Jump(target) =>
        RawInstr(op, 0, 0, 0, targetIndex(target), 0L)
      case Instruction.Ecalli(hostId) =>
        RawInstr(op, 0, 0, 0, hostId, 0L)

      // ==========================================================================
      // Two Immediate
      // ==========================================================================
      case Instruction.StoreImmU8(address, value) => RawInstr(op, 0, 0, 0, address, value)
      case Instruction.StoreImmU16(address, value) => RawInstr(op, 0, 0, 0, address, value)
      case Instruction.StoreImmU32(address, value) => RawInstr(op, 0, 0, 0, address, value)
      case Instruction.StoreImmU64(address, value) => RawInstr(op, 0, 0, 0, address, value)

      // ==========================================================================
      // Register + Immediate
      // ==========================================================================
      case Instruction.JumpIndirect(reg, offset) => RawInstr(op, reg, 0, 0, offset, 0L)
      case Instruction.LoadImm(reg, imm) => RawInstr(op, reg, 0, 0, imm, 0L)
      case Instruction.LoadImm64(reg, imm) => RawInstr(op, reg, 0, 0, imm, 0L)
      case Instruction.LoadU8(reg, address) => RawInstr(op, reg, 0, 0, address, 0L)
      case Instruction.LoadI8(reg, address) => RawInstr(op, reg, 0, 0, address, 0L)
      case Instruction.LoadU16(reg, address) => RawInstr(op, reg, 0, 0, address, 0L)
      case Instruction.LoadI16(reg, address) => RawInstr(op, reg, 0, 0, address, 0L)
      case Instruction.LoadU32(reg, address) => RawInstr(op, reg, 0, 0, address, 0L)
      case Instruction.LoadI32(reg, address) => RawInstr(op, reg, 0, 0, address, 0L)
      case Instruction.LoadU64(reg, address) => RawInstr(op, reg, 0, 0, address, 0L)
      case Instruction.StoreU8(reg, address) => RawInstr(op, reg, 0, 0, address, 0L)
      case Instruction.StoreU16(reg, address) => RawInstr(op, reg, 0, 0, address, 0L)
      case Instruction.StoreU32(reg, address) => RawInstr(op, reg, 0, 0, address, 0L)
      case Instruction.StoreU64(reg, address) => RawInstr(op, reg, 0, 0, address, 0L)

      // ==========================================================================
      // Register + Immediate + Offset (Branches with Immediate)
      // ==========================================================================
      case Instruction.LoadImmAndJump(reg, imm, target) =>
        RawInstr(op, reg, 0, 0, imm, targetIndex(target))
      case Instruction.BranchEqImm(reg, imm, offset) =>
        RawInstr(op, reg, 0, 0, imm, targetIndex(offset))
      case Instruction.BranchNotEqImm(reg, imm, offset) =>
        RawInstr(op, reg, 0, 0, imm, targetIndex(offset))
      case Instruction.BranchLessUnsignedImm(reg, imm, offset) =>
        RawInstr(op, reg, 0, 0, imm, targetIndex(offset))
      case Instruction.BranchLessSignedImm(reg, imm, offset) =>
        RawInstr(op, reg, 0, 0, imm, targetIndex(offset))
      case Instruction.BranchGreaterOrEqualUnsignedImm(reg, imm, offset) =>
        RawInstr(op, reg, 0, 0, imm, targetIndex(offset))
      case Instruction.BranchGreaterOrEqualSignedImm(reg, imm, offset) =>
        RawInstr(op, reg, 0, 0, imm, targetIndex(offset))
      case Instruction.BranchLessOrEqualSignedImm(reg, imm, offset) =>
        RawInstr(op, reg, 0, 0, imm, targetIndex(offset))
      case Instruction.BranchLessOrEqualUnsignedImm(reg, imm, offset) =>
        RawInstr(op, reg, 0, 0, imm, targetIndex(offset))
      case Instruction.BranchGreaterSignedImm(reg, imm, offset) =>
        RawInstr(op, reg, 0, 0, imm, targetIndex(offset))
      case Instruction.BranchGreaterUnsignedImm(reg, imm, offset) =>
        RawInstr(op, reg, 0, 0, imm, targetIndex(offset))

      // ==========================================================================
      // Store Immediate Indirect (reg + offset -> value)
      // ==========================================================================
      case Instruction.StoreImmIndirectU8(reg, offset, value) => RawInstr(op, reg, 0, 0, offset, value)
      case Instruction.StoreImmIndirectU16(reg, offset, value) => RawInstr(op, reg, 0, 0, offset, value)
      case Instruction.StoreImmIndirectU32(reg, offset, value) => RawInstr(op, reg, 0, 0, offset, value)
      case Instruction.StoreImmIndirectU64(reg, offset, value) => RawInstr(op, reg, 0, 0, offset, value)

      // ==========================================================================
      // Two Register
      // ==========================================================================
      case Instruction.MoveReg(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)
      case Instruction.Sbrk(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)
      case Instruction.CountLeadingZeroBits32(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)
      case Instruction.CountLeadingZeroBits64(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)
      case Instruction.CountTrailingZeroBits32(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)
      case Instruction.CountTrailingZeroBits64(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)
      case Instruction.CountSetBits32(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)
      case Instruction.CountSetBits64(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)
      case Instruction.SignExtend8(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)
      case Instruction.SignExtend16(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)
      case Instruction.ZeroExtend16(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)
      case Instruction.ReverseByte(dst, src) => RawInstr(op, dst, src, 0, 0L, 0L)

      // ==========================================================================
      // Two Register + Immediate (Arithmetic/Logic with Immediate)
      // ==========================================================================
      case Instruction.AddImm32(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.AddImm64(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.AndImm(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.XorImm(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.OrImm(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.MulImm32(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.MulImm64(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.SetLessThanUnsignedImm(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.SetLessThanSignedImm(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.SetGreaterThanUnsignedImm(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.SetGreaterThanSignedImm(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftLogicalLeftImm32(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftLogicalLeftImm64(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftLogicalRightImm32(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftLogicalRightImm64(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftArithmeticRightImm32(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftArithmeticRightImm64(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.NegateAndAddImm32(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.NegateAndAddImm64(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftLogicalLeftImmAlt32(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftLogicalLeftImmAlt64(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftLogicalRightImmAlt32(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftLogicalRightImmAlt64(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftArithmeticRightImmAlt32(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.ShiftArithmeticRightImmAlt64(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.CmovIfZeroImm(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.CmovIfNotZeroImm(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.RotateRightImm32(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.RotateRightImm64(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.RotateRightImmAlt32(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)
      case Instruction.RotateRightImmAlt64(dst, src, imm) => RawInstr(op, dst, src, 0, imm, 0L)

      // ==========================================================================
      // Two Register + Offset (Indirect Memory)
      // ==========================================================================
      case Instruction.StoreIndirectU8(src, base, offset) => RawInstr(op, src, base, 0, offset, 0L)
      case Instruction.StoreIndirectU16(src, base, offset) => RawInstr(op, src, base, 0, offset, 0L)
      case Instruction.StoreIndirectU32(src, base, offset) => RawInstr(op, src, base, 0, offset, 0L)
      case Instruction.StoreIndirectU64(src, base, offset) => RawInstr(op, src, base, 0, offset, 0L)
      case Instruction.LoadIndirectU8(dst, base, offset) => RawInstr(op, dst, base, 0, offset, 0L)
      case Instruction.LoadIndirectI8(dst, base, offset) => RawInstr(op, dst, base, 0, offset, 0L)
      case Instruction.LoadIndirectU16(dst, base, offset) => RawInstr(op, dst, base, 0, offset, 0L)
      case Instruction.LoadIndirectI16(dst, base, offset) => RawInstr(op, dst, base, 0, offset, 0L)
      case Instruction.LoadIndirectU32(dst, base, offset) => RawInstr(op, dst, base, 0, offset, 0L)
      case Instruction.LoadIndirectI32(dst, base, offset) => RawInstr(op, dst, base, 0, offset, 0L)
      case Instruction.LoadIndirectU64(dst, base, offset) => RawInstr(op, dst, base, 0, offset, 0L)

      // ==========================================================================
      // Two Register + Offset (Branches)
      // ==========================================================================
      case Instruction.BranchEq(r1, r2, offset) => RawInstr(op, r1, r2, 0, targetIndex(offset), 0L)
      case Instruction.BranchNotEq(r1, r2, offset) => RawInstr(op, r1, r2, 0, targetIndex(offset), 0L)
      case Instruction.BranchLessUnsigned(r1, r2, offset) => RawInstr(op, r1, r2, 0, targetIndex(offset), 0L)
      case Instruction.BranchLessSigned(r1, r2, offset) => RawInstr(op, r1, r2, 0, targetIndex(offset), 0L)
      case Instruction.BranchGreaterOrEqualUnsigned(r1, r2, offset) => RawInstr(op, r1, r2, 0, targetIndex(offset), 0L)
      case Instruction.BranchGreaterOrEqualSigned(r1, r2, offset) => RawInstr(op, r1, r2, 0, targetIndex(offset), 0L)

      // ==========================================================================
      // Three Register (Arithmetic/Logic)
      // ==========================================================================
      case Instruction.Add32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.Add64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.Sub32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.Sub64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.Mul32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.Mul64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.DivUnsigned32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.DivUnsigned64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.DivSigned32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.DivSigned64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.RemUnsigned32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.RemUnsigned64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.RemSigned32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.RemSigned64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.And(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.Xor(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.Or(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.AndInverted(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.OrInverted(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.Xnor(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.ShiftLogicalLeft32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.ShiftLogicalLeft64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.ShiftLogicalRight32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.ShiftLogicalRight64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.ShiftArithmeticRight32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.ShiftArithmeticRight64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.RotateLeft32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.RotateLeft64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.RotateRight32(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.RotateRight64(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.SetLessThanUnsigned(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.SetLessThanSigned(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.CmovIfZero(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.CmovIfNotZero(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.Maximum(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.MaximumUnsigned(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.Minimum(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.MinimumUnsigned(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.MulUpperSignedSigned(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.MulUpperUnsignedUnsigned(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)
      case Instruction.MulUpperSignedUnsigned(d, s1, s2) => RawInstr(op, d, s1, s2, 0L, 0L)

      // ==========================================================================
      // Four Operand
      // ==========================================================================
      case Instruction.LoadImmAndJumpIndirect(dst, base, imm, offset) =>
        RawInstr(op, dst, base, 0, imm, offset)

  /** A fully-prepared program ready for `PvmRecompiler.compile`: parallel
    * column arrays plus the (already index-translated) jump table. */
  final case class PreparedProgram(
    opcodes: Array[Int],
    a: Array[Int],
    b: Array[Int],
    c: Array[Int],
    imm: Array[Long],
    imm2: Array[Long],
    jumpTable: Array[Int]
  )

  /**
   * Decode a whole code blob and translate it into the recompiler's generic
   * column form.
   */
  def prepareProgram(code: Array[Byte], bitmask: Array[Byte], jumpTable: io.forge.jam.pvm.program.JumpTable): PreparedProgram =
    // Pass 1: decode the whole blob, recording each instruction's byte offset.
    val instrs = scala.collection.mutable.ArrayBuffer.empty[Instruction]
    val byteOffsets = scala.collection.mutable.ArrayBuffer.empty[Int]
    var off = 0
    while off < code.length do
      val (instr, skip) = InstructionDecoder.decode(code, bitmask, off)
      instrs += instr
      byteOffsets += off
      off += math.max(1, skip)
    val offsetToIndex: Map[Int, Int] = byteOffsets.zipWithIndex.toMap

    val n = instrs.length
    val opcodes = new Array[Int](n)
    val aArr = new Array[Int](n)
    val bArr = new Array[Int](n)
    val cArr = new Array[Int](n)
    val immArr = new Array[Long](n)
    val imm2Arr = new Array[Long](n)

    // Byte offset -> instruction index, or InvalidTarget (sentinel: caller
    // replaces the whole instruction with Panic when a target doesn't land
    // on a decoded instruction's leader byte).
    def targetIndex(byteOffset: Long): Long =
      offsetToIndex.get((byteOffset & 0xFFFFFFFFL).toInt) match
        case Some(idx) => idx.toLong
        case None => InvalidTarget

    for i <- 0 until n do
      val mapped = mapInstruction(instrs(i), targetIndex)
      // A static control-flow target that resolved to InvalidTarget replaces
      // the whole instruction with Panic ("static jump/branch targets valid
      // only at block leaders / jump-table entries; otherwise panic").
      val invalidTarget = staticTargetField(instrs(i)) match
        case StaticTargetField.TargetInImm => mapped.imm == InvalidTarget
        case StaticTargetField.TargetInImm2 => mapped.imm2 == InvalidTarget
        case StaticTargetField.NoStaticTarget => false
      val raw =
        if invalidTarget then mapInstruction(Instruction.Panic, targetIndex)
        else mapped
      opcodes(i) = raw.opcode; aArr(i) = raw.a; bArr(i) = raw.b; cArr(i) = raw.c
      immArr(i) = raw.imm; imm2Arr(i) = raw.imm2

    // Jump table: byte-offset entries -> instruction indices; invalid (not a
    // decoded instruction's byte offset) entries are dropped, matching the
    // current Rust `pvm_compile` in-range filtering.
    val jt = jumpTable.iterator.flatMap(entry => offsetToIndex.get(entry)).map(_.toInt).toArray

    PreparedProgram(opcodes, aArr, bArr, cArr, immArr, imm2Arr, jt)

  /** Convenience overload taking a `ProgramBlob` directly. */
  def prepareProgram(blob: ProgramBlob): PreparedProgram =
    prepareProgram(blob.code, blob.bitmask, blob.jumpTable)
