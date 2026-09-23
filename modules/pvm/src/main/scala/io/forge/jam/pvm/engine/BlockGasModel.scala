package io.forge.jam.pvm.engine

import scala.collection.mutable.ArrayBuffer
import io.forge.jam.pvm.{Instruction, Opcode}
import io.forge.jam.pvm.program.InstructionDecoder

object BlockGasModel:

  private inline val MaxDecodePerCycle = 4
  private inline val MaxStartPerCycle = 5
  private inline val MaxLiveRobEntries = 32
  private inline val PipelineLatency = 3

  private inline val StateDecoding = 0 // DEC
  private inline val StateWaiting = 1 // WAIT
  private inline val StateExecuting = 2 // EXE
  private inline val StateExecuted = 3 // FIN
  private inline val StateRetired = 4 // none

  private val TrapOpcodeByte: Int = Opcode.Panic.value // 0
  private val UnlikelyOpcodeByte: Int = Opcode.Unlikely.value // 2

  private inline val MemCost = 25

  private final case class Units(a: Int, l: Int, s: Int, m: Int, d: Int):
    def +(o: Units): Units = Units(a + o.a, l + o.l, s + o.s, m + o.m, d + o.d)
    def -(o: Units): Units = Units(a - o.a, l - o.l, s - o.s, m - o.m, d - o.d)
    def <=(o: Units): Boolean = a <= o.a && l <= o.l && s <= o.s && m <= o.m && d <= o.d

  private val NoUnits = Units(0, 0, 0, 0, 0)
  private val AluUnit = Units(1, 0, 0, 0, 0)
  private val AluLoadUnits = Units(1, 1, 0, 0, 0)
  private val AluStoreUnits = Units(1, 0, 1, 0, 0)
  private val AluMulUnits = Units(1, 0, 0, 1, 0)
  private val AluDivUnits = Units(1, 0, 0, 0, 1)
  private val TwoAluUnits = Units(2, 0, 0, 0, 0) // trailing_zero_bits_* row

  /** Initial capacities t^init = (4, 4, 4, 1, 1) (eq:gascostforblock ~1004). */
  private val InitialUnits = Units(4, 4, 4, 1, 1)

  private final class RobEntry(
      var state: Int,
      var cyclesLeft: Int,
      val deps: Array[Int],
      var regsMask: Int,
      val units: Units
  )

  private final case class InstrAttrs(
      cycles: Int,
      decodeSlots: Int,
      units: Units,
      srcMask: Int,
      dstMask: Int,
      isTerminator: Boolean,
      isMoveReg: Boolean,
      fullSkip: Int
  )

  private inline def bit(r: Int): Int = 1 << r

  private inline def instrByte(code: Array[Byte], index: Long): Int =
    if index >= 0 && index < code.length then code(index.toInt) & 0xff else 0

  private def branchCost(code: Array[Byte], pc: Int, fullSkip: Int, target: Long): Int =
    val fallthroughByte = instrByte(code, pc.toLong + fullSkip.toLong)
    val targetByte = instrByte(code, target)
    if fallthroughByte == TrapOpcodeByte || fallthroughByte == UnlikelyOpcodeByte ||
      targetByte == TrapOpcodeByte || targetByte == UnlikelyOpcodeByte
    then 1
    else 20

  private inline def p(a: Int, b: Int, srcMask: Int, dstMask: Int): Int =
    if (srcMask & dstMask) != 0 then a else b

  private inline def pShift(a: Int, b: Int, s1: Int, d: Int): Int =
    if s1 == d then a else b

  private def attrsAt(code: Array[Byte], bitmask: Array[Byte], iota: Int): InstrAttrs =
    val (instr, skip) = InstructionDecoder.decode(code, bitmask, iota)
    import Instruction as I

    inline def mk(
        cycles: Int,
        slots: Int,
        units: Units,
        src: Int = 0,
        dst: Int = 0
    ): InstrAttrs =
      InstrAttrs(cycles, slots, units, src, dst, instr.opcode.startsNewBasicBlock, false, skip)

    instr match
      case I.MoveReg(dst, src) =>
        InstrAttrs(0, 1, NoUnits, bit(src), bit(dst), false, true, skip)

      case I.Panic | I.Fallthrough => mk(2, 1, NoUnits)
      case I.Unlikely => mk(40, 1, NoUnits)
      case I.Jump(_) => mk(15, 1, NoUnits)
      case I.LoadImmAndJump(reg, _, _) => mk(15, 1, NoUnits, dst = bit(reg))
      case I.JumpIndirect(reg, _) => mk(22, 1, NoUnits, src = bit(reg))
      case I.LoadImmAndJumpIndirect(dst, base, _, _) => mk(22, 1, NoUnits, src = bit(base), dst = bit(dst))
      case I.Ecalli(_) => mk(100, 4, AluUnit)

      case I.And(d, s1, s2) => aluThreeOp(1, 1, 2, d, s1, s2, skip)
      case I.Xor(d, s1, s2) => aluThreeOp(1, 1, 2, d, s1, s2, skip)
      case I.Or(d, s1, s2) => aluThreeOp(1, 1, 2, d, s1, s2, skip)
      case I.Add64(d, s1, s2) => aluThreeOp(1, 1, 2, d, s1, s2, skip)
      case I.Sub64(d, s1, s2) => aluThreeOp(1, 1, 2, d, s1, s2, skip)
      case I.Add32(d, s1, s2) => aluThreeOp(2, 2, 3, d, s1, s2, skip)
      case I.Sub32(d, s1, s2) => aluThreeOp(2, 2, 3, d, s1, s2, skip)

      case I.AndImm(d, s, _) => aluTwoOp(1, 1, 2, d, s, skip)
      case I.XorImm(d, s, _) => aluTwoOp(1, 1, 2, d, s, skip)
      case I.OrImm(d, s, _) => aluTwoOp(1, 1, 2, d, s, skip)
      case I.AddImm64(d, s, _) => aluTwoOp(1, 1, 2, d, s, skip)
      case I.ShiftLogicalRightImm64(d, s, _) => aluTwoOp(1, 1, 2, d, s, skip)
      case I.ShiftArithmeticRightImm64(d, s, _) => aluTwoOp(1, 1, 2, d, s, skip)
      case I.ShiftLogicalLeftImm64(d, s, _) => aluTwoOp(1, 1, 2, d, s, skip)
      case I.RotateRightImm64(d, s, _) => aluTwoOp(1, 1, 2, d, s, skip)
      case I.ReverseByte(d, s) => aluTwoOp(1, 1, 2, d, s, skip)
      case I.AddImm32(d, s, _) => aluTwoOp(2, 2, 3, d, s, skip)
      case I.ShiftLogicalRightImm32(d, s, _) => aluTwoOp(2, 2, 3, d, s, skip)
      case I.ShiftArithmeticRightImm32(d, s, _) => aluTwoOp(2, 2, 3, d, s, skip)
      case I.ShiftLogicalLeftImm32(d, s, _) => aluTwoOp(2, 2, 3, d, s, skip)
      case I.RotateRightImm32(d, s, _) => aluTwoOp(2, 2, 3, d, s, skip)

      case I.CountSetBits64(d, s) => mk(1, 1, AluUnit, src = bit(s), dst = bit(d))
      case I.CountSetBits32(d, s) => mk(1, 1, AluUnit, src = bit(s), dst = bit(d))
      case I.CountLeadingZeroBits64(d, s) => mk(1, 1, AluUnit, src = bit(s), dst = bit(d))
      case I.CountLeadingZeroBits32(d, s) => mk(1, 1, AluUnit, src = bit(s), dst = bit(d))
      case I.SignExtend8(d, s) => mk(1, 1, AluUnit, src = bit(s), dst = bit(d))
      case I.SignExtend16(d, s) => mk(1, 1, AluUnit, src = bit(s), dst = bit(d))
      case I.ZeroExtend16(d, s) => mk(1, 1, AluUnit, src = bit(s), dst = bit(d))
      case I.CountTrailingZeroBits64(d, s) => mk(2, 1, TwoAluUnits, src = bit(s), dst = bit(d))
      case I.CountTrailingZeroBits32(d, s) => mk(2, 1, TwoAluUnits, src = bit(s), dst = bit(d))

      case I.ShiftLogicalLeft64(d, s1, s2) => shiftOp(1, 2, 3, d, s1, s2, skip)
      case I.ShiftLogicalRight64(d, s1, s2) => shiftOp(1, 2, 3, d, s1, s2, skip)
      case I.ShiftArithmeticRight64(d, s1, s2) => shiftOp(1, 2, 3, d, s1, s2, skip)
      case I.RotateLeft64(d, s1, s2) => shiftOp(1, 2, 3, d, s1, s2, skip)
      case I.RotateRight64(d, s1, s2) => shiftOp(1, 2, 3, d, s1, s2, skip)
      case I.ShiftLogicalLeft32(d, s1, s2) => shiftOp(2, 3, 4, d, s1, s2, skip)
      case I.ShiftLogicalRight32(d, s1, s2) => shiftOp(2, 3, 4, d, s1, s2, skip)
      case I.ShiftArithmeticRight32(d, s1, s2) => shiftOp(2, 3, 4, d, s1, s2, skip)
      case I.RotateLeft32(d, s1, s2) => shiftOp(2, 3, 4, d, s1, s2, skip)
      case I.RotateRight32(d, s1, s2) => shiftOp(2, 3, 4, d, s1, s2, skip)

      case I.ShiftLogicalLeftImmAlt64(d, s, _) => mk(1, 3, AluUnit, src = bit(s), dst = bit(d))
      case I.ShiftLogicalRightImmAlt64(d, s, _) => mk(1, 3, AluUnit, src = bit(s), dst = bit(d))
      case I.ShiftArithmeticRightImmAlt64(d, s, _) => mk(1, 3, AluUnit, src = bit(s), dst = bit(d))
      case I.RotateRightImmAlt64(d, s, _) => mk(1, 3, AluUnit, src = bit(s), dst = bit(d))
      case I.ShiftLogicalLeftImmAlt32(d, s, _) => mk(2, 4, AluUnit, src = bit(s), dst = bit(d))
      case I.ShiftLogicalRightImmAlt32(d, s, _) => mk(2, 4, AluUnit, src = bit(s), dst = bit(d))
      case I.ShiftArithmeticRightImmAlt32(d, s, _) => mk(2, 4, AluUnit, src = bit(s), dst = bit(d))
      case I.RotateRightImmAlt32(d, s, _) => mk(2, 4, AluUnit, src = bit(s), dst = bit(d))

      case I.SetLessThanUnsigned(d, s1, s2) => mk(3, 3, AluUnit, src = bit(s1) | bit(s2), dst = bit(d))
      case I.SetLessThanSigned(d, s1, s2) => mk(3, 3, AluUnit, src = bit(s1) | bit(s2), dst = bit(d))
      case I.SetLessThanUnsignedImm(d, s, _) => mk(3, 3, AluUnit, src = bit(s), dst = bit(d))
      case I.SetLessThanSignedImm(d, s, _) => mk(3, 3, AluUnit, src = bit(s), dst = bit(d))
      case I.SetGreaterThanUnsignedImm(d, s, _) => mk(3, 3, AluUnit, src = bit(s), dst = bit(d))
      case I.SetGreaterThanSignedImm(d, s, _) => mk(3, 3, AluUnit, src = bit(s), dst = bit(d))

      case I.CmovIfZero(d, s1, s2) => mk(2, 2, AluUnit, src = bit(s1) | bit(s2), dst = bit(d))
      case I.CmovIfNotZero(d, s1, s2) => mk(2, 2, AluUnit, src = bit(s1) | bit(s2), dst = bit(d))
      case I.CmovIfZeroImm(d, s, _) => mk(2, 3, AluUnit, src = bit(s), dst = bit(d))
      case I.CmovIfNotZeroImm(d, s, _) => mk(2, 3, AluUnit, src = bit(s), dst = bit(d))

      case I.Maximum(d, s1, s2) => aluThreeOp(3, 2, 3, d, s1, s2, skip)
      case I.MaximumUnsigned(d, s1, s2) => aluThreeOp(3, 2, 3, d, s1, s2, skip)
      case I.Minimum(d, s1, s2) => aluThreeOp(3, 2, 3, d, s1, s2, skip)
      case I.MinimumUnsigned(d, s1, s2) => aluThreeOp(3, 2, 3, d, s1, s2, skip)

      case I.LoadIndirectU8(d, b, _) => mk(MemCost, 1, AluLoadUnits, src = bit(b), dst = bit(d))
      case I.LoadIndirectI8(d, b, _) => mk(MemCost, 1, AluLoadUnits, src = bit(b), dst = bit(d))
      case I.LoadIndirectU16(d, b, _) => mk(MemCost, 1, AluLoadUnits, src = bit(b), dst = bit(d))
      case I.LoadIndirectI16(d, b, _) => mk(MemCost, 1, AluLoadUnits, src = bit(b), dst = bit(d))
      case I.LoadIndirectU32(d, b, _) => mk(MemCost, 1, AluLoadUnits, src = bit(b), dst = bit(d))
      case I.LoadIndirectI32(d, b, _) => mk(MemCost, 1, AluLoadUnits, src = bit(b), dst = bit(d))
      case I.LoadIndirectU64(d, b, _) => mk(MemCost, 1, AluLoadUnits, src = bit(b), dst = bit(d))
      case I.LoadU8(r, _) => mk(MemCost, 1, AluLoadUnits, dst = bit(r))
      case I.LoadI8(r, _) => mk(MemCost, 1, AluLoadUnits, dst = bit(r))
      case I.LoadU16(r, _) => mk(MemCost, 1, AluLoadUnits, dst = bit(r))
      case I.LoadI16(r, _) => mk(MemCost, 1, AluLoadUnits, dst = bit(r))
      case I.LoadU32(r, _) => mk(MemCost, 1, AluLoadUnits, dst = bit(r))
      case I.LoadI32(r, _) => mk(MemCost, 1, AluLoadUnits, dst = bit(r))
      case I.LoadU64(r, _) => mk(MemCost, 1, AluLoadUnits, dst = bit(r))

      case I.StoreImmIndirectU8(r, _, _) => mk(MemCost, 1, AluStoreUnits, src = bit(r))
      case I.StoreImmIndirectU16(r, _, _) => mk(MemCost, 1, AluStoreUnits, src = bit(r))
      case I.StoreImmIndirectU32(r, _, _) => mk(MemCost, 1, AluStoreUnits, src = bit(r))
      case I.StoreImmIndirectU64(r, _, _) => mk(MemCost, 1, AluStoreUnits, src = bit(r))
      case I.StoreIndirectU8(s, b, _) => mk(MemCost, 1, AluStoreUnits, src = bit(s) | bit(b))
      case I.StoreIndirectU16(s, b, _) => mk(MemCost, 1, AluStoreUnits, src = bit(s) | bit(b))
      case I.StoreIndirectU32(s, b, _) => mk(MemCost, 1, AluStoreUnits, src = bit(s) | bit(b))
      case I.StoreIndirectU64(s, b, _) => mk(MemCost, 1, AluStoreUnits, src = bit(s) | bit(b))
      case I.StoreImmU8(_, _) => mk(MemCost, 1, AluStoreUnits)
      case I.StoreImmU16(_, _) => mk(MemCost, 1, AluStoreUnits)
      case I.StoreImmU32(_, _) => mk(MemCost, 1, AluStoreUnits)
      case I.StoreImmU64(_, _) => mk(MemCost, 1, AluStoreUnits)
      case I.StoreU8(r, _) => mk(MemCost, 1, AluStoreUnits, src = bit(r))
      case I.StoreU16(r, _) => mk(MemCost, 1, AluStoreUnits, src = bit(r))
      case I.StoreU32(r, _) => mk(MemCost, 1, AluStoreUnits, src = bit(r))
      case I.StoreU64(r, _) => mk(MemCost, 1, AluStoreUnits, src = bit(r))

      case I.BranchEq(r1, r2, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r1) | bit(r2))
      case I.BranchNotEq(r1, r2, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r1) | bit(r2))
      case I.BranchLessUnsigned(r1, r2, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r1) | bit(r2))
      case I.BranchLessSigned(r1, r2, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r1) | bit(r2))
      case I.BranchGreaterOrEqualUnsigned(r1, r2, t) =>
        mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r1) | bit(r2))
      case I.BranchGreaterOrEqualSigned(r1, r2, t) =>
        mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r1) | bit(r2))
      case I.BranchEqImm(r, _, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r))
      case I.BranchNotEqImm(r, _, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r))
      case I.BranchLessUnsignedImm(r, _, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r))
      case I.BranchLessOrEqualUnsignedImm(r, _, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r))
      case I.BranchGreaterOrEqualUnsignedImm(r, _, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r))
      case I.BranchGreaterUnsignedImm(r, _, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r))
      case I.BranchLessSignedImm(r, _, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r))
      case I.BranchLessOrEqualSignedImm(r, _, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r))
      case I.BranchGreaterOrEqualSignedImm(r, _, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r))
      case I.BranchGreaterSignedImm(r, _, t) => mk(branchCost(code, iota, skip, t), 1, AluUnit, src = bit(r))

      case I.DivUnsigned32(d, s1, s2) => mk(60, 4, AluDivUnits, src = bit(s1) | bit(s2), dst = bit(d))
      case I.DivSigned32(d, s1, s2) => mk(60, 4, AluDivUnits, src = bit(s1) | bit(s2), dst = bit(d))
      case I.RemUnsigned32(d, s1, s2) => mk(60, 4, AluDivUnits, src = bit(s1) | bit(s2), dst = bit(d))
      case I.RemSigned32(d, s1, s2) => mk(60, 4, AluDivUnits, src = bit(s1) | bit(s2), dst = bit(d))
      case I.DivUnsigned64(d, s1, s2) => mk(60, 4, AluDivUnits, src = bit(s1) | bit(s2), dst = bit(d))
      case I.DivSigned64(d, s1, s2) => mk(60, 4, AluDivUnits, src = bit(s1) | bit(s2), dst = bit(d))
      case I.RemUnsigned64(d, s1, s2) => mk(60, 4, AluDivUnits, src = bit(s1) | bit(s2), dst = bit(d))
      case I.RemSigned64(d, s1, s2) => mk(60, 4, AluDivUnits, src = bit(s1) | bit(s2), dst = bit(d))

      case I.AndInverted(d, s1, s2) => mk(2, 3, AluUnit, src = bit(s1) | bit(s2), dst = bit(d))
      case I.OrInverted(d, s1, s2) => mk(2, 3, AluUnit, src = bit(s1) | bit(s2), dst = bit(d))
      case I.Xnor(d, s1, s2) => aluThreeOp(2, 2, 3, d, s1, s2, skip)
      case I.NegateAndAddImm64(d, s, _) => mk(2, 3, AluUnit, src = bit(s), dst = bit(d))
      case I.NegateAndAddImm32(d, s, _) => mk(3, 4, AluUnit, src = bit(s), dst = bit(d))
      case I.LoadImm(r, _) => mk(1, 1, NoUnits, dst = bit(r))
      case I.LoadImm64(r, _) => mk(1, 2, NoUnits, dst = bit(r))

      case I.Mul64(d, s1, s2) =>
        val src = bit(s1) | bit(s2)
        mk(3, p(1, 2, src, bit(d)), AluMulUnits, src = src, dst = bit(d))
      case I.Mul32(d, s1, s2) =>
        val src = bit(s1) | bit(s2)
        mk(4, p(2, 3, src, bit(d)), AluMulUnits, src = src, dst = bit(d))
      case I.MulImm64(d, s, _) => mk(3, p(1, 2, bit(s), bit(d)), AluMulUnits, src = bit(s), dst = bit(d))
      case I.MulImm32(d, s, _) => mk(4, p(2, 3, bit(s), bit(d)), AluMulUnits, src = bit(s), dst = bit(d))
      case I.MulUpperSignedSigned(d, s1, s2) => mk(4, 4, AluMulUnits, src = bit(s1) | bit(s2), dst = bit(d))
      case I.MulUpperUnsignedUnsigned(d, s1, s2) => mk(4, 4, AluMulUnits, src = bit(s1) | bit(s2), dst = bit(d))
      case I.MulUpperSignedUnsigned(d, s1, s2) => mk(6, 4, AluMulUnits, src = bit(s1) | bit(s2), dst = bit(d))

      case _ => InstrAttrs(2, 1, NoUnits, 0, 0, true, false, skip)

  private def aluThreeOp(cycles: Int, a: Int, b: Int, d: Int, s1: Int, s2: Int, skip: Int): InstrAttrs =
    val src = bit(s1) | bit(s2)
    val dst = bit(d)
    InstrAttrs(cycles, p(a, b, src, dst), AluUnit, src, dst, false, false, skip)

  private def aluTwoOp(cycles: Int, a: Int, b: Int, d: Int, s: Int, skip: Int): InstrAttrs =
    val src = bit(s)
    val dst = bit(d)
    InstrAttrs(cycles, p(a, b, src, dst), AluUnit, src, dst, false, false, skip)

  private def shiftOp(cycles: Int, a: Int, b: Int, d: Int, s1: Int, s2: Int, skip: Int): InstrAttrs =
    InstrAttrs(cycles, pShift(a, b, s1, d), AluUnit, bit(s1) | bit(s2), bit(d), false, false, skip)

  def gasCostForBlock(code: Array[Byte], bitmask: Array[Byte], blockStart: Int): Long =
    var iota = blockStart
    var iotaDone = false // iota == none
    var cycles = 0L
    var d = MaxDecodePerCycle
    var e = MaxStartPerCycle
    var units = InitialUnits
    val rob = ArrayBuffer.empty[RobEntry]
    // Attributes of the instruction at iota (recomputed only when iota moves).
    var attrs: InstrAttrs = attrsAt(code, bitmask, iota)

    def findReadyToStart(): Int =
      var j = 0
      while j < rob.length do
        val en = rob(j)
        if en.state == StateWaiting && (en.units <= units) then
          var depsOk = true
          val deps = en.deps
          var k = 0
          while depsOk && k < deps.length do
            if rob(deps(k)).cyclesLeft != 0 then depsOk = false
            k += 1
          if depsOk then return j
        j += 1
      -1

    while true do
      var live = 0
      var i = 0
      while i < rob.length do
        if rob(i).state != StateRetired then live += 1
        i += 1

      if !iotaDone && attrs.decodeSlots <= d && live < MaxLiveRobEntries then
        if attrs.isMoveReg then
          val src = attrs.srcMask
          val dst = attrs.dstMask
          var j = 0
          while j < rob.length do
            val en = rob(j)
            if (en.regsMask & src) != 0 then en.regsMask |= dst
            else en.regsMask &= ~dst
            j += 1
          d -= 1
          iota += attrs.fullSkip
          attrs = attrsAt(code, bitmask, iota)
        else
          val src = attrs.srcMask
          val dst = attrs.dstMask
          var depsCount = 0
          var j = 0
          while j < rob.length do
            if (rob(j).regsMask & src) != 0 then depsCount += 1
            j += 1
          val deps = new Array[Int](depsCount)
          var di = 0
          j = 0
          while j < rob.length do
            if (rob(j).regsMask & src) != 0 then
              deps(di) = j
              di += 1
            rob(j).regsMask &= ~dst
            j += 1
          rob += new RobEntry(StateDecoding, attrs.cycles, deps, dst, attrs.units)
          d -= attrs.decodeSlots
          if attrs.isTerminator then iotaDone = true
          else
            iota += attrs.fullSkip
            attrs = attrsAt(code, bitmask, iota)
      else
        val ready = findReadyToStart()
        if ready >= 0 && e > 0 then
          e -= 1
          val en = rob(ready)
          units = units - en.units
          en.state = StateExecuting
        else if iotaDone && live == 0 then
          return math.max(cycles - PipelineLatency, 1L)
        else
          d = MaxDecodePerCycle
          e = MaxStartPerCycle
          cycles += 1
          var j = 0
          while j < rob.length do
            val en = rob(j)
            if en.state == StateExecuting && en.cyclesLeft == 1 then units = units + en.units
            j += 1
          var prefixDone = true
          j = 0
          while j < rob.length do
            val en = rob(j)
            val oldState = en.state
            val oldCycles = en.cyclesLeft
            en.state =
              if prefixDone && (oldState == StateExecuted || oldState == StateRetired) then StateRetired
              else if oldState == StateDecoding then StateWaiting
              else if oldState == StateExecuting && oldCycles == 0 then StateExecuted
              else oldState
            if oldState == StateExecuting && oldCycles > 0 then en.cyclesLeft = oldCycles - 1
            prefixDone = prefixDone && (oldState == StateExecuted || oldState == StateRetired)
            j += 1

    throw new IllegalStateException("unreachable")

  val isTerminatorOpcodeValue: Array[Boolean] =
    val arr = new Array[Boolean](256)
    Opcode.values.foreach(op => arr(op.value) = op.startsNewBasicBlock)
    arr
