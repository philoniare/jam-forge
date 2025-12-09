package io.forge.jam.pvm

/**
 * Sealed trait hierarchy for PVM instructions.
 *
 * Instructions are organized by operand pattern:
 * - Argless: no operands (Panic, Fallthrough, Memset)
 * - RegImm: register + immediate
 * - RegRegImm: two registers + immediate
 * - RegRegReg: three registers
 * - ImmImm: two immediates
 * - Imm: single immediate
 * - RegReg: two registers
 * - RegRegImmImm: two registers + two immediates
 *
 * Width-polymorphic instructions use separate case classes (Add32 vs Add64)
 * rather than GADTs for simpler pattern matching and reduced type complexity.
 */
sealed trait Instruction:
  /** The opcode for this instruction */
  def opcode: Opcode

object Instruction:

  // ============================================================================
  // Argless Instructions
  // ============================================================================

  /** Panic - terminate execution with error */
  case object Panic extends Instruction:
    def opcode: Opcode = Opcode.Panic

  /** Fallthrough - explicit basic block boundary */
  case object Fallthrough extends Instruction:
    def opcode: Opcode = Opcode.Fallthrough

  /** Memset - memory set operation */
  case object Memset extends Instruction:
    def opcode: Opcode = Opcode.Memset

  /** Invalid - represents an invalid/unknown instruction */
  case object Invalid extends Instruction:
    def opcode: Opcode = Opcode.Panic

  // ============================================================================
  // Single Immediate Instructions
  // ============================================================================

  /** Jump - unconditional direct jump */
  final case class Jump(target: Long) extends Instruction:
    def opcode: Opcode = Opcode.Jump

  /** Ecalli - host call instruction */
  final case class Ecalli(hostId: Long) extends Instruction:
    def opcode: Opcode = Opcode.Ecalli

  // ============================================================================
  // Two Immediate Instructions
  // ============================================================================

  /** StoreImmU8 - store 8-bit immediate to memory address */
  final case class StoreImmU8(address: Long, value: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreImmU8

  /** StoreImmU16 - store 16-bit immediate to memory address */
  final case class StoreImmU16(address: Long, value: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreImmU16

  /** StoreImmU32 - store 32-bit immediate to memory address */
  final case class StoreImmU32(address: Long, value: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreImmU32

  /** StoreImmU64 - store 64-bit immediate to memory address */
  final case class StoreImmU64(address: Long, value: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreImmU64

  // ============================================================================
  // Register + Immediate Instructions
  // ============================================================================

  /** JumpIndirect - jump to address in register + offset */
  final case class JumpIndirect(reg: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.JumpIndirect

  /** LoadImm - load 32-bit immediate into register */
  final case class LoadImm(reg: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadImm

  /** LoadImm64 - load 64-bit immediate into register */
  final case class LoadImm64(reg: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadImm64

  /** LoadU8 - load unsigned 8-bit from address */
  final case class LoadU8(reg: Int, address: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadU8

  /** LoadI8 - load signed 8-bit from address */
  final case class LoadI8(reg: Int, address: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadI8

  /** LoadU16 - load unsigned 16-bit from address */
  final case class LoadU16(reg: Int, address: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadU16

  /** LoadI16 - load signed 16-bit from address */
  final case class LoadI16(reg: Int, address: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadI16

  /** LoadU32 - load unsigned 32-bit from address */
  final case class LoadU32(reg: Int, address: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadU32

  /** LoadI32 - load signed 32-bit from address */
  final case class LoadI32(reg: Int, address: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadI32

  /** LoadU64 - load unsigned 64-bit from address */
  final case class LoadU64(reg: Int, address: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadU64

  /** StoreU8 - store 8-bit from register to address */
  final case class StoreU8(reg: Int, address: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreU8

  /** StoreU16 - store 16-bit from register to address */
  final case class StoreU16(reg: Int, address: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreU16

  /** StoreU32 - store 32-bit from register to address */
  final case class StoreU32(reg: Int, address: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreU32

  /** StoreU64 - store 64-bit from register to address */
  final case class StoreU64(reg: Int, address: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreU64

  // ============================================================================
  // Register + Immediate + Offset Instructions (Branches with Immediate)
  // ============================================================================

  /** LoadImmAndJump - load immediate and jump to offset */
  final case class LoadImmAndJump(reg: Int, imm: Long, target: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadImmAndJump

  /** BranchEqImm - branch if register equals immediate */
  final case class BranchEqImm(reg: Int, imm: Long, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchEqImm

  /** BranchNotEqImm - branch if register not equals immediate */
  final case class BranchNotEqImm(reg: Int, imm: Long, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchNotEqImm

  /** BranchLessUnsignedImm - branch if register < immediate (unsigned) */
  final case class BranchLessUnsignedImm(reg: Int, imm: Long, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchLessUnsignedImm

  /** BranchLessSignedImm - branch if register < immediate (signed) */
  final case class BranchLessSignedImm(reg: Int, imm: Long, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchLessSignedImm

  /** BranchGreaterOrEqualUnsignedImm - branch if register >= immediate (unsigned) */
  final case class BranchGreaterOrEqualUnsignedImm(reg: Int, imm: Long, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchGreaterOrEqualUnsignedImm

  /** BranchGreaterOrEqualSignedImm - branch if register >= immediate (signed) */
  final case class BranchGreaterOrEqualSignedImm(reg: Int, imm: Long, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchGreaterOrEqualSignedImm

  /** BranchLessOrEqualSignedImm - branch if register <= immediate (signed) */
  final case class BranchLessOrEqualSignedImm(reg: Int, imm: Long, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchLessOrEqualSignedImm

  /** BranchLessOrEqualUnsignedImm - branch if register <= immediate (unsigned) */
  final case class BranchLessOrEqualUnsignedImm(reg: Int, imm: Long, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchLessOrEqualUnsignedImm

  /** BranchGreaterSignedImm - branch if register > immediate (signed) */
  final case class BranchGreaterSignedImm(reg: Int, imm: Long, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchGreaterSignedImm

  /** BranchGreaterUnsignedImm - branch if register > immediate (unsigned) */
  final case class BranchGreaterUnsignedImm(reg: Int, imm: Long, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchGreaterUnsignedImm

  // ============================================================================
  // Store Immediate Indirect (reg + offset -> value)
  // ============================================================================

  /** StoreImmIndirectU8 - store 8-bit immediate via register+offset */
  final case class StoreImmIndirectU8(reg: Int, offset: Long, value: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreImmIndirectU8

  /** StoreImmIndirectU16 - store 16-bit immediate via register+offset */
  final case class StoreImmIndirectU16(reg: Int, offset: Long, value: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreImmIndirectU16

  /** StoreImmIndirectU32 - store 32-bit immediate via register+offset */
  final case class StoreImmIndirectU32(reg: Int, offset: Long, value: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreImmIndirectU32

  /** StoreImmIndirectU64 - store 64-bit immediate via register+offset */
  final case class StoreImmIndirectU64(reg: Int, offset: Long, value: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreImmIndirectU64

  // ============================================================================
  // Two Register Instructions
  // ============================================================================

  /** MoveReg - copy value from one register to another */
  final case class MoveReg(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.MoveReg

  /** Sbrk - extend heap, dst = old heap pointer, size from src */
  final case class Sbrk(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.Sbrk

  /** CountLeadingZeroBits32 - count leading zeros 32-bit */
  final case class CountLeadingZeroBits32(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.CountLeadingZeroBits32

  /** CountLeadingZeroBits64 - count leading zeros 64-bit */
  final case class CountLeadingZeroBits64(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.CountLeadingZeroBits64

  /** CountTrailingZeroBits32 - count trailing zeros 32-bit */
  final case class CountTrailingZeroBits32(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.CountTrailingZeroBits32

  /** CountTrailingZeroBits64 - count trailing zeros 64-bit */
  final case class CountTrailingZeroBits64(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.CountTrailingZeroBits64

  /** CountSetBits32 - population count 32-bit */
  final case class CountSetBits32(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.CountSetBits32

  /** CountSetBits64 - population count 64-bit */
  final case class CountSetBits64(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.CountSetBits64

  /** SignExtend8 - sign-extend byte to word */
  final case class SignExtend8(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.SignExtend8

  /** SignExtend16 - sign-extend halfword to word */
  final case class SignExtend16(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.SignExtend16

  /** ZeroExtend16 - zero-extend halfword to word */
  final case class ZeroExtend16(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.ZeroExtend16

  /** ReverseByte - reverse byte order */
  final case class ReverseByte(dst: Int, src: Int) extends Instruction:
    def opcode: Opcode = Opcode.ReverseByte

  // ============================================================================
  // Two Register + Immediate Instructions (Arithmetic/Logic with Immediate)
  // ============================================================================

  /** AddImm32 - dst = src + imm (32-bit) */
  final case class AddImm32(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.AddImm32

  /** AddImm64 - dst = src + imm (64-bit) */
  final case class AddImm64(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.AddImm64

  /** AndImm - dst = src & imm */
  final case class AndImm(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.AndImm

  /** XorImm - dst = src ^ imm */
  final case class XorImm(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.XorImm

  /** OrImm - dst = src | imm */
  final case class OrImm(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.OrImm

  /** MulImm32 - dst = src * imm (32-bit) */
  final case class MulImm32(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.MulImm32

  /** MulImm64 - dst = src * imm (64-bit) */
  final case class MulImm64(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.MulImm64

  /** SetLessThanUnsignedImm - dst = (src < imm) ? 1 : 0 (unsigned) */
  final case class SetLessThanUnsignedImm(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.SetLessThanUnsignedImm

  /** SetLessThanSignedImm - dst = (src < imm) ? 1 : 0 (signed) */
  final case class SetLessThanSignedImm(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.SetLessThanSignedImm

  /** SetGreaterThanUnsignedImm - dst = (src > imm) ? 1 : 0 (unsigned) */
  final case class SetGreaterThanUnsignedImm(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.SetGreaterThanUnsignedImm

  /** SetGreaterThanSignedImm - dst = (src > imm) ? 1 : 0 (signed) */
  final case class SetGreaterThanSignedImm(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.SetGreaterThanSignedImm

  /** ShiftLogicalLeftImm32 - dst = src << imm (32-bit) */
  final case class ShiftLogicalLeftImm32(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalLeftImm32

  /** ShiftLogicalLeftImm64 - dst = src << imm (64-bit) */
  final case class ShiftLogicalLeftImm64(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalLeftImm64

  /** ShiftLogicalRightImm32 - dst = src >>> imm (32-bit) */
  final case class ShiftLogicalRightImm32(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalRightImm32

  /** ShiftLogicalRightImm64 - dst = src >>> imm (64-bit) */
  final case class ShiftLogicalRightImm64(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalRightImm64

  /** ShiftArithmeticRightImm32 - dst = src >> imm (32-bit, signed) */
  final case class ShiftArithmeticRightImm32(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftArithmeticRightImm32

  /** ShiftArithmeticRightImm64 - dst = src >> imm (64-bit, signed) */
  final case class ShiftArithmeticRightImm64(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftArithmeticRightImm64

  /** NegateAndAddImm32 - dst = imm - src (32-bit) */
  final case class NegateAndAddImm32(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.NegateAndAddImm32

  /** NegateAndAddImm64 - dst = imm - src (64-bit) */
  final case class NegateAndAddImm64(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.NegateAndAddImm64

  /** ShiftLogicalLeftImmAlt32 - alternative shift left (32-bit) */
  final case class ShiftLogicalLeftImmAlt32(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalLeftImmAlt32

  /** ShiftLogicalLeftImmAlt64 - alternative shift left (64-bit) */
  final case class ShiftLogicalLeftImmAlt64(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalLeftImmAlt64

  /** ShiftLogicalRightImmAlt32 - alternative logical shift right (32-bit) */
  final case class ShiftLogicalRightImmAlt32(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalRightImmAlt32

  /** ShiftLogicalRightImmAlt64 - alternative logical shift right (64-bit) */
  final case class ShiftLogicalRightImmAlt64(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalRightImmAlt64

  /** ShiftArithmeticRightImmAlt32 - alternative arithmetic shift right (32-bit) */
  final case class ShiftArithmeticRightImmAlt32(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftArithmeticRightImmAlt32

  /** ShiftArithmeticRightImmAlt64 - alternative arithmetic shift right (64-bit) */
  final case class ShiftArithmeticRightImmAlt64(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.ShiftArithmeticRightImmAlt64

  /** CmovIfZeroImm - dst = (src == 0) ? imm : dst */
  final case class CmovIfZeroImm(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.CmovIfZeroImm

  /** CmovIfNotZeroImm - dst = (src != 0) ? imm : dst */
  final case class CmovIfNotZeroImm(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.CmovIfNotZeroImm

  /** RotateRightImm32 - rotate right by immediate (32-bit) */
  final case class RotateRightImm32(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.RotateRightImm32

  /** RotateRightImm64 - rotate right by immediate (64-bit) */
  final case class RotateRightImm64(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.RotateRightImm64

  /** RotateRightImmAlt32 - alternative rotate right by immediate (32-bit) */
  final case class RotateRightImmAlt32(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.RotateRightImmAlt32

  /** RotateRightImmAlt64 - alternative rotate right by immediate (64-bit) */
  final case class RotateRightImmAlt64(dst: Int, src: Int, imm: Long) extends Instruction:
    def opcode: Opcode = Opcode.RotateRightImmAlt64

  // ============================================================================
  // Two Register + Offset Instructions (Indirect Memory)
  // ============================================================================

  /** StoreIndirectU8 - store 8-bit via base register + offset */
  final case class StoreIndirectU8(src: Int, base: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreIndirectU8

  /** StoreIndirectU16 - store 16-bit via base register + offset */
  final case class StoreIndirectU16(src: Int, base: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreIndirectU16

  /** StoreIndirectU32 - store 32-bit via base register + offset */
  final case class StoreIndirectU32(src: Int, base: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreIndirectU32

  /** StoreIndirectU64 - store 64-bit via base register + offset */
  final case class StoreIndirectU64(src: Int, base: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.StoreIndirectU64

  /** LoadIndirectU8 - load unsigned 8-bit via base register + offset */
  final case class LoadIndirectU8(dst: Int, base: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadIndirectU8

  /** LoadIndirectI8 - load signed 8-bit via base register + offset */
  final case class LoadIndirectI8(dst: Int, base: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadIndirectI8

  /** LoadIndirectU16 - load unsigned 16-bit via base register + offset */
  final case class LoadIndirectU16(dst: Int, base: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadIndirectU16

  /** LoadIndirectI16 - load signed 16-bit via base register + offset */
  final case class LoadIndirectI16(dst: Int, base: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadIndirectI16

  /** LoadIndirectU32 - load unsigned 32-bit via base register + offset */
  final case class LoadIndirectU32(dst: Int, base: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadIndirectU32

  /** LoadIndirectI32 - load signed 32-bit via base register + offset */
  final case class LoadIndirectI32(dst: Int, base: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadIndirectI32

  /** LoadIndirectU64 - load unsigned 64-bit via base register + offset */
  final case class LoadIndirectU64(dst: Int, base: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadIndirectU64

  // ============================================================================
  // Two Register + Offset Instructions (Branches)
  // ============================================================================

  /** BranchEq - branch if r1 == r2 */
  final case class BranchEq(r1: Int, r2: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchEq

  /** BranchNotEq - branch if r1 != r2 */
  final case class BranchNotEq(r1: Int, r2: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchNotEq

  /** BranchLessUnsigned - branch if r1 < r2 (unsigned) */
  final case class BranchLessUnsigned(r1: Int, r2: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchLessUnsigned

  /** BranchLessSigned - branch if r1 < r2 (signed) */
  final case class BranchLessSigned(r1: Int, r2: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchLessSigned

  /** BranchGreaterOrEqualUnsigned - branch if r1 >= r2 (unsigned) */
  final case class BranchGreaterOrEqualUnsigned(r1: Int, r2: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchGreaterOrEqualUnsigned

  /** BranchGreaterOrEqualSigned - branch if r1 >= r2 (signed) */
  final case class BranchGreaterOrEqualSigned(r1: Int, r2: Int, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.BranchGreaterOrEqualSigned

  // ============================================================================
  // Three Register Instructions (Arithmetic/Logic)
  // ============================================================================

  /** Add32 - d = s1 + s2 (32-bit) */
  final case class Add32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.Add32

  /** Add64 - d = s1 + s2 (64-bit) */
  final case class Add64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.Add64

  /** Sub32 - d = s1 - s2 (32-bit) */
  final case class Sub32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.Sub32

  /** Sub64 - d = s1 - s2 (64-bit) */
  final case class Sub64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.Sub64

  /** Mul32 - d = s1 * s2 (32-bit) */
  final case class Mul32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.Mul32

  /** Mul64 - d = s1 * s2 (64-bit) */
  final case class Mul64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.Mul64

  /** DivUnsigned32 - d = s1 / s2 (32-bit, unsigned) */
  final case class DivUnsigned32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.DivUnsigned32

  /** DivUnsigned64 - d = s1 / s2 (64-bit, unsigned) */
  final case class DivUnsigned64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.DivUnsigned64

  /** DivSigned32 - d = s1 / s2 (32-bit, signed) */
  final case class DivSigned32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.DivSigned32

  /** DivSigned64 - d = s1 / s2 (64-bit, signed) */
  final case class DivSigned64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.DivSigned64

  /** RemUnsigned32 - d = s1 % s2 (32-bit, unsigned) */
  final case class RemUnsigned32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.RemUnsigned32

  /** RemUnsigned64 - d = s1 % s2 (64-bit, unsigned) */
  final case class RemUnsigned64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.RemUnsigned64

  /** RemSigned32 - d = s1 % s2 (32-bit, signed) */
  final case class RemSigned32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.RemSigned32

  /** RemSigned64 - d = s1 % s2 (64-bit, signed) */
  final case class RemSigned64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.RemSigned64

  /** And - d = s1 & s2 */
  final case class And(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.And

  /** Xor - d = s1 ^ s2 */
  final case class Xor(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.Xor

  /** Or - d = s1 | s2 */
  final case class Or(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.Or

  /** AndInverted - d = s1 & ~s2 */
  final case class AndInverted(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.AndInverted

  /** OrInverted - d = s1 | ~s2 */
  final case class OrInverted(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.OrInverted

  /** Xnor - d = ~(s1 ^ s2) */
  final case class Xnor(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.Xnor

  /** ShiftLogicalLeft32 - d = s1 << s2 (32-bit) */
  final case class ShiftLogicalLeft32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalLeft32

  /** ShiftLogicalLeft64 - d = s1 << s2 (64-bit) */
  final case class ShiftLogicalLeft64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalLeft64

  /** ShiftLogicalRight32 - d = s1 >>> s2 (32-bit) */
  final case class ShiftLogicalRight32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalRight32

  /** ShiftLogicalRight64 - d = s1 >>> s2 (64-bit) */
  final case class ShiftLogicalRight64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.ShiftLogicalRight64

  /** ShiftArithmeticRight32 - d = s1 >> s2 (32-bit, signed) */
  final case class ShiftArithmeticRight32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.ShiftArithmeticRight32

  /** ShiftArithmeticRight64 - d = s1 >> s2 (64-bit, signed) */
  final case class ShiftArithmeticRight64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.ShiftArithmeticRight64

  /** RotateLeft32 - rotate left (32-bit) */
  final case class RotateLeft32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.RotateLeft32

  /** RotateLeft64 - rotate left (64-bit) */
  final case class RotateLeft64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.RotateLeft64

  /** RotateRight32 - rotate right (32-bit) */
  final case class RotateRight32(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.RotateRight32

  /** RotateRight64 - rotate right (64-bit) */
  final case class RotateRight64(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.RotateRight64

  /** SetLessThanUnsigned - d = (s1 < s2) ? 1 : 0 (unsigned) */
  final case class SetLessThanUnsigned(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.SetLessThanUnsigned

  /** SetLessThanSigned - d = (s1 < s2) ? 1 : 0 (signed) */
  final case class SetLessThanSigned(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.SetLessThanSigned

  /** CmovIfZero - d = (s2 == 0) ? s1 : d */
  final case class CmovIfZero(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.CmovIfZero

  /** CmovIfNotZero - d = (s2 != 0) ? s1 : d */
  final case class CmovIfNotZero(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.CmovIfNotZero

  /** Maximum - d = max(s1, s2) (signed) */
  final case class Maximum(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.Maximum

  /** MaximumUnsigned - d = max(s1, s2) (unsigned) */
  final case class MaximumUnsigned(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.MaximumUnsigned

  /** Minimum - d = min(s1, s2) (signed) */
  final case class Minimum(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.Minimum

  /** MinimumUnsigned - d = min(s1, s2) (unsigned) */
  final case class MinimumUnsigned(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.MinimumUnsigned

  /** MulUpperSignedSigned - upper bits of signed * signed multiply */
  final case class MulUpperSignedSigned(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.MulUpperSignedSigned

  /** MulUpperUnsignedUnsigned - upper bits of unsigned * unsigned multiply */
  final case class MulUpperUnsignedUnsigned(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.MulUpperUnsignedUnsigned

  /** MulUpperSignedUnsigned - upper bits of signed * unsigned multiply */
  final case class MulUpperSignedUnsigned(d: Int, s1: Int, s2: Int) extends Instruction:
    def opcode: Opcode = Opcode.MulUpperSignedUnsigned

  // ============================================================================
  // Four Operand Instructions
  // ============================================================================

  /** LoadImmAndJumpIndirect - load immediate and jump indirect */
  final case class LoadImmAndJumpIndirect(dst: Int, base: Int, imm: Long, offset: Long) extends Instruction:
    def opcode: Opcode = Opcode.LoadImmAndJumpIndirect
