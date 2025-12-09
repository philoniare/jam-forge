package io.forge.jam.pvm

/**
 * PVM instruction opcodes
 *
 * Each opcode has a unique bytecode value used for instruction encoding.
 * Opcodes are grouped by category:
 * - Control flow: panic, fallthrough, jump, branches
 * - Memory: load, store (direct and indirect)
 * - Arithmetic: add, sub, mul, div, rem
 * - Bitwise: and, or, xor, shifts, rotates
 * - Comparison: set less/greater than
 * - Special: ecalli (host call), sbrk, sign/zero extend
 */
enum Opcode(val value: Int):

  // ============================================================================
  // Control Flow (0-49)
  // ============================================================================

  /** Panic - terminate execution with error */
  case Panic extends Opcode(0)

  /** Fallthrough - explicit basic block boundary */
  case Fallthrough extends Opcode(1)

  /** Memset - memory set operation */
  case Memset extends Opcode(2)

  /** Ecalli - host call instruction */
  case Ecalli extends Opcode(10)

  /** LoadImm64 - load 64-bit immediate */
  case LoadImm64 extends Opcode(20)

  /** StoreImmU8 - store 8-bit immediate to memory */
  case StoreImmU8 extends Opcode(30)

  /** StoreImmU16 - store 16-bit immediate to memory */
  case StoreImmU16 extends Opcode(31)

  /** StoreImmU32 - store 32-bit immediate to memory */
  case StoreImmU32 extends Opcode(32)

  /** StoreImmU64 - store 64-bit immediate to memory */
  case StoreImmU64 extends Opcode(33)

  /** Jump - unconditional direct jump */
  case Jump extends Opcode(40)

  /** JumpIndirect - unconditional indirect jump via register */
  case JumpIndirect extends Opcode(50)

  /** LoadImm - load immediate value into register */
  case LoadImm extends Opcode(51)

  /** LoadU8 - load unsigned 8-bit from memory */
  case LoadU8 extends Opcode(52)

  /** LoadI8 - load signed 8-bit from memory */
  case LoadI8 extends Opcode(53)

  /** LoadU16 - load unsigned 16-bit from memory */
  case LoadU16 extends Opcode(54)

  /** LoadI16 - load signed 16-bit from memory */
  case LoadI16 extends Opcode(55)

  /** LoadU32 - load unsigned 32-bit from memory */
  case LoadU32 extends Opcode(56)

  /** LoadI32 - load signed 32-bit from memory */
  case LoadI32 extends Opcode(57)

  /** LoadU64 - load unsigned 64-bit from memory */
  case LoadU64 extends Opcode(58)

  /** StoreU8 - store 8-bit to memory */
  case StoreU8 extends Opcode(59)

  /** StoreU16 - store 16-bit to memory */
  case StoreU16 extends Opcode(60)

  /** StoreU32 - store 32-bit to memory */
  case StoreU32 extends Opcode(61)

  /** StoreU64 - store 64-bit to memory */
  case StoreU64 extends Opcode(62)

  // ============================================================================
  // Store Immediate Indirect (70-79)
  // ============================================================================

  /** StoreImmIndirectU8 - store 8-bit immediate via register+offset */
  case StoreImmIndirectU8 extends Opcode(70)

  /** StoreImmIndirectU16 - store 16-bit immediate via register+offset */
  case StoreImmIndirectU16 extends Opcode(71)

  /** StoreImmIndirectU32 - store 32-bit immediate via register+offset */
  case StoreImmIndirectU32 extends Opcode(72)

  /** StoreImmIndirectU64 - store 64-bit immediate via register+offset */
  case StoreImmIndirectU64 extends Opcode(73)

  // ============================================================================
  // Branch with Immediate (80-99)
  // ============================================================================

  /** LoadImmAndJump - load immediate and jump */
  case LoadImmAndJump extends Opcode(80)

  /** BranchEqImm - branch if register equals immediate */
  case BranchEqImm extends Opcode(81)

  /** BranchNotEqImm - branch if register not equals immediate */
  case BranchNotEqImm extends Opcode(82)

  /** BranchLessUnsignedImm - branch if register < immediate (unsigned) */
  case BranchLessUnsignedImm extends Opcode(83)

  /** BranchLessOrEqualUnsignedImm - branch if register <= immediate (unsigned) */
  case BranchLessOrEqualUnsignedImm extends Opcode(84)

  /** BranchGreaterOrEqualUnsignedImm - branch if register >= immediate (unsigned) */
  case BranchGreaterOrEqualUnsignedImm extends Opcode(85)

  /** BranchGreaterUnsignedImm - branch if register > immediate (unsigned) */
  case BranchGreaterUnsignedImm extends Opcode(86)

  /** BranchLessSignedImm - branch if register < immediate (signed) */
  case BranchLessSignedImm extends Opcode(87)

  /** BranchLessOrEqualSignedImm - branch if register <= immediate (signed) */
  case BranchLessOrEqualSignedImm extends Opcode(88)

  /** BranchGreaterOrEqualSignedImm - branch if register >= immediate (signed) */
  case BranchGreaterOrEqualSignedImm extends Opcode(89)

  /** BranchGreaterSignedImm - branch if register > immediate (signed) */
  case BranchGreaterSignedImm extends Opcode(90)

  // ============================================================================
  // Two-Register Operations (100-119)
  // ============================================================================

  /** MoveReg - move value between registers */
  case MoveReg extends Opcode(100)

  /** Sbrk - extend heap */
  case Sbrk extends Opcode(101)

  /** CountSetBits64 - population count 64-bit */
  case CountSetBits64 extends Opcode(102)

  /** CountSetBits32 - population count 32-bit */
  case CountSetBits32 extends Opcode(103)

  /** CountLeadingZeroBits64 - count leading zeros 64-bit */
  case CountLeadingZeroBits64 extends Opcode(104)

  /** CountLeadingZeroBits32 - count leading zeros 32-bit */
  case CountLeadingZeroBits32 extends Opcode(105)

  /** CountTrailingZeroBits64 - count trailing zeros 64-bit */
  case CountTrailingZeroBits64 extends Opcode(106)

  /** CountTrailingZeroBits32 - count trailing zeros 32-bit */
  case CountTrailingZeroBits32 extends Opcode(107)

  /** SignExtend8 - sign-extend byte to word */
  case SignExtend8 extends Opcode(108)

  /** SignExtend16 - sign-extend halfword to word */
  case SignExtend16 extends Opcode(109)

  /** ZeroExtend16 - zero-extend halfword to word */
  case ZeroExtend16 extends Opcode(110)

  /** ReverseByte - reverse byte order */
  case ReverseByte extends Opcode(111)

  // ============================================================================
  // Indirect Memory Operations (120-130)
  // ============================================================================

  /** StoreIndirectU8 - store 8-bit via base register + offset */
  case StoreIndirectU8 extends Opcode(120)

  /** StoreIndirectU16 - store 16-bit via base register + offset */
  case StoreIndirectU16 extends Opcode(121)

  /** StoreIndirectU32 - store 32-bit via base register + offset */
  case StoreIndirectU32 extends Opcode(122)

  /** StoreIndirectU64 - store 64-bit via base register + offset */
  case StoreIndirectU64 extends Opcode(123)

  /** LoadIndirectU8 - load unsigned 8-bit via base register + offset */
  case LoadIndirectU8 extends Opcode(124)

  /** LoadIndirectI8 - load signed 8-bit via base register + offset */
  case LoadIndirectI8 extends Opcode(125)

  /** LoadIndirectU16 - load unsigned 16-bit via base register + offset */
  case LoadIndirectU16 extends Opcode(126)

  /** LoadIndirectI16 - load signed 16-bit via base register + offset */
  case LoadIndirectI16 extends Opcode(127)

  /** LoadIndirectU32 - load unsigned 32-bit via base register + offset */
  case LoadIndirectU32 extends Opcode(128)

  /** LoadIndirectI32 - load signed 32-bit via base register + offset */
  case LoadIndirectI32 extends Opcode(129)

  /** LoadIndirectU64 - load unsigned 64-bit via base register + offset */
  case LoadIndirectU64 extends Opcode(130)

  // ============================================================================
  // Arithmetic with Immediate - 32-bit (131-149)
  // ============================================================================

  /** AddImm32 - add immediate 32-bit */
  case AddImm32 extends Opcode(131)

  /** AndImm - bitwise AND with immediate */
  case AndImm extends Opcode(132)

  /** XorImm - bitwise XOR with immediate */
  case XorImm extends Opcode(133)

  /** OrImm - bitwise OR with immediate */
  case OrImm extends Opcode(134)

  /** MulImm32 - multiply immediate 32-bit */
  case MulImm32 extends Opcode(135)

  /** SetLessThanUnsignedImm - set if less than immediate (unsigned) */
  case SetLessThanUnsignedImm extends Opcode(136)

  /** SetLessThanSignedImm - set if less than immediate (signed) */
  case SetLessThanSignedImm extends Opcode(137)

  /** ShiftLogicalLeftImm32 - shift left by immediate 32-bit */
  case ShiftLogicalLeftImm32 extends Opcode(138)

  /** ShiftLogicalRightImm32 - logical shift right by immediate 32-bit */
  case ShiftLogicalRightImm32 extends Opcode(139)

  /** ShiftArithmeticRightImm32 - arithmetic shift right by immediate 32-bit */
  case ShiftArithmeticRightImm32 extends Opcode(140)

  /** NegateAndAddImm32 - negate and add immediate 32-bit */
  case NegateAndAddImm32 extends Opcode(141)

  /** SetGreaterThanUnsignedImm - set if greater than immediate (unsigned) */
  case SetGreaterThanUnsignedImm extends Opcode(142)

  /** SetGreaterThanSignedImm - set if greater than immediate (signed) */
  case SetGreaterThanSignedImm extends Opcode(143)

  /** ShiftLogicalLeftImmAlt32 - shift left by immediate alt 32-bit */
  case ShiftLogicalLeftImmAlt32 extends Opcode(144)

  /** ShiftLogicalRightImmAlt32 - logical shift right by immediate alt 32-bit */
  case ShiftLogicalRightImmAlt32 extends Opcode(145)

  /** ShiftArithmeticRightImmAlt32 - arithmetic shift right by immediate alt 32-bit */
  case ShiftArithmeticRightImmAlt32 extends Opcode(146)

  /** CmovIfZeroImm - conditional move if zero with immediate */
  case CmovIfZeroImm extends Opcode(147)

  /** CmovIfNotZeroImm - conditional move if not zero with immediate */
  case CmovIfNotZeroImm extends Opcode(148)

  // ============================================================================
  // Arithmetic with Immediate - 64-bit (149-159)
  // ============================================================================

  /** AddImm64 - add immediate 64-bit */
  case AddImm64 extends Opcode(149)

  /** MulImm64 - multiply immediate 64-bit */
  case MulImm64 extends Opcode(150)

  /** ShiftLogicalLeftImm64 - shift left by immediate 64-bit */
  case ShiftLogicalLeftImm64 extends Opcode(151)

  /** ShiftLogicalRightImm64 - logical shift right by immediate 64-bit */
  case ShiftLogicalRightImm64 extends Opcode(152)

  /** ShiftArithmeticRightImm64 - arithmetic shift right by immediate 64-bit */
  case ShiftArithmeticRightImm64 extends Opcode(153)

  /** NegateAndAddImm64 - negate and add immediate 64-bit */
  case NegateAndAddImm64 extends Opcode(154)

  /** ShiftLogicalLeftImmAlt64 - shift left by immediate alt 64-bit */
  case ShiftLogicalLeftImmAlt64 extends Opcode(155)

  /** ShiftLogicalRightImmAlt64 - logical shift right by immediate alt 64-bit */
  case ShiftLogicalRightImmAlt64 extends Opcode(156)

  /** ShiftArithmeticRightImmAlt64 - arithmetic shift right by immediate alt 64-bit */
  case ShiftArithmeticRightImmAlt64 extends Opcode(157)

  /** RotateRightImm64 - rotate right by immediate 64-bit */
  case RotateRightImm64 extends Opcode(158)

  /** RotateRightImmAlt64 - rotate right by immediate alt 64-bit */
  case RotateRightImmAlt64 extends Opcode(159)

  /** RotateRightImm32 - rotate right by immediate 32-bit */
  case RotateRightImm32 extends Opcode(160)

  /** RotateRightImmAlt32 - rotate right by immediate alt 32-bit */
  case RotateRightImmAlt32 extends Opcode(161)

  // ============================================================================
  // Branch with Two Registers (170-179)
  // ============================================================================

  /** BranchEq - branch if two registers are equal */
  case BranchEq extends Opcode(170)

  /** BranchNotEq - branch if two registers are not equal */
  case BranchNotEq extends Opcode(171)

  /** BranchLessUnsigned - branch if r1 < r2 (unsigned) */
  case BranchLessUnsigned extends Opcode(172)

  /** BranchLessSigned - branch if r1 < r2 (signed) */
  case BranchLessSigned extends Opcode(173)

  /** BranchGreaterOrEqualUnsigned - branch if r1 >= r2 (unsigned) */
  case BranchGreaterOrEqualUnsigned extends Opcode(174)

  /** BranchGreaterOrEqualSigned - branch if r1 >= r2 (signed) */
  case BranchGreaterOrEqualSigned extends Opcode(175)

  // ============================================================================
  // Load Immediate and Jump Indirect (180)
  // ============================================================================

  /** LoadImmAndJumpIndirect - load immediate and jump via register */
  case LoadImmAndJumpIndirect extends Opcode(180)

  // ============================================================================
  // Three-Register Arithmetic - 32-bit (190-199)
  // ============================================================================

  /** Add32 - add two registers 32-bit */
  case Add32 extends Opcode(190)

  /** Sub32 - subtract two registers 32-bit */
  case Sub32 extends Opcode(191)

  /** Mul32 - multiply two registers 32-bit */
  case Mul32 extends Opcode(192)

  /** DivUnsigned32 - unsigned divide 32-bit */
  case DivUnsigned32 extends Opcode(193)

  /** DivSigned32 - signed divide 32-bit */
  case DivSigned32 extends Opcode(194)

  /** RemUnsigned32 - unsigned remainder 32-bit */
  case RemUnsigned32 extends Opcode(195)

  /** RemSigned32 - signed remainder 32-bit */
  case RemSigned32 extends Opcode(196)

  /** ShiftLogicalLeft32 - shift left 32-bit */
  case ShiftLogicalLeft32 extends Opcode(197)

  /** ShiftLogicalRight32 - logical shift right 32-bit */
  case ShiftLogicalRight32 extends Opcode(198)

  /** ShiftArithmeticRight32 - arithmetic shift right 32-bit */
  case ShiftArithmeticRight32 extends Opcode(199)

  // ============================================================================
  // Three-Register Arithmetic - 64-bit (200-209)
  // ============================================================================

  /** Add64 - add two registers 64-bit */
  case Add64 extends Opcode(200)

  /** Sub64 - subtract two registers 64-bit */
  case Sub64 extends Opcode(201)

  /** Mul64 - multiply two registers 64-bit */
  case Mul64 extends Opcode(202)

  /** DivUnsigned64 - unsigned divide 64-bit */
  case DivUnsigned64 extends Opcode(203)

  /** DivSigned64 - signed divide 64-bit */
  case DivSigned64 extends Opcode(204)

  /** RemUnsigned64 - unsigned remainder 64-bit */
  case RemUnsigned64 extends Opcode(205)

  /** RemSigned64 - signed remainder 64-bit */
  case RemSigned64 extends Opcode(206)

  /** ShiftLogicalLeft64 - shift left 64-bit */
  case ShiftLogicalLeft64 extends Opcode(207)

  /** ShiftLogicalRight64 - logical shift right 64-bit */
  case ShiftLogicalRight64 extends Opcode(208)

  /** ShiftArithmeticRight64 - arithmetic shift right 64-bit */
  case ShiftArithmeticRight64 extends Opcode(209)

  // ============================================================================
  // Three-Register Bitwise and Comparison (210-230)
  // ============================================================================

  /** And - bitwise AND */
  case And extends Opcode(210)

  /** Xor - bitwise XOR */
  case Xor extends Opcode(211)

  /** Or - bitwise OR */
  case Or extends Opcode(212)

  /** MulUpperSignedSigned - upper bits of signed * signed multiply */
  case MulUpperSignedSigned extends Opcode(213)

  /** MulUpperUnsignedUnsigned - upper bits of unsigned * unsigned multiply */
  case MulUpperUnsignedUnsigned extends Opcode(214)

  /** MulUpperSignedUnsigned - upper bits of signed * unsigned multiply */
  case MulUpperSignedUnsigned extends Opcode(215)

  /** SetLessThanUnsigned - set if less than (unsigned) */
  case SetLessThanUnsigned extends Opcode(216)

  /** SetLessThanSigned - set if less than (signed) */
  case SetLessThanSigned extends Opcode(217)

  /** CmovIfZero - conditional move if zero */
  case CmovIfZero extends Opcode(218)

  /** CmovIfNotZero - conditional move if not zero */
  case CmovIfNotZero extends Opcode(219)

  /** RotateLeft64 - rotate left 64-bit */
  case RotateLeft64 extends Opcode(220)

  /** RotateLeft32 - rotate left 32-bit */
  case RotateLeft32 extends Opcode(221)

  /** RotateRight64 - rotate right 64-bit */
  case RotateRight64 extends Opcode(222)

  /** RotateRight32 - rotate right 32-bit */
  case RotateRight32 extends Opcode(223)

  /** AndInverted - bitwise AND with second operand inverted */
  case AndInverted extends Opcode(224)

  /** OrInverted - bitwise OR with second operand inverted */
  case OrInverted extends Opcode(225)

  /** Xnor - bitwise XNOR */
  case Xnor extends Opcode(226)

  /** Maximum - signed maximum */
  case Maximum extends Opcode(227)

  /** MaximumUnsigned - unsigned maximum */
  case MaximumUnsigned extends Opcode(228)

  /** Minimum - signed minimum */
  case Minimum extends Opcode(229)

  /** MinimumUnsigned - unsigned minimum */
  case MinimumUnsigned extends Opcode(230)

  /**
   * Returns true if this instruction starts a new basic block.
   *
   * Control flow instructions (jumps, branches) terminate the current
   * basic block and start a new one.
   */
  def startsNewBasicBlock: Boolean = this match
    case Panic | Fallthrough | Jump | JumpIndirect |
         LoadImmAndJump | LoadImmAndJumpIndirect |
         BranchEq | BranchNotEq |
         BranchLessUnsigned | BranchLessSigned |
         BranchGreaterOrEqualUnsigned | BranchGreaterOrEqualSigned |
         BranchEqImm | BranchNotEqImm |
         BranchLessUnsignedImm | BranchLessSignedImm |
         BranchGreaterOrEqualUnsignedImm | BranchGreaterOrEqualSignedImm |
         BranchLessOrEqualSignedImm | BranchLessOrEqualUnsignedImm |
         BranchGreaterSignedImm | BranchGreaterUnsignedImm => true
    case _ => false

  /**
   * Returns true if execution can fall through to the next instruction.
   *
   * Unconditional jumps and panic cannot fall through.
   * Conditional branches can fall through if not taken.
   */
  def canFallthrough: Boolean = this match
    case Panic | Jump | JumpIndirect |
         LoadImmAndJump | LoadImmAndJumpIndirect => false
    case _ => true

end Opcode

object Opcode:
  /** Lookup table for bytecode to opcode mapping */
  private val byValue: Map[Int, Opcode] =
    Opcode.values.map(op => op.value -> op).toMap

  /**
   * Look up an opcode by its bytecode value.
   *
   * @param value The bytecode value (0-255)
   * @return Some(opcode) if valid, None if invalid
   */
  def fromByte(value: Int): Option[Opcode] = byValue.get(value)

  /**
   * Look up an opcode by its bytecode value, throwing if invalid.
   *
   * @param value The bytecode value (0-255)
   * @return The corresponding opcode
   * @throws NoSuchElementException if value is not a valid opcode
   */
  def fromByteOrThrow(value: Int): Opcode =
    fromByte(value).getOrElse(
      throw new NoSuchElementException(s"Invalid opcode: $value")
    )
