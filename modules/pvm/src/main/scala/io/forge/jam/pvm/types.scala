package io.forge.jam.pvm

import scala.annotation.targetName
import spire.math.{UByte, UShort, UInt, ULong}

/**
 * Core type definitions for the JAM PVM using Spire unsigned types.
 *
 * Design Philosophy:
 * - Opaque types for zero-cost type safety
 * - Phantom types for compile-time width tracking
 * - Spire unsigned types for correct unsigned semantics
 */
object types:

  // ══════════════════════════════════════════════════════════════════════════
  // Phantom Types for Compile-Time Width and Signedness Tracking
  // ══════════════════════════════════════════════════════════════════════════

  /** Phantom type for instruction/register width */
  sealed trait Width
  sealed trait W32 extends Width
  sealed trait W64 extends Width

  /** Phantom type for signedness */
  sealed trait Signedness
  sealed trait Signed extends Signedness
  sealed trait Unsigned extends Signedness

  // ══════════════════════════════════════════════════════════════════════════
  // Opaque Types with Spire Unsigned Backing
  // ══════════════════════════════════════════════════════════════════════════

  /** Memory address (32-bit unsigned) */
  opaque type Address = UInt
  object Address:
    inline def apply(v: UInt): Address = v
    @targetName("addressFromInt")
    inline def apply(v: Int): Address = UInt(v)
    @targetName("addressFromLong")
    inline def apply(v: Long): Address = UInt(v.toInt)
    val Zero: Address = UInt(0)

  extension (a: Address)
    @targetName("addressValue")
    inline def value: UInt = a
    @targetName("addressPlusUInt")
    inline def +(offset: UInt): Address = a + offset
    @targetName("addressPlusInt")
    inline def +(offset: Int): Address = a + UInt(offset)
    @targetName("addressToInt")
    inline def toInt: Int = a.signed
    @targetName("addressToLong")
    inline def toLong: Long = a.toLong

  /** Program counter (32-bit unsigned) */
  opaque type ProgramCounter = UInt
  object ProgramCounter:
    inline def apply(v: UInt): ProgramCounter = v
    @targetName("pcFromInt")
    inline def apply(v: Int): ProgramCounter = UInt(v)
    val MaxValue: ProgramCounter = UInt(-1)  // 0xFFFFFFFF

  extension (pc: ProgramCounter)
    @targetName("pcValue")
    inline def value: UInt = pc
    @targetName("pcPlusInt")
    inline def +(offset: Int): ProgramCounter = pc + UInt(offset)
    @targetName("pcToInt")
    inline def toInt: Int = pc.signed

  /** Gas counter (64-bit signed - can go negative to indicate exhaustion) */
  opaque type Gas = Long
  object Gas:
    inline def apply(v: Long): Gas = v
    val Zero: Gas = 0L

  extension (g: Gas)
    @targetName("gasValue")
    inline def value: Long = g
    @targetName("gasMinusLong")
    inline def -(cost: Long): Gas = g - cost
    @targetName("gasPlusLong")
    inline def +(amount: Long): Gas = g + amount
    @targetName("gasIsExhausted")
    inline def isExhausted: Boolean = g <= 0

  /** Compiled instruction target offset */
  opaque type Target = UInt
  object Target:
    inline def apply(v: UInt): Target = v
    @targetName("targetFromInt")
    inline def apply(v: Int): Target = UInt(v)
    val OutOfRange: Target = UInt(0)

  extension (t: Target)
    @targetName("targetValue")
    inline def value: UInt = t
    @targetName("targetPlusInt")
    inline def +(offset: Int): Target = t + UInt(offset)

  // ══════════════════════════════════════════════════════════════════════════
  // Register Types with Phantom Width
  // ══════════════════════════════════════════════════════════════════════════

  /**
   * Type-safe register index with phantom width parameter.
   * The width W ensures compile-time checking of 32 vs 64 bit operations.
   */
  opaque type Reg[W <: Width] = Int

  object Reg:
    /** Number of general-purpose registers (r0-r12) */
    val Count: Int = 13

    inline def apply[W <: Width](n: Int): Reg[W] =
      require(n >= 0 && n < Count, s"Register must be 0-${Count-1}, got $n")
      n

    /** Unsafe construction without bounds checking (for internal use) */
    inline def unsafeApply[W <: Width](n: Int): Reg[W] = n

    // Named registers for convenience
    def r0[W <: Width]: Reg[W] = 0
    def r1[W <: Width]: Reg[W] = 1
    def r2[W <: Width]: Reg[W] = 2
    def r3[W <: Width]: Reg[W] = 3
    def r4[W <: Width]: Reg[W] = 4
    def r5[W <: Width]: Reg[W] = 5
    def r6[W <: Width]: Reg[W] = 6
    def r7[W <: Width]: Reg[W] = 7
    def r8[W <: Width]: Reg[W] = 8
    def r9[W <: Width]: Reg[W] = 9
    def r10[W <: Width]: Reg[W] = 10
    def r11[W <: Width]: Reg[W] = 11
    def r12[W <: Width]: Reg[W] = 12

  extension [W <: Width](r: Reg[W])
    inline def index: Int = r
    inline def widen[W2 <: Width]: Reg[W2] = r

  // ══════════════════════════════════════════════════════════════════════════
  // Register or Immediate Value ADT
  // ══════════════════════════════════════════════════════════════════════════

  /**
   * Represents either a register reference or an immediate value.
   * Used for instruction operands that can be either.
   */
  enum RegImm[+W <: Width]:
    case FromReg(r: Reg[W @scala.annotation.unchecked.uncheckedVariance])
    case Imm(value: Long)  // Sign-extended to 64-bit for uniformity

  object RegImm:
    def reg[W <: Width](r: Reg[W]): RegImm[W] = RegImm.FromReg(r)
    def imm[W <: Width](v: Long): RegImm[W] = RegImm.Imm(v)
    @targetName("immFromUInt")
    def imm[W <: Width](v: UInt): RegImm[W] = RegImm.Imm(v.signed.toLong)

  extension [W <: Width](ri: RegImm[W])
    inline def fold[A](onReg: Reg[W] => A, onImm: Long => A): A = ri match
      case RegImm.FromReg(r) => onReg(r)
      case RegImm.Imm(v) => onImm(v)

    inline def isReg: Boolean = ri.isInstanceOf[RegImm.FromReg[?]]
    inline def isImm: Boolean = ri.isInstanceOf[RegImm.Imm[?]]
