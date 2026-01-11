package io.forge.jam.pvm.engine

import spire.math.UInt
import io.forge.jam.pvm.types.*

// ============================================================================
// Focused Traits (Interface Segregation)
// ============================================================================

/**
 * Register access operations.
 */
trait RegisterOps:
  /** Gets the value of a register */
  def getReg(idx: Int): Long

  /** Sets a 32-bit value in a register (sign-extended to 64-bit) */
  def setReg32(idx: Int, value: UInt): Unit

  /** Sets a 32-bit value from primitive Int (sign-extended to 64-bit) */
  def setReg32Int(idx: Int, value: Int): Unit

  /** Sets a 64-bit value in a register */
  def setReg64(idx: Int, value: Long): Unit

/**
 * Control flow operations.
 */
trait ControlFlowOps:
  /** Advances to the next instruction (returns compiled offset) */
  def advance(): Option[UInt]

  /** Resolves a jump to a program counter */
  def resolveJump(pc: ProgramCounter): Option[UInt]

  /** Resolves a fallthrough to the next instruction */
  def resolveFallthrough(pc: ProgramCounter): Option[UInt]

  /** Handles indirect jump via jump table */
  def jumpIndirect(pc: ProgramCounter, address: UInt): Option[UInt]

  /** Handles indirect jump via jump table */
  def jumpIndirectInt(pc: ProgramCounter, address: Int): Option[UInt]

  /** Branch helper - jumps to target if condition is true, otherwise continues to nextPc */
  def branch(condition: Boolean, pc: ProgramCounter, target: Int, nextPc: ProgramCounter): Option[UInt]

/**
 * Interrupt signaling operations.
 */
trait InterruptOps:
  /** Triggers a panic interrupt */
  def panic(pc: ProgramCounter): Option[UInt]

  /** Triggers an out-of-gas interrupt */
  def outOfGas(pc: ProgramCounter): Option[UInt]

  /** Triggers an ecalli (host call) interrupt */
  def ecalli(pc: ProgramCounter, nextPc: ProgramCounter, hostId: UInt): Option[UInt]

  /** Triggers a finished interrupt (normal termination) */
  def finished(): Option[UInt]

  /** Triggers a segfault interrupt */
  def segfault(pc: ProgramCounter, pageAddress: UInt): Option[UInt]

/**
 * Memory load operations.
 */
trait MemoryLoadOps:
  def loadU8(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt]
  def loadI8(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt]
  def loadU16(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt]
  def loadI16(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt]
  def loadU32(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt]
  def loadI32(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt]
  def loadU64(pc: ProgramCounter, dst: Int, address: UInt): Option[UInt]
  def loadU8Int(pc: ProgramCounter, dst: Int, address: Int): Option[UInt]
  def loadI8Int(pc: ProgramCounter, dst: Int, address: Int): Option[UInt]
  def loadU16Int(pc: ProgramCounter, dst: Int, address: Int): Option[UInt]
  def loadI16Int(pc: ProgramCounter, dst: Int, address: Int): Option[UInt]
  def loadU32Int(pc: ProgramCounter, dst: Int, address: Int): Option[UInt]
  def loadI32Int(pc: ProgramCounter, dst: Int, address: Int): Option[UInt]
  def loadU64Int(pc: ProgramCounter, dst: Int, address: Int): Option[UInt]

/**
 * Memory store operations.
 */
trait MemoryStoreOps:
  def storeU8(pc: ProgramCounter, src: Int, address: UInt): Option[UInt]
  def storeU16(pc: ProgramCounter, src: Int, address: UInt): Option[UInt]
  def storeU32(pc: ProgramCounter, src: Int, address: UInt): Option[UInt]
  def storeU64(pc: ProgramCounter, src: Int, address: UInt): Option[UInt]

  def storeImmU8(pc: ProgramCounter, address: UInt, value: Byte): Option[UInt]
  def storeImmU16(pc: ProgramCounter, address: UInt, value: Short): Option[UInt]
  def storeImmU32(pc: ProgramCounter, address: UInt, value: Int): Option[UInt]
  def storeImmU64(pc: ProgramCounter, address: UInt, value: Long): Option[UInt]

  def storeU8Int(pc: ProgramCounter, src: Int, address: Int): Option[UInt]
  def storeU16Int(pc: ProgramCounter, src: Int, address: Int): Option[UInt]
  def storeU32Int(pc: ProgramCounter, src: Int, address: Int): Option[UInt]
  def storeU64Int(pc: ProgramCounter, src: Int, address: Int): Option[UInt]

  def storeImmU8Int(pc: ProgramCounter, address: Int, value: Byte): Option[UInt]
  def storeImmU16Int(pc: ProgramCounter, address: Int, value: Short): Option[UInt]
  def storeImmU32Int(pc: ProgramCounter, address: Int, value: Int): Option[UInt]
  def storeImmU64Int(pc: ProgramCounter, address: Int, value: Long): Option[UInt]

/**
 * Combined memory operations.
 */
trait MemoryOps extends MemoryLoadOps, MemoryStoreOps

/**
 * Heap operations (sbrk syscall).
 */
trait HeapOps:
  /** Extends the heap (sbrk syscall) */
  def sbrk(dst: Int, size: UInt): Option[UInt]

// ============================================================================
// Combined ExecutionContext
// ============================================================================

/**
 * Full execution context combining all operation traits.
 * This is the main trait that instruction executors use.
 */
trait ExecutionContext
    extends RegisterOps, ControlFlowOps, InterruptOps, MemoryOps, HeapOps:

  // ============================================================================
  // Helper Operations (with default implementations)
  // ============================================================================

  /** Three-register 32-bit operation */
  inline def op3_32(d: Int, s1: Int, s2: Int)(f: (Int, Int) => Int): Option[UInt] =
    val v1 = getReg(s1).toInt
    val v2 = getReg(s2).toInt
    setReg32Int(d, f(v1, v2))
    advance()

  /** Three-register 64-bit operation */
  inline def op3_64(d: Int, s1: Int, s2: Int)(f: (Long, Long) => Long): Option[UInt] =
    val v1 = getReg(s1)
    val v2 = getReg(s2)
    setReg64(d, f(v1, v2))
    advance()

  /** Two-register + immediate 32-bit operation */
  inline def op2Imm32(d: Int, src: Int, imm: Int)(f: (Int, Int) => Int): Option[UInt] =
    val v = getReg(src).toInt
    setReg32Int(d, f(v, imm))
    advance()

  /** Two-register + immediate 64-bit operation */
  inline def op2Imm64(d: Int, src: Int, imm: Long)(f: (Long, Long) => Long): Option[UInt] =
    val v = getReg(src)
    setReg64(d, f(v, imm))
    advance()

  // ============================================================================
  // Specialized Operations
  // ============================================================================

  /** add 64-bit: d = s1 + s2 */
  inline def add64(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg64(d, getReg(s1) + getReg(s2))
    advance()

  /** sub 64-bit: d = s1 - s2 */
  inline def sub64(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg64(d, getReg(s1) - getReg(s2))
    advance()

  /** mul 64-bit: d = s1 * s2 */
  inline def mul64(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg64(d, getReg(s1) * getReg(s2))
    advance()

  /** and 64-bit: d = s1 & s2 */
  inline def and64(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg64(d, getReg(s1) & getReg(s2))
    advance()

  /** or 64-bit: d = s1 | s2 */
  inline def or64(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg64(d, getReg(s1) | getReg(s2))
    advance()

  /** xor 64-bit: d = s1 ^ s2 */
  inline def xor64(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg64(d, getReg(s1) ^ getReg(s2))
    advance()

  /** add 32-bit: d = (s1 + s2) sign-extended */
  inline def add32(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg32Int(d, getReg(s1).toInt + getReg(s2).toInt)
    advance()

  /** sub 32-bit: d = (s1 - s2) sign-extended */
  inline def sub32(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg32Int(d, getReg(s1).toInt - getReg(s2).toInt)
    advance()

  /** mul 32-bit: d = (s1 * s2) sign-extended */
  inline def mul32(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg32Int(d, getReg(s1).toInt * getReg(s2).toInt)
    advance()

  /** shift left 32-bit: d = (s1 << (s2 & 31)) sign-extended */
  inline def shl32(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg32Int(d, getReg(s1).toInt << (getReg(s2).toInt & 31))
    advance()

  /** logical shift right 32-bit: d = (s1 >>> (s2 & 31)) sign-extended */
  inline def shr32(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg32Int(d, getReg(s1).toInt >>> (getReg(s2).toInt & 31))
    advance()

  /** arithmetic shift right 32-bit: d = (s1 >> (s2 & 31)) sign-extended */
  inline def sar32(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg32Int(d, getReg(s1).toInt >> (getReg(s2).toInt & 31))
    advance()

  /** shift left 64-bit: d = s1 << (s2 & 63) */
  inline def shl64(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg64(d, getReg(s1) << (getReg(s2).toInt & 63))
    advance()

  /** logical shift right 64-bit: d = s1 >>> (s2 & 63) */
  inline def shr64(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg64(d, getReg(s1) >>> (getReg(s2).toInt & 63))
    advance()

  /** arithmetic shift right 64-bit: d = s1 >> (s2 & 63) */
  inline def sar64(d: Int, s1: Int, s2: Int): Option[UInt] =
    setReg64(d, getReg(s1) >> (getReg(s2).toInt & 63))
    advance()

  // Immediate versions
  /** add immediate 64-bit: d = src + imm */
  inline def addImm64(d: Int, src: Int, imm: Long): Option[UInt] =
    setReg64(d, getReg(src) + imm)
    advance()

  /** and immediate: d = src & imm */
  inline def andImm(d: Int, src: Int, imm: Long): Option[UInt] =
    setReg64(d, getReg(src) & imm)
    advance()

  /** or immediate: d = src | imm */
  inline def orImm(d: Int, src: Int, imm: Long): Option[UInt] =
    setReg64(d, getReg(src) | imm)
    advance()

  /** xor immediate: d = src ^ imm */
  inline def xorImm(d: Int, src: Int, imm: Long): Option[UInt] =
    setReg64(d, getReg(src) ^ imm)
    advance()
