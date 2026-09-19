package io.forge.jam.pvm

import scala.annotation.targetName
import spire.math.UInt

/**
 * Core type definitions for the JAM PVM using Spire unsigned types.
 *
 * Design Philosophy:
 * - Opaque types for zero-cost type safety
 * - Spire unsigned types for correct unsigned semantics
 */
object types:

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

  object Reg:
    /** Number of general-purpose registers (r0-r12) */
    val Count: Int = 13
