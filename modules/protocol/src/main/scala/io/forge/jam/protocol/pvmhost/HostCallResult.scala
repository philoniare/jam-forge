package io.forge.jam.protocol.pvmhost

import spire.math.ULong

/**
 * Host call result constants.
 */
object HostCallResult:

  /** Success - operation completed successfully */
  val OK: ULong = ULong(0)

  /** Item does not exist (2^64 - 1) */
  val NONE: ULong = ULong.MaxValue

  /** Name unknown (2^64 - 2) */
  val WHAT: ULong = ULong.MaxValue - ULong(1)

  /** Memory index not accessible / Out of bounds (2^64 - 3) */
  val OOB: ULong = ULong.MaxValue - ULong(2)

  /** Index unknown / Who? (2^64 - 4) */
  val WHO: ULong = ULong.MaxValue - ULong(3)

  /** Storage full (2^64 - 5) */
  val FULL: ULong = ULong.MaxValue - ULong(4)

  /** Core index unknown (2^64 - 6) */
  val CORE: ULong = ULong.MaxValue - ULong(5)

  /** Insufficient funds / Cash (2^64 - 7) */
  val CASH: ULong = ULong.MaxValue - ULong(6)

  /** Gas limit too low (2^64 - 8) */
  val LOW: ULong = ULong.MaxValue - ULong(7)

  /** Invalid operation / Huh? (2^64 - 9) */
  val HUH: ULong = ULong.MaxValue - ULong(8)

  // ===========================================================================
  // Inner PVM Invocation Result Codes (for INVOKE host call)
  // ===========================================================================

  /** Inner PVM halted normally */
  val HALT: ULong = ULong(0)

  /** Inner PVM panicked */
  val PANIC: ULong = ULong(1)

  /** Inner PVM page fault */
  val FAULT: ULong = ULong(2)

  /** Inner PVM host call requested */
  val HOST: ULong = ULong(3)

  /** Inner PVM ran out of gas */
  val OOG: ULong = ULong(4)
