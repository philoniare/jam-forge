package io.forge.jam.pvm.program

import io.forge.jam.pvm.Opcode

/**
 * Program parsing utilities and static operations.
 *
 * Provides methods for:
 * - Bitmask parsing for instruction boundary detection
 * - Argument reading from instruction chunks
 * - Jump target validation
 * - Basic block boundary detection
 */
object Program:

  /** Maximum skip value in bitmask (24 bytes) */
  val BitmaskMax: Int = 24

  /** Invalid instruction index marker */
  val InvalidInstructionIndex: Int = 256

  // ============================================================================
  // Bitmask Operations
  // ============================================================================

  /**
   * Check if the bit at a given offset is set in the bitmask.
   *
   * @param bitmask The instruction boundary bitmask
   * @param codeLen Total code length
   * @param offset The offset to check
   * @return True if bit is set, false otherwise
   */
  def getBitForOffset(bitmask: Array[Byte], codeLen: Int, offset: Int): Boolean =
    val byteIndex = offset >> 3
    if byteIndex >= bitmask.length || offset > codeLen then false
    else
      val shift = offset & 7
      ((bitmask(byteIndex) >> shift) & 1) == 1

  /**
   * Get the skip value of the previous instruction.
   *
   * @param bitmask The instruction boundary bitmask
   * @param offset Current instruction offset
   * @return Some(skip) if found, None if too far back
   */
  def getPreviousInstructionSkip(bitmask: Array[Byte], offset: Int): Option[Int] =
    val shift = offset & 7
    var mask = (bitmask(offset >> 3) & 0xFF) << 24

    // Build up the mask from previous bytes
    if offset >= 8 then
      mask = mask | ((bitmask((offset >> 3) - 1) & 0xFF) << 16)
    if offset >= 16 then
      mask = mask | ((bitmask((offset >> 3) - 2) & 0xFF) << 8)
    if offset >= 24 then
      mask = mask | (bitmask((offset >> 3) - 3) & 0xFF)

    mask = mask << (8 - shift)
    mask = mask >>> 1

    val skip = java.lang.Integer.numberOfLeadingZeros(mask) - 1

    if skip > BitmaskMax then None else Some(skip)

  /**
   * Find the next instruction offset (unbounded search).
   *
   * @param bitmask The instruction boundary bitmask
   * @param codeLen Total code length
   * @param offsetStart Starting offset
   * @return Next instruction offset
   */
  def findNextOffsetUnbounded(bitmask: Array[Byte], codeLen: Int, offsetStart: Int): Int =
    var offset = offsetStart
    var done = false
    while !done do
      val byteIndex = offset >> 3
      if byteIndex >= bitmask.length then
        done = true
      else
        val byte = bitmask(byteIndex) & 0xFF
        val shift = offset & 7
        val mask = byte >> shift

        if mask == 0 then
          offset += 8 - shift
        else
          offset += java.lang.Integer.numberOfTrailingZeros(mask)
          done = true

    math.min(codeLen, offset)

  // ============================================================================
  // Jump Target Validation
  // ============================================================================

  /**
   * Check if an offset is a valid jump target.
   *
   * A jump target is valid if:
   * 1. The bit is set in the bitmask (instruction boundary)
   * 2. Offset 0 is always valid
   * 3. The previous instruction starts a new basic block
   *
   * @param code The code section
   * @param bitmask The instruction boundary bitmask
   * @param offset The target offset to validate
   * @return True if valid jump target
   */
  def isJumpTargetValid(code: Array[Byte], bitmask: Array[Byte], offset: Int): Boolean =
    if !getBitForOffset(bitmask, code.length, offset) then false
    else if offset == 0 then true
    else
      getPreviousInstructionSkip(bitmask, offset) match
        case None => false
        case Some(skip) =>
          val previousOffset = offset - skip - 1
          if previousOffset < 0 || previousOffset >= code.length then false
          else
            Opcode.fromByte(code(previousOffset) & 0xFF) match
              case None => false
              case Some(opcode) => opcode.startsNewBasicBlock

  /**
   * Find the start of the basic block containing the given offset.
   *
   * @param code The code section
   * @param bitmask The instruction boundary bitmask
   * @param initialOffset The offset within the block
   * @return Some(blockStart) if found, None if invalid
   */
  def findStartOfBasicBlock(code: Array[Byte], bitmask: Array[Byte], initialOffset: Int): Option[Int] =
    if !getBitForOffset(bitmask, code.length, initialOffset) then return None

    if initialOffset == 0 then return Some(0)

    var offset = initialOffset
    var done = false
    var result: Option[Int] = None

    while !done do
      getPreviousInstructionSkip(bitmask, offset) match
        case None =>
          done = true
          result = None
        case Some(skip) =>
          val previousOffset = offset - skip - 1
          val opcode = if previousOffset >= 0 && previousOffset < code.length then
            Opcode.fromByte(code(previousOffset) & 0xFF).getOrElse(Opcode.Panic)
          else
            Opcode.Panic

          if opcode.startsNewBasicBlock then
            done = true
            result = Some(offset)
          else
            offset = previousOffset
            if offset == 0 then
              done = true
              result = Some(0)

    result
