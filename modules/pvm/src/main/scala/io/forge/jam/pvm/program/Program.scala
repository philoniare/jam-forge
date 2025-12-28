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
   * Parse bitmask using fast path (requires 4+ bytes available).
   *
   * @param bitmask The instruction boundary bitmask
   * @param offset Current instruction offset
   * @return Some(skip) if successful, None if not enough bytes
   */
  def parseBitmaskFast(bitmask: Array[Byte], offset: Int): Option[Int] =
    val currentOffset = offset + 1
    val byteIndex = currentOffset >> 3

    // Ensure we have enough bytes to read
    if byteIndex + 4 > bitmask.length then return None

    val shift = currentOffset & 7

    // Read 4 bytes and convert to UInt
    val value = (bitmask(byteIndex) & 0xFF) |
                ((bitmask(byteIndex + 1) & 0xFF) << 8) |
                ((bitmask(byteIndex + 2) & 0xFF) << 16) |
                ((bitmask(byteIndex + 3) & 0xFF) << 24)

    // Create mask with trailing 1 at BitmaskMax position
    val mask = (value >>> shift) | (1 << BitmaskMax)

    Some(java.lang.Integer.numberOfTrailingZeros(mask))

  /**
   * Parse bitmask using slow path (handles edge cases near end of code).
   *
   * @param bitmask The instruction boundary bitmask
   * @param codeLength Total code length
   * @param offset Current instruction offset
   * @return (skip, isNextInstructionInvalid)
   */
  def parseBitmaskSlow(bitmask: Array[Byte], codeLength: Int, offset: Int): (Int, Boolean) =
    var currentOffset = offset + 1
    var isNextInstructionInvalid = true
    val origin = currentOffset

    var done = false
    while !done do
      val byteIndex = currentOffset >> 3
      if byteIndex >= bitmask.length then
        done = true
      else
        val byte = bitmask(byteIndex) & 0xFF
        val shift = currentOffset & 7
        val mask = byte >> shift

        if mask == 0 then
          currentOffset += 8 - shift
          if (currentOffset - origin) >= BitmaskMax then
            done = true
        else
          currentOffset += java.lang.Integer.numberOfTrailingZeros(mask)
          isNextInstructionInvalid = currentOffset >= codeLength ||
                                     (currentOffset - origin) > BitmaskMax
          done = true

    val finalOffset = math.min(currentOffset, codeLength)
    val skip = math.min(finalOffset - origin, BitmaskMax)

    (skip, isNextInstructionInvalid)

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

  // ============================================================================
  // Argument Reading Functions
  // ============================================================================

  /**
   * Sign extend a value at a specific bit position.
   *
   * @param value The value to sign extend
   * @param bitsToCut Number of bits to cut (shift amount)
   * @return Sign-extended value
   */
  private def signExtendAt(value: Long, bitsToCut: Int): Long =
    val shifted = (value << bitsToCut).toInt
    (shifted >> bitsToCut).toLong & 0xffffffffL

  /**
   * Read an immediate argument from chunk.
   *
   * @param chunk The data chunk
   * @param skip Skip value from bitmask
   * @return The immediate value
   */
  def readArgsImm(chunk: U128, skip: Int): Long =
    Varint.readSimpleVarint(chunk.toUInt.toLong & 0xffffffffL, skip)

  /**
   * Read an offset argument from chunk (adds instruction offset).
   *
   * @param chunk The data chunk
   * @param instructionOffset Current instruction offset
   * @param skip Skip value from bitmask
   * @return The absolute offset
   */
  def readArgsOffset(chunk: U128, instructionOffset: Int, skip: Int): Long =
    (instructionOffset + readArgsImm(chunk, skip).toInt).toLong & 0xffffffffL

  /**
   * Read two immediate arguments from chunk.
   *
   * @param chunk The data chunk
   * @param skip Skip value from bitmask
   * @return (imm1, imm2)
   */
  def readArgsImm2(chunk: U128, skip: Int): (Long, Long) =
    val (imm1Bits, imm1Skip, imm2Bits) = LookupTable.Table1.get(skip, chunk.toUInt.toLong)
    val shiftedChunk = chunk >> 8
    val chunk64 = shiftedChunk.low
    val imm1 = signExtendAt(chunk64 & 0xffffffffL, imm1Bits)
    val finalChunk = chunk64 >>> imm1Skip
    val imm2 = signExtendAt(finalChunk & 0xffffffffL, imm2Bits)
    (imm1, imm2)

  /**
   * Read register and immediate arguments from chunk.
   *
   * @param chunk The data chunk
   * @param skip Skip value from bitmask
   * @return (register, immediate)
   */
  def readArgsRegImm(chunk: U128, skip: Int): (RawReg, Long) =
    val chunk64 = chunk.low
    val reg = RawReg(chunk64.toInt)
    val shiftedChunk = chunk64 >>> 8
    val (_, _, immBits) = LookupTable.Table1.get(skip, 0)
    val imm = signExtendAt(shiftedChunk & 0xffffffffL, immBits)
    (reg, imm)

  /**
   * Read register and two immediate arguments from chunk.
   *
   * @param chunk The data chunk
   * @param skip Skip value from bitmask
   * @return (register, imm1, imm2)
   */
  def readArgsRegImm2(chunk: U128, skip: Int): (RawReg, Long, Long) =
    val reg = RawReg(chunk.toUInt)
    val (imm1Bits, imm1Skip, imm2Bits) = LookupTable.Table1.get(skip, (chunk.toUInt >>> 4).toLong)
    val shiftedChunk = chunk >> 8
    val chunk64 = shiftedChunk.low
    val imm1 = signExtendAt(chunk64 & 0xffffffffL, imm1Bits)
    val finalChunk = chunk64 >>> imm1Skip
    val imm2 = signExtendAt(finalChunk & 0xffffffffL, imm2Bits)
    (reg, imm1, imm2)

  /**
   * Read register, immediate, and offset arguments from chunk.
   *
   * @param chunk The data chunk
   * @param instructionOffset Current instruction offset
   * @param skip Skip value from bitmask
   * @return (register, imm1, absoluteOffset)
   */
  def readArgsRegImmOffset(chunk: U128, instructionOffset: Int, skip: Int): (RawReg, Long, Long) =
    val (reg, imm1, imm2) = readArgsRegImm2(chunk, skip)
    (reg, imm1, (instructionOffset + imm2.toInt).toLong & 0xffffffffL)

  /**
   * Read two registers and two immediate arguments from chunk.
   *
   * @param chunk The data chunk
   * @param skip Skip value from bitmask
   * @return (reg1, reg2, imm1, imm2)
   */
  def readArgsRegs2Imm2(chunk: U128, skip: Int): (RawReg, RawReg, Long, Long) =
    val value = chunk.toUInt
    val reg1 = RawReg(value)
    val reg2 = RawReg(value >>> 4)
    val imm1Aux = (value >>> 8).toLong

    val (imm1Bits, imm1Skip, imm2Bits) = LookupTable.Table2.get(skip, imm1Aux)
    val shiftedChunk = chunk >> 16
    val chunk64 = shiftedChunk.low
    val imm1 = signExtendAt(chunk64 & 0xffffffffL, imm1Bits)
    val finalChunk = chunk64 >>> imm1Skip
    val imm2 = signExtendAt(finalChunk & 0xffffffffL, imm2Bits)
    (reg1, reg2, imm1, imm2)

  /**
   * Read two registers and immediate from chunk.
   *
   * @param chunk The data chunk
   * @param skip Skip value from bitmask
   * @return (reg1, reg2, immediate)
   */
  def readArgsRegs2Imm(chunk: U128, skip: Int): (RawReg, RawReg, Long) =
    val chunk64 = chunk.low
    val value = chunk64.toInt
    val reg1 = RawReg(value)
    val reg2 = RawReg(value >>> 4)
    val shiftedChunk = chunk64 >>> 8
    val (_, _, immBits) = LookupTable.Table1.get(skip, 0)
    val imm = signExtendAt(shiftedChunk & 0xffffffffL, immBits)
    (reg1, reg2, imm)

  /**
   * Read two registers and offset from chunk.
   *
   * @param chunk The data chunk
   * @param instructionOffset Current instruction offset
   * @param skip Skip value from bitmask
   * @return (reg1, reg2, absoluteOffset)
   */
  def readArgsRegs2Offset(chunk: U128, instructionOffset: Int, skip: Int): (RawReg, RawReg, Long) =
    val (reg1, reg2, imm) = readArgsRegs2Imm(chunk, skip)
    (reg1, reg2, (instructionOffset + imm.toInt).toLong & 0xffffffffL)

  /**
   * Read three registers from chunk.
   *
   * @param chunk The data chunk
   * @return (reg1, reg2, reg3)
   */
  def readArgsRegs3(chunk: U128): (RawReg, RawReg, RawReg) =
    val value = chunk.toUInt
    val reg2 = RawReg(value)
    val reg3 = RawReg(value >>> 4)
    val reg1 = RawReg(value >>> 8)
    (reg1, reg2, reg3)

  /**
   * Read two registers from chunk.
   *
   * @param chunk The data chunk
   * @return (reg1, reg2)
   */
  def readArgsRegs2(chunk: U128): (RawReg, RawReg) =
    val value = chunk.toUInt
    (RawReg(value), RawReg(value >>> 4))

  /**
   * Read register and 64-bit immediate from chunk.
   *
   * @param chunk The data chunk
   * @param skip Skip value (unused for 64-bit imm)
   * @return (register, immediate64)
   */
  def readArgsRegImm64(chunk: U128): (RawReg, Long) =
    val reg = RawReg(chunk.toUInt)
    val imm = (chunk >> 8).low
    (reg, imm)
