package io.forge.jam.pvm.program

import io.forge.jam.pvm.Abi

/**
 * Parsed PVM program blob.
 *
 * Contains all the components extracted from a PVM binary:
 * - Code section
 * - Bitmask for instruction boundaries
 * - Jump table
 * - Read-only and read-write data
 * - Program mode (32-bit or 64-bit)
 *
 * @param code The executable code section
 * @param bitmask Instruction boundary bitmask
 * @param jumpTable The jump table for indirect jumps
 * @param is64Bit True if this is a 64-bit program
 * @param roData Read-only data section
 * @param rwData Read-write data section
 * @param stackSize Initial stack size
 */
final case class ProgramBlob(
  code: Array[Byte],
  bitmask: Array[Byte],
  jumpTable: JumpTable,
  is64Bit: Boolean,
  roData: Array[Byte],
  rwData: Array[Byte],
  stackSize: Int,
  heapPages: Int = 0,
  originalRwDataLen: Int = -1
):
  /** Check if code size is within limits */
  def isCodeSizeValid: Boolean =
    code.length <= Abi.VmMaximumCodeSize.signed

  /** Check if jump table size is within limits */
  def isJumpTableSizeValid: Boolean =
    jumpTable.len <= Abi.VmMaximumJumpTableEntries.signed

  /** Total code length */
  def codeLength: Int = code.length

object ProgramBlob:

  /**
   * Creates a ProgramBlob from raw code+jumptable bytes (JAM format).
   *
   * - Varint: jump table entry count
   * - Byte: jump table entry size
   * - Varint: code length
   * - Jump table bytes
   * - Code bytes
   * - Bitmask bytes
   */
  def fromCodeAndJumpTable(
    data: Array[Byte],
    roData: Array[Byte] = Array.empty,
    rwData: Array[Byte] = Array.empty,
    stackSize: Int = 0,
    is64Bit: Boolean = true,
    heapPages: Int = 0,
    originalRwDataLen: Int = -1
  ): Option[ProgramBlob] =
    if data.isEmpty then return None

    var offset = 0

    // Read jump table entry count (varint), |j| <= Z_J. A negative Long is a
    // varint of 2^63 or more, so it is over the limit too.
    val jumpTableEntryCount = readJamFormatVarint(data, offset) match
      case Some((count, newOffset)) if count >= 0 && count <= Abi.VmMaximumJumpTableEntries.signed =>
        offset = newOffset
        count.toInt
      case _ => return None

    if offset >= data.length then return None

    // Read jump table entry size (single byte)
    val jumpTableEntrySize = data(offset) & 0xFF
    offset += 1

    // Read code length (varint), |c| <= Z_C
    val codeLength = readJamFormatVarint(data, offset) match
      case Some((len, newOffset)) if len >= 0 && len <= Abi.VmMaximumCodeSize.signed =>
        offset = newOffset
        len.toInt
      case _ => return None

    // Calculate jump table length (up to Z_J * 255 octets, which overflows an Int)
    val jumpTableOctets = jumpTableEntryCount.toLong * jumpTableEntrySize

    // Calculate bitmask length
    val bitmaskLength = (codeLength + 7) / 8

    // Verify we have enough data
    val expectedTotal = offset + jumpTableOctets + codeLength + bitmaskLength
    if data.length < expectedTotal then return None
    val jumpTableLength = jumpTableOctets.toInt // bounded by data.length above

    // JAM format: jump table comes BEFORE code
    // Extract jump table
    val jumpTableData = new Array[Byte](jumpTableLength)
    if jumpTableLength > 0 then
      System.arraycopy(data, offset, jumpTableData, 0, jumpTableLength)
    offset += jumpTableLength

    // Extract code
    val code = new Array[Byte](codeLength)
    System.arraycopy(data, offset, code, 0, codeLength)
    offset += codeLength

    // Extract bitmask
    val bitmask = new Array[Byte](bitmaskLength)
    System.arraycopy(data, offset, bitmask, 0, bitmaskLength)

    val jumpTable = JumpTable(jumpTableData, jumpTableEntrySize)

    Some(ProgramBlob(
      code = code,
      bitmask = bitmask,
      jumpTable = jumpTable,
      is64Bit = is64Bit,
      roData = roData,
      rwData = rwData,
      stackSize = stackSize,
      heapPages = heapPages,
      originalRwDataLen = originalRwDataLen
    ))

  /**
   * Read a varint from data at offset
   * Returns (value, newOffset) or None on failure.
   */
  private def readJamFormatVarint(data: Array[Byte], offset: Int): Option[(Long, Int)] =
    if offset >= data.length then return None

    val firstByte = data(offset) & 0xFF

    // Count leading 1 bits to determine length
    var byteLength = 0
    var temp = firstByte
    while (temp & 0x80) != 0 && byteLength < 8 do
      byteLength += 1
      temp = (temp << 1) & 0xFF

    if offset + 1 + byteLength > data.length then return None

    // Read additional bytes (little-endian)
    var result: Long = 0L
    for i <- 0 until byteLength do
      result = result | ((data(offset + 1 + i).toLong & 0xFF) << (8 * i))

    // Mask for top bits from first byte
    val mask = (1 << (8 - byteLength)) - 1
    val topBits = firstByte & mask

    val value = result + (topBits.toLong << (8 * byteLength))
    Some((value, offset + 1 + byteLength))
