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

    // Read jump table entry count (varint)
    val jumpTableEntryCount = readJamFormatVarint(data, offset) match
      case Some((count, newOffset)) =>
        offset = newOffset
        count.toInt
      case None => return None

    if offset >= data.length then return None

    // Read jump table entry size (single byte)
    val jumpTableEntrySize = data(offset) & 0xFF
    offset += 1

    // Read code length (varint)
    val codeLength = readJamFormatVarint(data, offset) match
      case Some((len, newOffset)) =>
        offset = newOffset
        len.toInt
      case None => return None

    // Calculate jump table length
    val jumpTableLength = jumpTableEntryCount * jumpTableEntrySize

    // Calculate bitmask length
    val bitmaskLength = (codeLength + 7) / 8

    // Verify we have enough data
    val expectedTotal = offset + jumpTableLength + codeLength + bitmaskLength
    if data.length < expectedTotal then return None

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

  /**
   * Simple reader for parsing PVM blobs.
   * Tracks position and uses Varint utilities for varint decoding.
   */
  private class BlobReader(data: Array[Byte]):
    var position: Int = 0

    def remaining: Int = data.length - position

    def readByte(): Option[Int] =
      if position >= data.length then None
      else
        val b = data(position) & 0xFF
        position += 1
        Some(b)

    def readVarintValue(): Option[Int] =
      if position >= data.length then None
      else
        val firstByte = data(position) & 0xFF
        position += 1

        val remainingBytes = data.drop(position)
        Varint.readVarint(remainingBytes, firstByte) match
          case Some((length, value)) =>
            position += length
            Some(value.toInt)
          case None => None

    def readSlice(length: Int): Option[Array[Byte]] =
      if position + length > data.length then None
      else
        val slice = new Array[Byte](length)
        System.arraycopy(data, position, slice, 0, length)
        position += length
        Some(slice)

  /**
   * Parse a PVM blob from raw bytes.
   *
   * The blob format is:
   * - Version byte (0 = 32-bit, 1 = 64-bit)
   * - Varint: ro_data_size
   * - Varint: rw_data_size
   * - Varint: stack_size
   * - Varint: code_size
   * - Varint: jump_table_entry_count
   * - Varint: jump_table_entry_size (if entry_count > 0)
   * - Code section (code_size bytes)
   * - Jump table (entry_count * entry_size bytes)
   * - Bitmask ((code_size + 7) / 8 bytes)
   * - RO data (ro_data_size bytes)
   * - RW data (rw_data_size bytes)
   *
   * @param data The raw blob bytes
   * @return Some(ProgramBlob) on success, None on failure
   */
  def parse(data: Array[Byte]): Option[ProgramBlob] =
    if data.isEmpty then None
    else
      val reader = new BlobReader(data)

      for
        version <- reader.readByte()
        if version <= 1
        is64Bit = version == 1

        roDataSize <- reader.readVarintValue()
        rwDataSize <- reader.readVarintValue()
        stackSize <- reader.readVarintValue()
        codeSize <- reader.readVarintValue()
        if codeSize <= Abi.VmMaximumCodeSize.signed

        jumpTableEntryCount <- reader.readVarintValue()
        jumpTableEntrySize <- if jumpTableEntryCount > 0 then reader.readVarintValue() else Some(0)
        if jumpTableEntryCount <= Abi.VmMaximumJumpTableEntries.signed

        code <- reader.readSlice(codeSize)
        jumpTableSize = jumpTableEntryCount * jumpTableEntrySize
        jumpTableData <- reader.readSlice(jumpTableSize)
        jumpTable = JumpTable(jumpTableData, jumpTableEntrySize)

        bitmaskSize = (codeSize + 7) / 8
        bitmask <- reader.readSlice(bitmaskSize)

        roData <- reader.readSlice(roDataSize)
        rwData <- reader.readSlice(rwDataSize)
      yield ProgramBlob(
        code = code,
        bitmask = bitmask,
        jumpTable = jumpTable,
        is64Bit = is64Bit,
        roData = roData,
        rwData = rwData,
        stackSize = stackSize
      )
