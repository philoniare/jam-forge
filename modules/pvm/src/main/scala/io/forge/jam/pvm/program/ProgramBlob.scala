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
  stackSize: Int
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
