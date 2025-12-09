package io.forge.jam.pvm.program

import io.forge.jam.pvm.Abi

/**
 * Jump table for PVM programs.
 *
 * The jump table maps addresses to program counter values for indirect jumps.
 * Each entry is a fixed-size little-endian integer (1-4 bytes).
 *
 * @param blob The underlying byte array containing jump addresses
 * @param entrySize Size of each jump table entry in bytes (1-4)
 */
final case class JumpTable(blob: Array[Byte], entrySize: Int) extends Iterable[Int]:

  /**
   * Check if the jump table is empty.
   */
  override def isEmpty: Boolean = len == 0

  /**
   * Get the number of entries in the jump table.
   */
  def len: Int =
    if entrySize == 0 then 0
    else blob.length / entrySize

  /**
   * Get a program counter by memory address.
   *
   * Converts a memory address to a jump table index and returns the target.
   *
   * @param address The memory address (must be aligned)
   * @return Some(target) if valid, None otherwise
   */
  def getByAddress(address: Int): Option[Int] =
    if (address & (Abi.VmCodeAddressAlignment - 1)) != 0 || address == 0 then None
    else getByIndex((address - Abi.VmCodeAddressAlignment) / Abi.VmCodeAddressAlignment)

  /**
   * Get a program counter by index.
   *
   * @param index The entry index
   * @return Some(target) if valid, None otherwise
   */
  def getByIndex(index: Int): Option[Int] =
    if entrySize == 0 then None
    else
      val start = index.toLong * entrySize
      if start > Int.MaxValue then None
      else
        val end = start + entrySize
        if end > Int.MaxValue then None
        else
          val startInt = start.toInt
          val endInt = end.toInt

          if startInt >= blob.length || endInt > blob.length then None
          else
            val value = entrySize match
              case 1 => blob(startInt) & 0xFF
              case 2 =>
                (blob(startInt) & 0xFF) |
                  ((blob(startInt + 1) & 0xFF) << 8)
              case 3 =>
                (blob(startInt) & 0xFF) |
                  ((blob(startInt + 1) & 0xFF) << 8) |
                  ((blob(startInt + 2) & 0xFF) << 16)
              case 4 =>
                (blob(startInt) & 0xFF) |
                  ((blob(startInt + 1) & 0xFF) << 8) |
                  ((blob(startInt + 2) & 0xFF) << 16) |
                  ((blob(startInt + 3) & 0xFF) << 24)
              case _ => throw new IllegalStateException(s"Invalid entry size: $entrySize")
            Some(value)

  /**
   * Iterator over all jump table entries.
   */
  override def iterator: Iterator[Int] = new Iterator[Int]:
    private var index = 0

    override def hasNext: Boolean = index < len

    override def next(): Int =
      val value = getByIndex(index).getOrElse(
        throw new NoSuchElementException("No more elements")
      )
      index += 1
      value

object JumpTable:
  /** Empty jump table */
  val Empty: JumpTable = JumpTable(Array.emptyByteArray, 0)

  /**
   * Create a jump table from a byte array and entry size.
   *
   * @param blob The byte array
   * @param entrySize Entry size in bytes
   * @return JumpTable instance
   */
  def apply(blob: Array[Byte], entrySize: Int): JumpTable =
    new JumpTable(blob, entrySize)
