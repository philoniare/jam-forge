package io.forge.jam.pvm.memory

import spire.math.{UByte, UShort, UInt, ULong}
import io.forge.jam.pvm.MemoryResult
import io.forge.jam.pvm.MemoryMap
import io.forge.jam.pvm.types.*
import io.forge.jam.pvm.WordOps

/**
 * Memory trait with width-polymorphic load/store operations.
 *
 * Uses the MemoryResult ADT for error handling:
 * - Success: Operation completed successfully
 * - Segfault: Page access violation (with address and page address)
 * - OutOfBounds: Address out of valid range
 */
trait Memory:
  /**
   * The PageMap for per-page access control.
   */
  def pageMap: PageMap

  /**
   * The current heap size.
   */
  def heapSize: UInt

  /**
   * The current heap end address.
   */
  def heapEnd: UInt

  // ============================================================================
  // 8-bit Load Operations
  // ============================================================================

  /**
   * Loads an unsigned 8-bit value from memory.
   */
  def loadU8(address: UInt): MemoryResult[UByte]

  /**
   * Loads a signed 8-bit value from memory (sign-extended to Int).
   */
  def loadI8(address: UInt): MemoryResult[Byte] =
    loadU8(address).map(_.toByte)

  // ============================================================================
  // 16-bit Load Operations
  // ============================================================================

  /**
   * Loads an unsigned 16-bit value from memory (little-endian).
   */
  def loadU16(address: UInt): MemoryResult[UShort]

  /**
   * Loads a signed 16-bit value from memory (little-endian, sign-extended).
   */
  def loadI16(address: UInt): MemoryResult[Short] =
    loadU16(address).map(_.toShort)

  // ============================================================================
  // 32-bit Load Operations
  // ============================================================================

  /**
   * Loads an unsigned 32-bit value from memory (little-endian).
   */
  def loadU32(address: UInt): MemoryResult[UInt]

  /**
   * Loads a signed 32-bit value from memory (little-endian).
   */
  def loadI32(address: UInt): MemoryResult[Int] =
    loadU32(address).map(_.signed)

  // ============================================================================
  // 64-bit Load Operations
  // ============================================================================

  /**
   * Loads an unsigned 64-bit value from memory (little-endian).
   */
  def loadU64(address: UInt): MemoryResult[ULong]

  /**
   * Loads a signed 64-bit value from memory (little-endian).
   */
  def loadI64(address: UInt): MemoryResult[Long] =
    loadU64(address).map(_.signed)

  // ============================================================================
  // Store Operations
  // ============================================================================

  /**
   * Stores an 8-bit value to memory.
   */
  def storeU8(address: UInt, value: UByte): MemoryResult[Unit]

  /**
   * Stores a 16-bit value to memory (little-endian).
   */
  def storeU16(address: UInt, value: UShort): MemoryResult[Unit]

  /**
   * Stores a 32-bit value to memory (little-endian).
   */
  def storeU32(address: UInt, value: UInt): MemoryResult[Unit]

  /**
   * Stores a 64-bit value to memory (little-endian).
   */
  def storeU64(address: UInt, value: ULong): MemoryResult[Unit]

  // ============================================================================
  // Bulk Operations
  // ============================================================================

  /**
   * Reads a slice of memory.
   * Returns None if the access would cause a segfault.
   */
  def getMemorySlice(address: UInt, length: Int): MemoryResult[Array[Byte]]

  /**
   * Writes a slice of memory.
   * Returns false if the access would cause a segfault.
   */
  def setMemorySlice(address: UInt, data: Array[Byte]): MemoryResult[Unit]

  // ============================================================================
  // Heap Operations
  // ============================================================================

  /**
   * Grows the heap by the specified number of bytes (sbrk syscall).
   *
   * @param size Number of bytes to allocate (page-aligned growth)
   * @return Some(previousHeapEnd) on success, None on failure (OOM)
   */
  def sbrk(size: UInt): Option[UInt]

  // ============================================================================
  // Width-Polymorphic Operations via WordOps
  // ============================================================================

  /**
   * Loads a word of the given width from memory.
   */
  def loadWord[W <: Width](address: UInt)(using ops: WordOps[W]): MemoryResult[ops.Word] =
    ops.byteCount match
      case 4 => loadU32(address).map(v => ops.fromULong(ULong(v.toLong)))
      case 8 => loadU64(address).map(v => ops.fromULong(v))
      case _ => MemoryResult.OutOfBounds(address)

  /**
   * Stores a word of the given width to memory.
   */
  def storeWord[W <: Width](address: UInt, value: Long)(using ops: WordOps[W]): MemoryResult[Unit] =
    ops.byteCount match
      case 4 => storeU32(address, UInt(value.toInt))
      case 8 => storeU64(address, ULong(value))
      case _ => MemoryResult.OutOfBounds(address)

object Memory:
  /**
   * Checks if an address range is readable.
   */
  extension (mem: Memory)
    def isReadable(address: UInt, length: Int): Boolean =
      if length == 0 then true
      else mem.pageMap.isReadable(address, length)._1

    def isWritable(address: UInt, length: Int): Boolean =
      if length == 0 then true
      else mem.pageMap.isWritable(address, length)._1
