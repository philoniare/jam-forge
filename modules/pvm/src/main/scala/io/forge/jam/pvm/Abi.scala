package io.forge.jam.pvm

import spire.math.{UInt, ULong}

/**
 * PVM ABI constants matching the Gray Paper specification.
 */
object Abi:
  /** Total VM address space size: 2^32 */
  val AddressSpaceSize: ULong = ULong(0x100000000L)

  /** Minimum page size: 4 KB (2^12) */
  val VmMinPageSize: UInt = UInt(0x1000)

  /** Maximum page size: 64 KB (2^16) */
  val VmMaxPageSize: UInt = UInt(0x10000)

  /** Address to return to host (exit VM) */
  val VmAddrReturnToHost: UInt = UInt(0xffff0000)

  /** Maximum code size: 32 MB */
  val VmMaximumCodeSize: UInt = UInt(32 * 1024 * 1024)

  /** Maximum jump table entries: 16M */
  val VmMaximumJumpTableEntries: UInt = UInt(16 * 1024 * 1024)

  /** Maximum import count */
  val VmMaximumImportCount: UInt = UInt(1024)

  /** Code address alignment factor */
  val VmCodeAddressAlignment: Int = 2

  /** Address space bottom (after null page) */
  val VmAddressSpaceBottom: UInt = VmMaxPageSize

  /** Address space top (before guard page) */
  val VmAddressSpaceTop: UInt = UInt((AddressSpaceSize - ULong(VmMaxPageSize.toLong)).toInt)

/**
 * PVM standard constants from Gray Paper.
 */
object PvmConstants:
  /** Number of general-purpose registers (RA through R12) */
  val NumGeneralRegisters: Int = 13

  /** Maximum instruction size in bytes */
  val MaxInstructionSize: Int = 24

  /** ZP = 2^12: Page size (4 KB) */
  val ZP: UInt = UInt(4096)

  /** ZZ = 2^16: Standard zone size (64 KB) */
  val ZZ: UInt = UInt(65536)

  /** ZI = 2^24: Input data size (16 MB) */
  val ZI: UInt = UInt(16777216)

  /**
   * Maximum page count
   */
  val MaxPageCount: Int = 1 << 20 // 1,048,576 pages

  /** ZA = 2: Dynamic address alignment factor */
  val ZA: Int = 2

  /** Minimum valid address - below triggers panic */
  val MinValidAddress: UInt = UInt(0x10000)

  /** Standard register 0 initial value (return address) */
  val Register0Value: UInt = UInt(0xffff0000)

  /** Standard stack base address */
  val StackBaseAddress: UInt =
    UInt((0x100000000L - 2L * ZZ.toLong - ZI.toLong).toInt)

  /** Standard input/argument start address */
  val InputStartAddress: UInt = StackBaseAddress + ZZ

  /** GP standard stack base */
  val GpStackBase: UInt = UInt(0xfefe0000)

  /** GP standard stack size */
  val GpStackSize: UInt = UInt(0x10000)

  /** GP standard stack low address */
  val GpStackLow: UInt = GpStackBase - GpStackSize

/**
 * Memory map configuration for PVM instance.
 */
final case class MemoryMap(
  pageSize: UInt,
  roDataSize: UInt,
  rwDataAddress: UInt,
  rwDataSize: UInt,
  stackAddressHigh: UInt,
  stackSize: UInt,
  auxDataAddress: UInt,
  auxDataSize: UInt,
  heapBase: UInt,
  maxHeapSize: UInt
):
  import Abi.VmAddressSpaceBottom

  /** Start address of read-only data */
  def roDataAddress: UInt = VmAddressSpaceBottom

  /** Range of read-only data region */
  def roDataRange: Range = roDataAddress.signed until (roDataAddress + roDataSize).signed

  /** Range of read-write data region */
  def rwDataRange: Range = rwDataAddress.signed until (rwDataAddress + rwDataSize).signed

  /** Low address of stack */
  def stackAddressLow: UInt = stackAddressHigh - stackSize

  /** Range of stack region */
  def stackRange: Range = stackAddressLow.signed until stackAddressHigh.signed

  /** Range of auxiliary data region */
  def auxDataRange: Range = auxDataAddress.signed until (auxDataAddress + auxDataSize).signed

object MemoryMap:
  import Abi.*

  /**
   * Builder for MemoryMap with validation.
   */
  final class Builder private[MemoryMap] (pageSize: UInt):
    private var roDataSize: UInt = UInt(0)
    private var rwDataSize: UInt = UInt(0)
    private var stackSize: UInt = UInt(0)
    private var auxDataSize: UInt = UInt(0)

    def withRoDataSize(size: UInt): Builder =
      roDataSize = size; this
    def withRwDataSize(size: UInt): Builder =
      rwDataSize = size; this
    def withStackSize(size: UInt): Builder =
      stackSize = size; this
    def withAuxDataSize(size: UInt): Builder =
      auxDataSize = size; this

    def build(): Either[String, MemoryMap] =
      // Validate page size
      if pageSize < VmMinPageSize then
        return Left("Page size too small")
      if pageSize > VmMaxPageSize then
        return Left("Page size too big")
      if !isPowerOfTwo(pageSize) then
        return Left("Page size must be power of two")

      val alignedRoData = alignToPage(pageSize, roDataSize)
      val alignedRwData = alignToPage(pageSize, rwDataSize)
      val alignedStack = alignToPage(pageSize, stackSize)
      val alignedAux = alignToPage(pageSize, auxDataSize)

      // RO data starts at ZZ (0x10000)
      val roDataAddr = PvmConstants.ZZ

      // RW data starts at 2*ZZ + aligned RO data size (zone-aligned)
      val rwDataAddr = UInt(2L * PvmConstants.ZZ.toLong + alignToPage(PvmConstants.ZZ, roDataSize).toLong)
      val heapBase = ULong(rwDataAddr.toLong)

      // Stack base is fixed at 0xfefe0000, stack grows down
      val stackHigh = PvmConstants.StackBaseAddress  // 0xfefe0000
      val stackLow = UInt((stackHigh.toLong - alignedStack.toLong).toInt)

      // Aux/input region starts at 0xfeff0000
      val auxDataAddr = PvmConstants.InputStartAddress  // 0xfeff0000

      // Validate that heap doesn't overlap with stack
      val heapEndMax = rwDataAddr.toLong + alignedRwData.toLong
      if heapEndMax > stackLow.toLong then
        return Left("Memory size exceeded: heap would overlap stack")

      val maxHeap = UInt((stackLow.toLong - heapBase.toLong).toInt)

      Right(MemoryMap(
        pageSize = pageSize,
        roDataSize = alignedRoData,
        rwDataAddress = rwDataAddr,
        rwDataSize = alignedRwData,
        stackAddressHigh = stackHigh,
        stackSize = alignedStack,
        auxDataAddress = auxDataAddr,
        auxDataSize = alignedAux,
        heapBase = UInt(heapBase.toInt),
        maxHeapSize = maxHeap
      ))

    private def isPowerOfTwo(n: UInt): Boolean =
      n.signed != 0 && (n.signed & (n.signed - 1)) == 0

    private def alignToPage(pageSize: UInt, size: UInt): UInt =
      if (size.signed & (pageSize.signed - 1)) == 0 then size
      else UInt((size.signed + pageSize.signed) & (-pageSize.signed))

  def builder(pageSize: UInt): Builder = new Builder(pageSize)
