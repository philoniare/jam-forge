package io.forge.jam.pvm.memory

import spire.math.{UByte, UShort, UInt, ULong}
import scala.collection.mutable
import io.forge.jam.pvm.{MemoryResult, MemoryMap, Abi}
import io.forge.jam.pvm.types.*

/**
 * Dynamic memory implementation with page-on-demand allocation.
 *
 * This implementation allocates pages lazily as they are accessed,
 * tracking allocated pages in a Map.
 */
final class DynamicMemory private (
  private var _pageMap: PageMap,
  private val memoryMap: MemoryMap,
  private val pages: mutable.Map[UInt, Array[Byte]],
  private var _heapSize: UInt
) extends Memory:

  override def pageMap: PageMap = _pageMap

  override def heapSize: UInt = _heapSize

  override def heapEnd: UInt = memoryMap.heapBase + _heapSize

  private val pageSizeInt: Int = memoryMap.pageSize.signed
  private val pageSizeShift: Int = java.lang.Integer.numberOfTrailingZeros(pageSizeInt)

  // ============================================================================
  // Load Operations
  // ============================================================================

  override def loadU8(address: UInt): MemoryResult[UByte] =
    checkReadable(address, 1) match
      case Some(err) => err
      case None =>
        val pageAddr = getPageAddress(address)
        val offset = getPageOffset(address)
        pages.get(pageAddr) match
          case Some(page) => MemoryResult.Success(UByte(page(offset)))
          case None => MemoryResult.Success(UByte(0)) // Unallocated pages read as 0

  override def loadU16(address: UInt): MemoryResult[UShort] =
    checkReadable(address, 2) match
      case Some(err) => err
      case None =>
        var value = 0
        var i = 0
        while i < 2 do
          val b = loadByteRaw(address + UInt(i))
          value |= (b & 0xFF) << (i * 8)
          i += 1
        MemoryResult.Success(UShort(value.toShort))

  override def loadU32(address: UInt): MemoryResult[UInt] =
    checkReadable(address, 4) match
      case Some(err) => err
      case None =>
        var value = 0
        var i = 0
        while i < 4 do
          val b = loadByteRaw(address + UInt(i))
          value |= (b & 0xFF) << (i * 8)
          i += 1
        MemoryResult.Success(UInt(value))

  override def loadU64(address: UInt): MemoryResult[ULong] =
    checkReadable(address, 8) match
      case Some(err) => err
      case None =>
        var value = 0L
        var i = 0
        while i < 8 do
          val b = loadByteRaw(address + UInt(i))
          value |= (b & 0xFFL) << (i * 8)
          i += 1
        MemoryResult.Success(ULong(value))

  // ============================================================================
  // Store Operations
  // ============================================================================

  override def storeU8(address: UInt, value: UByte): MemoryResult[Unit] =
    checkWritable(address, 1) match
      case Some(err) => err
      case None =>
        val page = getOrCreatePage(address)
        val offset = getPageOffset(address)
        page(offset) = value.toByte
        MemoryResult.Success(())

  override def storeU16(address: UInt, value: UShort): MemoryResult[Unit] =
    checkWritable(address, 2) match
      case Some(err) => err
      case None =>
        var i = 0
        val v = value.toInt
        while i < 2 do
          storeByteRaw(address + UInt(i), ((v >> (i * 8)) & 0xFF).toByte)
          i += 1
        MemoryResult.Success(())

  override def storeU32(address: UInt, value: UInt): MemoryResult[Unit] =
    checkWritable(address, 4) match
      case Some(err) => err
      case None =>
        var i = 0
        val v = value.signed
        while i < 4 do
          storeByteRaw(address + UInt(i), ((v >> (i * 8)) & 0xFF).toByte)
          i += 1
        MemoryResult.Success(())

  override def storeU64(address: UInt, value: ULong): MemoryResult[Unit] =
    checkWritable(address, 8) match
      case Some(err) => err
      case None =>
        var i = 0
        val v = value.signed
        while i < 8 do
          storeByteRaw(address + UInt(i), ((v >> (i * 8)) & 0xFF).toByte)
          i += 1
        MemoryResult.Success(())

  // ============================================================================
  // Bulk Operations
  // ============================================================================

  override def getMemorySlice(address: UInt, length: Int): MemoryResult[Array[Byte]] =
    if length == 0 then return MemoryResult.Success(Array.empty)

    checkReadable(address, length) match
      case Some(err) => err
      case None =>
        val result = new Array[Byte](length)
        var i = 0
        while i < length do
          result(i) = loadByteRaw(address + UInt(i))
          i += 1
        MemoryResult.Success(result)

  override def setMemorySlice(address: UInt, data: Array[Byte]): MemoryResult[Unit] =
    if data.isEmpty then return MemoryResult.Success(())

    checkWritable(address, data.length) match
      case Some(err) => err
      case None =>
        var i = 0
        while i < data.length do
          storeByteRaw(address + UInt(i), data(i))
          i += 1
        MemoryResult.Success(())

  // ============================================================================
  // Heap Operations (sbrk)
  // ============================================================================

  override def sbrk(size: UInt): Option[UInt] =
    val prevHeapEnd = heapEnd

    // sbrk(0) just returns current heap end
    if size == UInt(0) then return Some(prevHeapEnd)

    // Check for overflow
    val newHeapSizeLong = _heapSize.toLong + size.toLong
    if newHeapSizeLong > 0xFFFFFFFFL then return None

    val newHeapSize = UInt(newHeapSizeLong.toInt)

    // Check against max heap size
    if newHeapSize.toLong > memoryMap.maxHeapSize.toLong then return None

    _heapSize = newHeapSize
    val newHeapEnd = memoryMap.heapBase + newHeapSize

    // Update page map for new heap pages
    val prevPageBoundary = alignToNextPageSize(prevHeapEnd)
    if newHeapEnd.toLong > prevPageBoundary.toLong then
      val startPage = UInt((prevPageBoundary.toLong / memoryMap.pageSize.toLong).toInt)
      val endPage = UInt((alignToNextPageSize(newHeapEnd).toLong / memoryMap.pageSize.toLong).toInt)
      val pageCount = (endPage.signed - startPage.signed)
      if pageCount > 0 then
        _pageMap.updatePages(startPage, pageCount, PageAccess.ReadWrite)

    Some(prevHeapEnd)

  // ============================================================================
  // Internal Helpers
  // ============================================================================

  private def checkReadable(address: UInt, length: Int): Option[MemoryResult[Nothing]] =
    val (isReadable, failAddr) = _pageMap.isReadable(address, length)
    if !isReadable then
      Some(MemoryResult.Segfault(address, _pageMap.alignToPageStart(failAddr)))
    else
      None

  private def checkWritable(address: UInt, length: Int): Option[MemoryResult[Nothing]] =
    val (isWritable, failAddr) = _pageMap.isWritable(address, length)
    if !isWritable then
      Some(MemoryResult.Segfault(address, _pageMap.alignToPageStart(failAddr)))
    else
      None

  /**
   * Gets the page address for a given address.
   */
  private inline def getPageAddress(address: UInt): UInt =
    UInt((address.signed >>> pageSizeShift) << pageSizeShift)

  /**
   * Gets the offset within a page for a given address.
   */
  private inline def getPageOffset(address: UInt): Int =
    address.signed & (pageSizeInt - 1)

  /**
   * Gets or creates a page at the given address.
   */
  private def getOrCreatePage(address: UInt): Array[Byte] =
    val pageAddr = getPageAddress(address)
    pages.getOrElseUpdate(pageAddr, new Array[Byte](pageSizeInt))

  /**
   * Loads a byte without permission checking.
   */
  private def loadByteRaw(address: UInt): Byte =
    val pageAddr = getPageAddress(address)
    val offset = getPageOffset(address)
    pages.get(pageAddr) match
      case Some(page) => page(offset)
      case None => 0.toByte

  /**
   * Stores a byte without permission checking.
   */
  private def storeByteRaw(address: UInt, value: Byte): Unit =
    val page = getOrCreatePage(address)
    val offset = getPageOffset(address)
    page(offset) = value

  /**
   * Aligns a size up to the next page boundary.
   */
  private def alignToNextPageSize(size: UInt): UInt =
    val ps = memoryMap.pageSize.toLong
    val sz = size.toLong
    UInt((((sz + ps - 1) / ps) * ps).toInt)

  /**
   * Clears all allocated pages.
   */
  def clear(): Unit =
    pages.clear()
    _heapSize = UInt(0)

  /**
   * Returns the number of allocated pages.
   */
  def allocatedPageCount: Int = pages.size

object DynamicMemory:
  /**
   * Creates a new DynamicMemory instance from a MemoryMap.
   */
  def create(memoryMap: MemoryMap): DynamicMemory =
    val pageMap = new PageMap(memoryMap.pageSize)

    // Initialize page permissions for the memory regions
    if memoryMap.roDataSize.signed > 0 then
      pageMap.update(memoryMap.roDataAddress, memoryMap.roDataSize.signed, PageAccess.ReadOnly)

    if memoryMap.rwDataSize.signed > 0 then
      pageMap.update(memoryMap.rwDataAddress, memoryMap.rwDataSize.signed, PageAccess.ReadWrite)

    if memoryMap.stackSize.signed > 0 then
      pageMap.update(memoryMap.stackAddressLow, memoryMap.stackSize.signed, PageAccess.ReadWrite)

    if memoryMap.auxDataSize.signed > 0 then
      pageMap.update(memoryMap.auxDataAddress, memoryMap.auxDataSize.signed, PageAccess.ReadOnly)

    new DynamicMemory(
      _pageMap = pageMap,
      memoryMap = memoryMap,
      pages = mutable.Map.empty,
      _heapSize = UInt(0)
    )
