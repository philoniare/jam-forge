package io.forge.jam.pvm.memory

import spire.math.{UByte, UShort, UInt, ULong}
import scala.collection.mutable
import io.forge.jam.pvm.{MemoryResult, MemoryMap, AlignmentOps}
import java.nio.{ByteBuffer, ByteOrder}

/**
 * Dynamic memory implementation with page-on-demand allocation.
 *
 * This implementation allocates pages lazily as they are accessed,
 * tracking allocated pages in a Map.
 */
final class DynamicMemory private (
  private val _pageMap: PageMap,
  private val memoryMap: MemoryMap,
  private val pages: mutable.Map[Int, Array[Byte]], // Use Int key for faster lookup
  private var _heapSize: UInt
) extends Memory:

  override def pageMap: PageMap = _pageMap

  override def heapSize: UInt = _heapSize

  override def heapEnd: UInt = memoryMap.heapBase + _heapSize

  private val pageSizeInt: Int = memoryMap.pageSize.signed
  private val pageSizeShift: Int = java.lang.Integer.numberOfTrailingZeros(pageSizeInt)
  private val pageMask: Int = pageSizeInt - 1

  // Page cache for sequential access optimization
  private var cachedPageAddr: Int = -1
  private var cachedPage: Array[Byte] = null

  // ============================================================================
  // Load Operations
  // ============================================================================

  override def loadU8(address: UInt): MemoryResult[UByte] =
    checkReadable(address, 1) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val pageAddr = addr >>> pageSizeShift
        val offset = addr & pageMask
        // Fast path: check cached page
        if pageAddr == cachedPageAddr && cachedPage != null then
          MemoryResult.Success(UByte(cachedPage(offset)))
        else
          pages.get(pageAddr) match
            case Some(page) =>
              cachedPageAddr = pageAddr
              cachedPage = page
              MemoryResult.Success(UByte(page(offset)))
            case None => MemoryResult.Success(UByte(0)) // Unallocated pages read as 0

  override def loadU16(address: UInt): MemoryResult[UShort] =
    checkReadable(address, 2) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val pageAddr = addr >>> pageSizeShift
        val offset = addr & pageMask
        // Check if fully within one page
        if offset + 2 <= pageSizeInt then
          getPageForRead(pageAddr) match
            case Some(page) =>
              val lo = page(offset) & 0xff
              val hi = page(offset + 1) & 0xff
              MemoryResult.Success(UShort(((hi << 8) | lo).toShort))
            case None =>
              MemoryResult.Success(UShort(0))
        else
          // Cross-page access - use byte-by-byte
          var value = 0
          var i = 0
          while i < 2 do
            val b = loadByteRawInt(addr + i)
            value |= (b & 0xff) << (i * 8)
            i += 1
          MemoryResult.Success(UShort(value.toShort))

  override def loadU32(address: UInt): MemoryResult[UInt] =
    checkReadable(address, 4) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val pageAddr = addr >>> pageSizeShift
        val offset = addr & pageMask
        // Check if fully within one page
        if offset + 4 <= pageSizeInt then
          getPageForRead(pageAddr) match
            case Some(page) =>
              val b0 = page(offset) & 0xff
              val b1 = page(offset + 1) & 0xff
              val b2 = page(offset + 2) & 0xff
              val b3 = page(offset + 3) & 0xff
              val value = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
              MemoryResult.Success(UInt(value))
            case None =>
              MemoryResult.Success(UInt(0))
        else
          // Cross-page access - use byte-by-byte
          var value = 0
          var i = 0
          while i < 4 do
            val b = loadByteRawInt(addr + i)
            value |= (b & 0xff) << (i * 8)
            i += 1
          MemoryResult.Success(UInt(value))

  override def loadU64(address: UInt): MemoryResult[ULong] =
    checkReadable(address, 8) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val pageAddr = addr >>> pageSizeShift
        val offset = addr & pageMask
        // Check if fully within one page
        if offset + 8 <= pageSizeInt then
          getPageForRead(pageAddr) match
            case Some(page) =>
              // Use ByteBuffer for efficient little-endian long read
              val value = ByteBuffer.wrap(page, offset, 8)
                .order(ByteOrder.LITTLE_ENDIAN)
                .getLong()
              MemoryResult.Success(ULong(value))
            case None =>
              MemoryResult.Success(ULong(0L))
        else
          // Cross-page access - use byte-by-byte
          var value = 0L
          var i = 0
          while i < 8 do
            val b = loadByteRawInt(addr + i)
            value |= (b & 0xffL) << (i * 8)
            i += 1
          MemoryResult.Success(ULong(value))

  // ============================================================================
  // Store Operations
  // ============================================================================

  override def storeU8(address: UInt, value: UByte): MemoryResult[Unit] =
    checkWritable(address, 1) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val pageAddr = addr >>> pageSizeShift
        val offset = addr & pageMask
        val page = getOrCreatePageCached(pageAddr)
        page(offset) = value.toByte
        MemoryResult.Success(())

  override def storeU16(address: UInt, value: UShort): MemoryResult[Unit] =
    checkWritable(address, 2) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val pageAddr = addr >>> pageSizeShift
        val offset = addr & pageMask
        val v = value.toInt
        // Check if fully within one page
        if offset + 2 <= pageSizeInt then
          val page = getOrCreatePageCached(pageAddr)
          page(offset) = (v & 0xff).toByte
          page(offset + 1) = ((v >> 8) & 0xff).toByte
        else
          // Cross-page access - use byte-by-byte
          var i = 0
          while i < 2 do
            storeByteRawInt(addr + i, ((v >> (i * 8)) & 0xff).toByte)
            i += 1
        MemoryResult.Success(())

  override def storeU32(address: UInt, value: UInt): MemoryResult[Unit] =
    checkWritable(address, 4) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val pageAddr = addr >>> pageSizeShift
        val offset = addr & pageMask
        val v = value.signed
        // Check if fully within one page
        if offset + 4 <= pageSizeInt then
          val page = getOrCreatePageCached(pageAddr)
          page(offset) = (v & 0xff).toByte
          page(offset + 1) = ((v >> 8) & 0xff).toByte
          page(offset + 2) = ((v >> 16) & 0xff).toByte
          page(offset + 3) = ((v >> 24) & 0xff).toByte
        else
          // Cross-page access - use byte-by-byte
          var i = 0
          while i < 4 do
            storeByteRawInt(addr + i, ((v >> (i * 8)) & 0xff).toByte)
            i += 1
        MemoryResult.Success(())

  override def storeU64(address: UInt, value: ULong): MemoryResult[Unit] =
    checkWritable(address, 8) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val pageAddr = addr >>> pageSizeShift
        val offset = addr & pageMask
        val v = value.signed
        // Check if fully within one page
        if offset + 8 <= pageSizeInt then
          val page = getOrCreatePageCached(pageAddr)
          // Use ByteBuffer for efficient little-endian long write
          ByteBuffer.wrap(page, offset, 8)
            .order(ByteOrder.LITTLE_ENDIAN)
            .putLong(v)
        else
          // Cross-page access - use byte-by-byte
          var i = 0
          while i < 8 do
            storeByteRawInt(addr + i, ((v >> (i * 8)) & 0xff).toByte)
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
        val addr = address.signed
        val pageAddr = addr >>> pageSizeShift
        val offset = addr & pageMask
        val result = new Array[Byte](length)

        // Check if fully within one page
        if offset + length <= pageSizeInt then
          getPageForRead(pageAddr) match
            case Some(page) =>
              System.arraycopy(page, offset, result, 0, length)
            case None =>
            // Return zeros (already initialized to zeros)
        else
          // Cross-page access - use byte-by-byte
          var i = 0
          while i < length do
            result(i) = loadByteRawInt(addr + i)
            i += 1
        MemoryResult.Success(result)

  override def setMemorySlice(address: UInt, data: Array[Byte]): MemoryResult[Unit] =
    if data.isEmpty then return MemoryResult.Success(())

    checkWritable(address, data.length) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val pageAddr = addr >>> pageSizeShift
        val offset = addr & pageMask

        // Check if fully within one page
        if offset + data.length <= pageSizeInt then
          val page = getOrCreatePageCached(pageAddr)
          System.arraycopy(data, 0, page, offset, data.length)
        else
          // Cross-page access - use byte-by-byte
          var i = 0
          while i < data.length do
            storeByteRawInt(addr + i, data(i))
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
    if newHeapSizeLong > 0xffffffffL then return None

    val newHeapSize = UInt(newHeapSizeLong.toInt)

    // Check against max heap size
    if newHeapSize.toLong > memoryMap.maxHeapSize.toLong then return None

    _heapSize = newHeapSize
    val newHeapEnd = memoryMap.heapBase + newHeapSize

    // Update page map for new heap pages
    val prevPageBoundary = AlignmentOps.alignUp(prevHeapEnd, memoryMap.pageSize)
    if newHeapEnd.toLong > prevPageBoundary.toLong then
      val startPage = UInt((prevPageBoundary.toLong / memoryMap.pageSize.toLong).toInt)
      val endPage =
        UInt((AlignmentOps.alignUp(newHeapEnd, memoryMap.pageSize).toLong / memoryMap.pageSize.toLong).toInt)
      val pageCount = (endPage.signed - startPage.signed)
      if pageCount > 0 then
        _pageMap.updatePages(startPage, pageCount, PageAccess.ReadWrite)

    Some(prevHeapEnd)

  // ============================================================================
  // Internal Helpers
  // ============================================================================

  /**
   * Gets page for read, updating cache if needed.
   */
  private inline def getPageForRead(pageAddr: Int): Option[Array[Byte]] =
    if pageAddr == cachedPageAddr && cachedPage != null then
      Some(cachedPage)
    else
      pages.get(pageAddr) match
        case Some(page) =>
          cachedPageAddr = pageAddr
          cachedPage = page
          Some(page)
        case None => None

  /**
   * Gets or creates a page using Int page address, with cache update.
   */
  private inline def getOrCreatePageCached(pageAddr: Int): Array[Byte] =
    if pageAddr == cachedPageAddr && cachedPage != null then
      cachedPage
    else
      val page = pages.getOrElseUpdate(pageAddr, new Array[Byte](pageSizeInt))
      cachedPageAddr = pageAddr
      cachedPage = page
      page

  /**
   * Gets or creates a page using Int page address (faster than UInt).
   */
  private def getOrCreatePageInt(pageAddr: Int): Array[Byte] =
    pages.getOrElseUpdate(pageAddr, new Array[Byte](pageSizeInt))

  /**
   * Gets or creates a page at the given address.
   */
  private def getOrCreatePage(address: UInt): Array[Byte] =
    val pageAddr = address.signed >>> pageSizeShift
    getOrCreatePageInt(pageAddr)

  /**
   * Loads a byte without permission checking, using primitive Int.
   */
  private inline def loadByteRawInt(addr: Int): Byte =
    val pageAddr = addr >>> pageSizeShift
    val offset = addr & pageMask
    // Check cache first
    if pageAddr == cachedPageAddr && cachedPage != null then
      cachedPage(offset)
    else
      pages.get(pageAddr) match
        case Some(page) =>
          cachedPageAddr = pageAddr
          cachedPage = page
          page(offset)
        case None => 0.toByte

  /**
   * Loads a byte without permission checking.
   */
  private def loadByteRaw(address: UInt): Byte =
    loadByteRawInt(address.signed)

  /**
   * Stores a byte without permission checking, using primitive Int.
   */
  private inline def storeByteRawInt(addr: Int, value: Byte): Unit =
    val pageAddr = addr >>> pageSizeShift
    val offset = addr & pageMask
    val page = getOrCreatePageCached(pageAddr)
    page(offset) = value

  /**
   * Stores a byte without permission checking.
   */
  private def storeByteRaw(address: UInt, value: Byte): Unit =
    storeByteRawInt(address.signed, value)

  /**
   * Clears all allocated pages.
   */
  def clear(): Unit =
    pages.clear()
    _heapSize = UInt(0)
    cachedPageAddr = -1
    cachedPage = null

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
      pages = mutable.Map.empty[Int, Array[Byte]],
      _heapSize = UInt(0)
    )
