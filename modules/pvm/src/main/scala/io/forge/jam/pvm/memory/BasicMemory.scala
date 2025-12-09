package io.forge.jam.pvm.memory

import spire.math.{UByte, UShort, UInt, ULong}
import io.forge.jam.pvm.{MemoryResult, MemoryMap, Abi, PvmConstants}
import io.forge.jam.pvm.types.*

/**
 * Basic memory implementation for non-dynamic paging mode.
 *
 * Implements the Memory trait with per-page access control via PageMap.
 * Manages separate memory regions: roData, rwData, stack, heap, and aux.
 */
final class BasicMemory private (
  private var _pageMap: PageMap,
  private val memoryMap: MemoryMap,
  private var roData: Array[Byte],
  private val rwData: Array[Byte],
  private val stack: Array[Byte],
  private val aux: Array[Byte],
  private var _heapSize: UInt,
  private var isDirty: Boolean
) extends Memory:

  override def pageMap: PageMap = _pageMap

  override def heapSize: UInt = _heapSize

  override def heapEnd: UInt = memoryMap.heapBase + _heapSize

  // ============================================================================
  // Load Operations
  // ============================================================================

  override def loadU8(address: UInt): MemoryResult[UByte] =
    checkReadable(address, 1) match
      case Some(err) => err
      case None =>
        readByte(address) match
          case Some(b) => MemoryResult.Success(UByte(b))
          case None => MemoryResult.OutOfBounds(address)

  override def loadU16(address: UInt): MemoryResult[UShort] =
    checkReadable(address, 2) match
      case Some(err) => err
      case None =>
        val b0 = readByte(address)
        val b1 = readByte(address + UInt(1))
        (b0, b1) match
          case (Some(lo), Some(hi)) =>
            val value = ((hi & 0xFF) << 8) | (lo & 0xFF)
            MemoryResult.Success(UShort(value.toShort))
          case _ => MemoryResult.OutOfBounds(address)

  override def loadU32(address: UInt): MemoryResult[UInt] =
    checkReadable(address, 4) match
      case Some(err) => err
      case None =>
        val bytes = readBytes(address, 4)
        bytes match
          case Some(arr) =>
            val value = (arr(0) & 0xFF) |
              ((arr(1) & 0xFF) << 8) |
              ((arr(2) & 0xFF) << 16) |
              ((arr(3) & 0xFF) << 24)
            MemoryResult.Success(UInt(value))
          case None => MemoryResult.OutOfBounds(address)

  override def loadU64(address: UInt): MemoryResult[ULong] =
    checkReadable(address, 8) match
      case Some(err) => err
      case None =>
        val bytes = readBytes(address, 8)
        bytes match
          case Some(arr) =>
            var value = 0L
            var i = 0
            while i < 8 do
              value |= (arr(i) & 0xFFL) << (i * 8)
              i += 1
            MemoryResult.Success(ULong(value))
          case None => MemoryResult.OutOfBounds(address)

  // ============================================================================
  // Store Operations
  // ============================================================================

  override def storeU8(address: UInt, value: UByte): MemoryResult[Unit] =
    checkWritable(address, 1) match
      case Some(err) => err
      case None =>
        if writeByte(address, value.toByte) then
          isDirty = true
          MemoryResult.Success(())
        else
          MemoryResult.OutOfBounds(address)

  override def storeU16(address: UInt, value: UShort): MemoryResult[Unit] =
    checkWritable(address, 2) match
      case Some(err) => err
      case None =>
        val ok1 = writeByte(address, (value.toInt & 0xFF).toByte)
        val ok2 = writeByte(address + UInt(1), ((value.toInt >> 8) & 0xFF).toByte)
        if ok1 && ok2 then
          isDirty = true
          MemoryResult.Success(())
        else
          MemoryResult.OutOfBounds(address)

  override def storeU32(address: UInt, value: UInt): MemoryResult[Unit] =
    checkWritable(address, 4) match
      case Some(err) => err
      case None =>
        val v = value.signed
        val bytes = Array(
          (v & 0xFF).toByte,
          ((v >> 8) & 0xFF).toByte,
          ((v >> 16) & 0xFF).toByte,
          ((v >> 24) & 0xFF).toByte
        )
        if writeBytes(address, bytes) then
          isDirty = true
          MemoryResult.Success(())
        else
          MemoryResult.OutOfBounds(address)

  override def storeU64(address: UInt, value: ULong): MemoryResult[Unit] =
    checkWritable(address, 8) match
      case Some(err) => err
      case None =>
        val v = value.signed
        val bytes = new Array[Byte](8)
        var i = 0
        while i < 8 do
          bytes(i) = ((v >> (i * 8)) & 0xFF).toByte
          i += 1
        if writeBytes(address, bytes) then
          isDirty = true
          MemoryResult.Success(())
        else
          MemoryResult.OutOfBounds(address)

  // ============================================================================
  // Bulk Operations
  // ============================================================================

  override def getMemorySlice(address: UInt, length: Int): MemoryResult[Array[Byte]] =
    if length == 0 then return MemoryResult.Success(Array.empty)

    checkReadable(address, length) match
      case Some(err) => err
      case None =>
        readBytes(address, length) match
          case Some(arr) => MemoryResult.Success(arr)
          case None => MemoryResult.OutOfBounds(address)

  override def setMemorySlice(address: UInt, data: Array[Byte]): MemoryResult[Unit] =
    if data.isEmpty then return MemoryResult.Success(())

    checkWritable(address, data.length) match
      case Some(err) => err
      case None =>
        if writeBytes(address, data) then
          isDirty = true
          MemoryResult.Success(())
        else
          MemoryResult.OutOfBounds(address)

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
    val prevPageBoundary = alignToNextPageSize(memoryMap.pageSize, prevHeapEnd)
    if newHeapEnd.toLong > prevPageBoundary.toLong then
      val startPage = UInt((prevPageBoundary.toLong / memoryMap.pageSize.toLong).toInt)
      val endPage = UInt((alignToNextPageSize(memoryMap.pageSize, newHeapEnd).toLong / memoryMap.pageSize.toLong).toInt)
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
   * Reads a single byte from the appropriate region.
   */
  private def readByte(address: UInt): Option[Byte] =
    val addr = address.signed
    val addrLong = address.toLong

    // Check RO data region
    val roStart = memoryMap.roDataAddress.signed
    val roEnd = roStart + roData.length
    if addr >= roStart && addr < roEnd then
      return Some(roData(addr - roStart))
    else if addr >= roStart && addr < (roStart + memoryMap.roDataSize.signed) then
      // Within RO region but past actual data - return 0
      return Some(0.toByte)

    // Check RW data + heap region
    val rwStart = memoryMap.rwDataAddress.signed
    val heapEndAddr = (memoryMap.heapBase + _heapSize).signed
    val effectiveEnd = math.max(rwStart + memoryMap.rwDataSize.signed, heapEndAddr)
    if addr >= rwStart && addr < effectiveEnd then
      val offset = addr - rwStart
      if offset >= 0 && offset < rwData.length then
        return Some(rwData(offset))
      else
        return Some(0.toByte) // Beyond buffer but in valid heap region

    // Check stack region
    val stackLow = memoryMap.stackAddressLow.signed
    val stackHigh = memoryMap.stackAddressHigh.signed
    if addr >= stackLow && addr < stackHigh then
      val offset = addr - stackLow
      if offset >= 0 && offset < stack.length then
        return Some(stack(offset))
      else
        return Some(0.toByte)

    // Check aux data region
    val auxStart = memoryMap.auxDataAddress.signed
    val auxEnd = auxStart + memoryMap.auxDataSize.signed
    if addr >= auxStart && addr < auxEnd then
      val offset = addr - auxStart
      if offset >= 0 && offset < aux.length then
        return Some(aux(offset))
      else
        return Some(0.toByte)

    None

  /**
   * Reads multiple bytes from memory.
   */
  private def readBytes(address: UInt, length: Int): Option[Array[Byte]] =
    val result = new Array[Byte](length)
    var i = 0
    while i < length do
      readByte(address + UInt(i)) match
        case Some(b) => result(i) = b
        case None => return None
      i += 1
    Some(result)

  /**
   * Writes a single byte to the appropriate region.
   */
  private def writeByte(address: UInt, value: Byte): Boolean =
    val addr = address.signed

    // RO data region - not writable
    val roStart = memoryMap.roDataAddress.signed
    val roEnd = roStart + memoryMap.roDataSize.signed
    if addr >= roStart && addr < roEnd then
      return false

    // Check RW data + heap region
    val rwStart = memoryMap.rwDataAddress.signed
    val heapEndAddr = (memoryMap.heapBase + _heapSize).signed
    val effectiveEnd = math.max(rwStart + memoryMap.rwDataSize.signed, heapEndAddr)
    if addr >= rwStart && addr < effectiveEnd then
      val offset = addr - rwStart
      if offset >= 0 && offset < rwData.length then
        rwData(offset) = value
        return true
      else
        // Within valid heap but beyond buffer - would need lazy expansion
        return false

    // Check stack region
    val stackLow = memoryMap.stackAddressLow.signed
    val stackHigh = memoryMap.stackAddressHigh.signed
    if addr >= stackLow && addr < stackHigh then
      val offset = addr - stackLow
      if offset >= 0 && offset < stack.length then
        stack(offset) = value
        return true
      else
        return false

    // Check aux data region (GP stack portion is writable)
    val gpStackLow = PvmConstants.GpStackLow.signed
    val gpStackBase = PvmConstants.GpStackBase.signed
    if addr >= gpStackLow && addr < gpStackBase then
      val auxStart = memoryMap.auxDataAddress.signed
      val offset = addr - auxStart
      if offset >= 0 && offset < aux.length then
        aux(offset) = value
        return true

    false

  /**
   * Writes multiple bytes to memory.
   */
  private def writeBytes(address: UInt, data: Array[Byte]): Boolean =
    var i = 0
    while i < data.length do
      if !writeByte(address + UInt(i), data(i)) then
        return false
      i += 1
    true

  /**
   * Aligns a size up to the next page boundary.
   */
  private def alignToNextPageSize(pageSize: UInt, size: UInt): UInt =
    val ps = pageSize.toLong
    val sz = size.toLong
    UInt((((sz + ps - 1) / ps) * ps).toInt)

  /**
   * Marks memory as dirty (needs reset before reuse).
   */
  def markDirty(): Unit = isDirty = true

  /**
   * Returns true if memory has been modified.
   */
  def dirty: Boolean = isDirty

  /**
   * Sets the read-only data (typically from program blob).
   */
  def setRoData(data: Array[Byte]): Unit =
    roData = data

object BasicMemory:
  /**
   * Creates a new BasicMemory instance from a MemoryMap.
   */
  def create(memoryMap: MemoryMap): BasicMemory =
    val pageMap = initializePageMap(memoryMap, None)

    new BasicMemory(
      _pageMap = pageMap,
      memoryMap = memoryMap,
      roData = new Array[Byte](memoryMap.roDataSize.signed),
      rwData = new Array[Byte](memoryMap.rwDataSize.signed),
      stack = new Array[Byte](memoryMap.stackSize.signed),
      aux = new Array[Byte](memoryMap.auxDataSize.signed),
      _heapSize = UInt(0),
      isDirty = false
    )

  /**
   * Creates a BasicMemory with initial RO/RW data.
   */
  def create(memoryMap: MemoryMap, roData: Array[Byte], rwData: Array[Byte]): BasicMemory =
    val pageMap = initializePageMap(memoryMap, Some(roData.length))

    // Copy rwData into buffer
    val rwBuffer = new Array[Byte](memoryMap.rwDataSize.signed)
    System.arraycopy(rwData, 0, rwBuffer, 0, math.min(rwData.length, rwBuffer.length))

    new BasicMemory(
      _pageMap = pageMap,
      memoryMap = memoryMap,
      roData = roData,
      rwData = rwBuffer,
      stack = new Array[Byte](memoryMap.stackSize.signed),
      aux = new Array[Byte](memoryMap.auxDataSize.signed),
      _heapSize = UInt(0),
      isDirty = false
    )

  /**
   * Initializes the PageMap with correct page access permissions.
   */
  private def initializePageMap(memoryMap: MemoryMap, roDataActualSize: Option[Int]): PageMap =
    val mappings = scala.collection.mutable.ListBuffer[(UInt, UInt, PageAccess)]()

    // RO data region - READ_ONLY
    val roSize = roDataActualSize.getOrElse(memoryMap.roDataSize.signed)
    if roSize > 0 then
      mappings += ((memoryMap.roDataAddress, UInt(roSize), PageAccess.ReadOnly))

    // RW data region - READ_WRITE
    if memoryMap.rwDataSize.signed > 0 then
      mappings += ((memoryMap.rwDataAddress, memoryMap.rwDataSize, PageAccess.ReadWrite))

    // Stack region - READ_WRITE
    if memoryMap.stackSize.signed > 0 then
      mappings += ((memoryMap.stackAddressLow, memoryMap.stackSize, PageAccess.ReadWrite))

    // Aux data region handling with GP stack
    if memoryMap.auxDataSize.signed > 0 then
      val gpStackLow = PvmConstants.GpStackLow
      val gpStackBase = PvmConstants.GpStackBase
      val gpStackSize = PvmConstants.GpStackSize

      val auxStart = memoryMap.auxDataAddress
      val auxEnd = auxStart + memoryMap.auxDataSize

      // Check if GP stack region overlaps with aux data
      if gpStackLow.toLong >= auxStart.toLong && gpStackBase.toLong <= auxEnd.toLong then
        // Mark before GP stack as READ_ONLY
        if gpStackLow.toLong > auxStart.toLong then
          mappings += ((auxStart, UInt((gpStackLow.toLong - auxStart.toLong).toInt), PageAccess.ReadOnly))

        // GP stack region - READ_WRITE
        mappings += ((gpStackLow, gpStackSize, PageAccess.ReadWrite))

        // Mark after GP stack as READ_ONLY
        if gpStackBase.toLong < auxEnd.toLong then
          mappings += ((gpStackBase, UInt((auxEnd.toLong - gpStackBase.toLong).toInt), PageAccess.ReadOnly))
      else
        // No GP stack overlap - mark entire aux as READ_ONLY
        mappings += ((auxStart, memoryMap.auxDataSize, PageAccess.ReadOnly))

    PageMap.create(mappings.toList, memoryMap.pageSize)

  /**
   * Aligns size to the next page boundary.
   */
  def alignToNextPageSize(pageSize: Int, size: Int): Int =
    ((size + pageSize - 1) / pageSize) * pageSize
