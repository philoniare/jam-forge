package io.forge.jam.pvm.memory

import spire.math.{UByte, UShort, UInt, ULong}
import io.forge.jam.pvm.{MemoryResult, MemoryMap, PvmConstants, AlignmentOps}
import java.nio.{ByteBuffer, ByteOrder}

/**
 * Basic memory implementation for non-dynamic paging mode.
 *
 * Implements the Memory trait with per-page access control via PageMap.
 * Manages separate memory regions: roData, rwData, stack, heap, and aux.
 */
final class BasicMemory private (
  private val _pageMap: PageMap,
  private val memoryMap: MemoryMap,
  private var roData: Array[Byte],
  private var rwData: Array[Byte], // var to allow resizing during sbrk
  private val stack: Array[Byte],
  private val aux: Array[Byte],
  private var _heapSize: UInt,
  private var isDirty: Boolean
) extends Memory:

  override def pageMap: PageMap = _pageMap

  override def heapSize: UInt = _heapSize

  override def heapEnd: UInt = memoryMap.heapBase + _heapSize

  // Pre-computed region boundaries (primitive Int for faster access)
  private val roStart: Int = memoryMap.roDataAddress.signed
  private val roRegionSize: Int = memoryMap.roDataSize.signed
  private val rwStart: Int = memoryMap.rwDataAddress.signed
  private val rwRegionSize: Int = memoryMap.rwDataSize.signed
  private val stackLow: Int = memoryMap.stackAddressLow.signed
  private val stackHigh: Int = memoryMap.stackAddressHigh.signed
  private val auxStart: Int = memoryMap.auxDataAddress.signed
  private val auxRegionSize: Int = memoryMap.auxDataSize.signed
  private val heapBaseInt: Int = memoryMap.heapBase.signed
  private val gpStackLowInt: Int = PvmConstants.GpStackLow.signed
  private val gpStackBaseInt: Int = PvmConstants.GpStackBase.signed

  // Region cache for sequential access optimization
  // Region IDs: 0 = none, 1 = roData, 2 = rwData/heap, 3 = stack, 4 = aux
  private var cachedRegionId: Int = 0
  private var cachedRegionStart: Int = 0
  private var cachedRegionEnd: Int = 0
  private var cachedRegionArray: Array[Byte] = null
  private var cachedRegionOffset: Int = 0

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
        val addr = address.signed
        // Try optimized bulk read if within single region
        findRegionBulk(addr, 2) match
          case Some((arr, offset)) =>
            val lo = arr(offset) & 0xff
            val hi = arr(offset + 1) & 0xff
            MemoryResult.Success(UShort(((hi << 8) | lo).toShort))
          case None =>
            // Fall back to byte-by-byte
            val b0 = readByteInt(addr)
            val b1 = readByteInt(addr + 1)
            (b0, b1) match
              case (Some(lo), Some(hi)) =>
                val value = ((hi & 0xff) << 8) | (lo & 0xff)
                MemoryResult.Success(UShort(value.toShort))
              case _ => MemoryResult.OutOfBounds(address)

  override def loadU32(address: UInt): MemoryResult[UInt] =
    checkReadable(address, 4) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        // Try optimized bulk read if within single region
        findRegionBulk(addr, 4) match
          case Some((arr, offset)) =>
            // Direct byte extraction (little-endian)
            val b0 = arr(offset) & 0xff
            val b1 = arr(offset + 1) & 0xff
            val b2 = arr(offset + 2) & 0xff
            val b3 = arr(offset + 3) & 0xff
            val value = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
            MemoryResult.Success(UInt(value))
          case None =>
            // Fall back to byte-by-byte
            readBytesInt(addr, 4) match
              case Some(arr) =>
                val value = (arr(0) & 0xff) |
                  ((arr(1) & 0xff) << 8) |
                  ((arr(2) & 0xff) << 16) |
                  ((arr(3) & 0xff) << 24)
                MemoryResult.Success(UInt(value))
              case None => MemoryResult.OutOfBounds(address)

  override def loadU64(address: UInt): MemoryResult[ULong] =
    checkReadable(address, 8) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        // Try optimized bulk read if within single region
        findRegionBulk(addr, 8) match
          case Some((arr, offset)) =>
            // Use ByteBuffer for efficient little-endian long read
            val value = ByteBuffer.wrap(arr, offset, 8)
              .order(ByteOrder.LITTLE_ENDIAN)
              .getLong()
            MemoryResult.Success(ULong(value))
          case None =>
            // Fall back to byte-by-byte
            readBytesInt(addr, 8) match
              case Some(arr) =>
                var value = 0L
                var i = 0
                while i < 8 do
                  value |= (arr(i) & 0xffL) << (i * 8)
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
        val addr = address.signed
        val v = value.toInt
        // Try optimized bulk write if within single region
        findWritableRegionBulk(addr, 2) match
          case Some((arr, offset)) =>
            arr(offset) = (v & 0xff).toByte
            arr(offset + 1) = ((v >> 8) & 0xff).toByte
            isDirty = true
            MemoryResult.Success(())
          case None =>
            // Fall back to byte-by-byte
            val ok1 = writeByteInt(addr, (v & 0xff).toByte)
            val ok2 = writeByteInt(addr + 1, ((v >> 8) & 0xff).toByte)
            if ok1 && ok2 then
              isDirty = true
              MemoryResult.Success(())
            else
              MemoryResult.OutOfBounds(address)

  override def storeU32(address: UInt, value: UInt): MemoryResult[Unit] =
    checkWritable(address, 4) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val v = value.signed
        // Try optimized bulk write if within single region
        findWritableRegionBulk(addr, 4) match
          case Some((arr, offset)) =>
            // Direct byte write (little-endian)
            arr(offset) = (v & 0xff).toByte
            arr(offset + 1) = ((v >> 8) & 0xff).toByte
            arr(offset + 2) = ((v >> 16) & 0xff).toByte
            arr(offset + 3) = ((v >> 24) & 0xff).toByte
            isDirty = true
            MemoryResult.Success(())
          case None =>
            // Fall back to byte-by-byte using pooled array
            val bytes = BasicMemory.pool4.get()
            bytes(0) = (v & 0xff).toByte
            bytes(1) = ((v >> 8) & 0xff).toByte
            bytes(2) = ((v >> 16) & 0xff).toByte
            bytes(3) = ((v >> 24) & 0xff).toByte
            if writeBytesInt(addr, bytes) then
              isDirty = true
              MemoryResult.Success(())
            else
              MemoryResult.OutOfBounds(address)

  override def storeU64(address: UInt, value: ULong): MemoryResult[Unit] =
    checkWritable(address, 8) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val v = value.signed
        // Try optimized bulk write if within single region
        findWritableRegionBulk(addr, 8) match
          case Some((arr, offset)) =>
            // Use ByteBuffer for efficient little-endian long write
            ByteBuffer.wrap(arr, offset, 8)
              .order(ByteOrder.LITTLE_ENDIAN)
              .putLong(v)
            isDirty = true
            MemoryResult.Success(())
          case None =>
            // Fall back to byte-by-byte using pooled array
            val bytes = BasicMemory.pool8.get()
            var i = 0
            while i < 8 do
              bytes(i) = ((v >> (i * 8)) & 0xff).toByte
              i += 1
            if writeBytesInt(addr, bytes) then
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
        val addr = address.signed
        // Try optimized bulk read if within single region
        findRegionBulk(addr, length) match
          case Some((arr, offset)) =>
            // Direct array copy
            val result = new Array[Byte](length)
            System.arraycopy(arr, offset, result, 0, length)
            MemoryResult.Success(result)
          case None =>
            // Fall back to byte-by-byte
            readBytesInt(addr, length) match
              case Some(arr) => MemoryResult.Success(arr)
              case None => MemoryResult.OutOfBounds(address)

  override def setMemorySlice(address: UInt, data: Array[Byte]): MemoryResult[Unit] =
    if data.isEmpty then return MemoryResult.Success(())

    checkWritable(address, data.length) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        // Try optimized bulk write if within single region
        findWritableRegionBulk(addr, data.length) match
          case Some((arr, offset)) =>
            // Direct array copy
            System.arraycopy(data, 0, arr, offset, data.length)
            isDirty = true
            MemoryResult.Success(())
          case None =>
            // Fall back to byte-by-byte
            if writeBytesInt(addr, data) then
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
    if newHeapSizeLong > 0xffffffffL then return None

    val newHeapSize = UInt(newHeapSizeLong.toInt)

    // Check against max heap size
    if newHeapSize.toLong > memoryMap.maxHeapSize.toLong then return None

    _heapSize = newHeapSize
    val newHeapEnd = memoryMap.heapBase + newHeapSize

    // Invalidate region cache when heap size changes
    if cachedRegionId == 2 then
      cachedRegionId = 0

    // Ensure rwData buffer is large enough for the new heap size
    // The heap lives in the rwData buffer starting at heapBase relative to rwDataAddress
    val requiredBufferSize = newHeapSize.signed
    if requiredBufferSize > rwData.length then
      val newBuffer = new Array[Byte](requiredBufferSize)
      System.arraycopy(rwData, 0, newBuffer, 0, rwData.length)
      rwData = newBuffer

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
  // Internal Helpers - Optimized with region caching
  // ============================================================================

  /**
   * Finds the backing array and offset for a bulk read operation.
   * Returns None if the range spans multiple regions or is out of bounds.
   */
  private def findRegionBulk(addr: Int, length: Int): Option[(Array[Byte], Int)] =
    val endAddr = addr + length - 1

    // Fast path: check cached region first
    if cachedRegionId != 0 && addr >= cachedRegionStart && endAddr < cachedRegionEnd then
      val offset = addr - cachedRegionStart + cachedRegionOffset
      if offset >= 0 && offset + length <= cachedRegionArray.length then
        return Some((cachedRegionArray, offset))

    // Slow path: find and cache the region
    findAndCacheRegion(addr, endAddr)

  /**
   * Finds the backing array and offset for a bulk write operation.
   * Returns None if the range spans multiple regions, is out of bounds, or is read-only.
   */
  private def findWritableRegionBulk(addr: Int, length: Int): Option[(Array[Byte], Int)] =
    val endAddr = addr + length - 1

    // Fast path: check cached region first (excluding RO region)
    if cachedRegionId > 1 && addr >= cachedRegionStart && endAddr < cachedRegionEnd then
      val offset = addr - cachedRegionStart + cachedRegionOffset
      if offset >= 0 && offset + length <= cachedRegionArray.length then
        return Some((cachedRegionArray, offset))

    // Slow path: find writable region
    findAndCacheWritableRegion(addr, endAddr)

  /**
   * Finds a region containing the address range and caches it.
   */
  private def findAndCacheRegion(addr: Int, endAddr: Int): Option[(Array[Byte], Int)] =
    // Check RO data region
    val roEnd = roStart + roData.length
    if addr >= roStart && endAddr < roEnd then
      val offset = addr - roStart
      cachedRegionId = 1
      cachedRegionStart = roStart
      cachedRegionEnd = roEnd
      cachedRegionArray = roData
      cachedRegionOffset = 0
      return Some((roData, offset))
    else if addr >= roStart && addr < (roStart + roRegionSize) then
      // Within RO region but may extend past actual data - can't use bulk
      return None

    // Check RW data + heap region
    val heapEndAddr = heapBaseInt + _heapSize.signed
    val rwEnd = math.max(rwStart + rwRegionSize, heapEndAddr)
    if addr >= rwStart && endAddr < rwEnd then
      val offset = addr - rwStart
      if offset >= 0 && offset + (endAddr - addr + 1) <= rwData.length then
        cachedRegionId = 2
        cachedRegionStart = rwStart
        cachedRegionEnd = rwEnd
        cachedRegionArray = rwData
        cachedRegionOffset = 0
        return Some((rwData, offset))
      else
        return None // Beyond buffer

    // Check stack region
    if addr >= stackLow && endAddr < stackHigh then
      val offset = addr - stackLow
      if offset >= 0 && offset + (endAddr - addr + 1) <= stack.length then
        cachedRegionId = 3
        cachedRegionStart = stackLow
        cachedRegionEnd = stackHigh
        cachedRegionArray = stack
        cachedRegionOffset = 0
        return Some((stack, offset))
      else
        return None

    // Check aux data region
    val auxEnd = auxStart + aux.length
    if addr >= auxStart && endAddr < auxEnd then
      val offset = addr - auxStart
      cachedRegionId = 4
      cachedRegionStart = auxStart
      cachedRegionEnd = auxEnd
      cachedRegionArray = aux
      cachedRegionOffset = 0
      return Some((aux, offset))

    None

  /**
   * Finds a writable region containing the address range.
   */
  private def findAndCacheWritableRegion(addr: Int, endAddr: Int): Option[(Array[Byte], Int)] =
    // Check RW data + heap region
    val heapEndAddr = heapBaseInt + _heapSize.signed
    val rwEnd = math.max(rwStart + rwRegionSize, heapEndAddr)
    if addr >= rwStart && endAddr < rwEnd then
      val offset = addr - rwStart
      if offset >= 0 && offset + (endAddr - addr + 1) <= rwData.length then
        cachedRegionId = 2
        cachedRegionStart = rwStart
        cachedRegionEnd = rwEnd
        cachedRegionArray = rwData
        cachedRegionOffset = 0
        return Some((rwData, offset))
      else
        return None

    // Check stack region
    if addr >= stackLow && endAddr < stackHigh then
      val offset = addr - stackLow
      if offset >= 0 && offset + (endAddr - addr + 1) <= stack.length then
        cachedRegionId = 3
        cachedRegionStart = stackLow
        cachedRegionEnd = stackHigh
        cachedRegionArray = stack
        cachedRegionOffset = 0
        return Some((stack, offset))
      else
        return None

    // Check aux data region (GP stack portion only)
    if addr >= gpStackLowInt && endAddr < gpStackBaseInt then
      val offset = addr - auxStart
      if offset >= 0 && offset + (endAddr - addr + 1) <= aux.length then
        cachedRegionId = 4
        cachedRegionStart = gpStackLowInt
        cachedRegionEnd = gpStackBaseInt
        cachedRegionArray = aux
        cachedRegionOffset = gpStackLowInt - auxStart
        return Some((aux, offset))

    None

  /**
   * Reads a single byte from the appropriate region.
   * Uses pre-computed boundaries for faster access.
   */
  private def readByte(address: UInt): Option[Byte] =
    readByteInt(address.signed)

  /**
   * Reads a single byte using primitive Int address.
   */
  private def readByteInt(addr: Int): Option[Byte] =
    // Fast path: check cached region
    if cachedRegionId != 0 && addr >= cachedRegionStart && addr < cachedRegionEnd then
      val offset = addr - cachedRegionStart + cachedRegionOffset
      if offset >= 0 && offset < cachedRegionArray.length then
        return Some(cachedRegionArray(offset))

    // Slow path: check all regions
    readByteSlowPath(addr)

  private def readByteSlowPath(addr: Int): Option[Byte] =
    // Check RO data region
    val roEnd = roStart + roData.length
    if addr >= roStart && addr < roEnd then
      cachedRegionId = 1
      cachedRegionStart = roStart
      cachedRegionEnd = roEnd
      cachedRegionArray = roData
      cachedRegionOffset = 0
      return Some(roData(addr - roStart))
    else if addr >= roStart && addr < (roStart + roRegionSize) then
      return Some(0.toByte)

    // Check RW data + heap region
    val heapEndAddr = heapBaseInt + _heapSize.signed
    val effectiveEnd = math.max(rwStart + rwRegionSize, heapEndAddr)
    if addr >= rwStart && addr < effectiveEnd then
      val offset = addr - rwStart
      if offset >= 0 && offset < rwData.length then
        cachedRegionId = 2
        cachedRegionStart = rwStart
        cachedRegionEnd = effectiveEnd
        cachedRegionArray = rwData
        cachedRegionOffset = 0
        return Some(rwData(offset))
      else
        return Some(0.toByte)

    // Check stack region
    if addr >= stackLow && addr < stackHigh then
      val offset = addr - stackLow
      if offset >= 0 && offset < stack.length then
        cachedRegionId = 3
        cachedRegionStart = stackLow
        cachedRegionEnd = stackHigh
        cachedRegionArray = stack
        cachedRegionOffset = 0
        return Some(stack(offset))
      else
        return Some(0.toByte)

    // Check aux data region
    val auxEnd = auxStart + auxRegionSize
    if addr >= auxStart && addr < auxEnd then
      val offset = addr - auxStart
      if offset >= 0 && offset < aux.length then
        cachedRegionId = 4
        cachedRegionStart = auxStart
        cachedRegionEnd = auxEnd
        cachedRegionArray = aux
        cachedRegionOffset = 0
        return Some(aux(offset))
      else
        return Some(0.toByte)

    Some(0.toByte)

  /**
   * Reads multiple bytes from memory using primitive Int address.
   */
  private def readBytesInt(addr: Int, length: Int): Option[Array[Byte]] =
    val result = new Array[Byte](length)
    var i = 0
    while i < length do
      readByteInt(addr + i) match
        case Some(b) => result(i) = b
        case None => return None
      i += 1
    Some(result)

  /**
   * Reads multiple bytes from memory.
   */
  private def readBytes(address: UInt, length: Int): Option[Array[Byte]] =
    readBytesInt(address.signed, length)

  /**
   * Writes a single byte to the appropriate region.
   * Uses pre-computed boundaries for faster access.
   */
  private def writeByte(address: UInt, value: Byte): Boolean =
    writeByteInt(address.signed, value)

  /**
   * Writes a single byte using primitive Int address.
   */
  private def writeByteInt(addr: Int, value: Byte): Boolean =
    // Fast path: check cached writable region
    if cachedRegionId > 1 && addr >= cachedRegionStart && addr < cachedRegionEnd then
      val offset = addr - cachedRegionStart + cachedRegionOffset
      if offset >= 0 && offset < cachedRegionArray.length then
        cachedRegionArray(offset) = value
        return true

    // Slow path: check all writable regions
    writeByteSlowPath(addr, value)

  private def writeByteSlowPath(addr: Int, value: Byte): Boolean =
    // RO data region - not writable
    val roEnd = roStart + roRegionSize
    if addr >= roStart && addr < roEnd then
      return false

    // Check RW data + heap region
    val heapEndAddr = heapBaseInt + _heapSize.signed
    val effectiveEnd = math.max(rwStart + rwRegionSize, heapEndAddr)
    if addr >= rwStart && addr < effectiveEnd then
      val offset = addr - rwStart
      if offset >= 0 && offset < rwData.length then
        cachedRegionId = 2
        cachedRegionStart = rwStart
        cachedRegionEnd = effectiveEnd
        cachedRegionArray = rwData
        cachedRegionOffset = 0
        rwData(offset) = value
        return true
      else
        return false

    // Check stack region
    if addr >= stackLow && addr < stackHigh then
      val offset = addr - stackLow
      if offset >= 0 && offset < stack.length then
        cachedRegionId = 3
        cachedRegionStart = stackLow
        cachedRegionEnd = stackHigh
        cachedRegionArray = stack
        cachedRegionOffset = 0
        stack(offset) = value
        return true
      else
        return false

    // Check aux data region (GP stack portion is writable at runtime)
    if addr >= gpStackLowInt && addr < gpStackBaseInt then
      val offset = addr - auxStart
      if offset >= 0 && offset < aux.length then
        cachedRegionId = 4
        cachedRegionStart = gpStackLowInt
        cachedRegionEnd = gpStackBaseInt
        cachedRegionArray = aux
        cachedRegionOffset = gpStackLowInt - auxStart
        aux(offset) = value
        return true

    false

  /**
   * Writes multiple bytes to memory using primitive Int address.
   */
  private def writeBytesInt(addr: Int, data: Array[Byte]): Boolean =
    var i = 0
    while i < data.length do
      if !writeByteInt(addr + i, data(i)) then
        return false
      i += 1
    true

  /**
   * Writes multiple bytes to memory.
   */
  private def writeBytes(address: UInt, data: Array[Byte]): Boolean =
    writeBytesInt(address.signed, data)

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
    // Invalidate cache if it was pointing to roData
    if cachedRegionId == 1 then
      cachedRegionId = 0

object BasicMemory:
  // Thread-local byte array pools to avoid per-operation allocations
  private val pool4: ThreadLocal[Array[Byte]] = ThreadLocal.withInitial(() => new Array[Byte](4))
  private val pool8: ThreadLocal[Array[Byte]] = ThreadLocal.withInitial(() => new Array[Byte](8))
  private val pool16: ThreadLocal[Array[Byte]] = ThreadLocal.withInitial(() => new Array[Byte](16))

  /**
   * Creates a new BasicMemory instance from a MemoryMap.
   */
  def create(memoryMap: MemoryMap): BasicMemory =
    val pageMap = initializePageMap(memoryMap, None, None)

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
   * Creates a BasicMemory with initial RO/RW data and heap size.
   *
   * @param memoryMap The memory map defining the memory layout
   * @param roData Read-only data section
   * @param rwData Read-write data section
   * @param initialHeapSize Initial heap size in bytes (includes page-aligned rwData + heapEmptyPages)
   * @param argumentData Optional argument/input data to place at InputStartAddress
   */
  def create(
    memoryMap: MemoryMap,
    roData: Array[Byte],
    rwData: Array[Byte],
    initialHeapSize: UInt = UInt(0),
    argumentData: Array[Byte] = Array.empty
  ): BasicMemory =
    val pageMap = initializePageMap(memoryMap, Some(roData.length), Some(argumentData.length))

    // Copy rwData into buffer
    val rwBuffer = new Array[Byte](memoryMap.rwDataSize.signed)
    System.arraycopy(rwData, 0, rwBuffer, 0, math.min(rwData.length, rwBuffer.length))

    // Create aux buffer and copy argument data to the input region
    val auxBuffer = new Array[Byte](memoryMap.auxDataSize.signed)
    if argumentData.nonEmpty then
      val inputStartAddress = PvmConstants.InputStartAddress.toLong
      val auxStartAddress = memoryMap.auxDataAddress.toLong
      val offset = (inputStartAddress - auxStartAddress).toInt
      if offset >= 0 && offset + argumentData.length <= auxBuffer.length then
        System.arraycopy(argumentData, 0, auxBuffer, offset, argumentData.length)

    new BasicMemory(
      _pageMap = pageMap,
      memoryMap = memoryMap,
      roData = roData,
      rwData = rwBuffer,
      stack = new Array[Byte](memoryMap.stackSize.signed),
      aux = auxBuffer,
      _heapSize = initialHeapSize,
      isDirty = false
    )

  /**
   * Initializes the PageMap with correct page access permissions.
   *
   * IMPORTANT: Only explicitly mapped regions are accessible. All other addresses
   * (including gaps between regions) are inaccessible and will cause a segfault.
   *
   * @param memoryMap The memory map defining region boundaries
   * @param roDataActualSize Optional actual size of RO data (may be smaller than roDataSize)
   * @param argumentDataSize Optional size of argument/input data in the aux region
   */
  private def initializePageMap(
    memoryMap: MemoryMap,
    roDataActualSize: Option[Int],
    argumentDataSize: Option[Int] = None
  ): PageMap =
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

    if memoryMap.auxDataSize.signed > 0 then
      val inputStartAddress = PvmConstants.InputStartAddress

      // Input/argument data region - READ_ONLY (only the actual data, not the entire region)
      argumentDataSize match
        case Some(argSize) if argSize > 0 =>
          // Only map the actual argument data size, page-aligned
          val alignedArgSize = AlignmentOps.alignUp(UInt(argSize), memoryMap.pageSize)
          mappings += ((inputStartAddress, alignedArgSize, PageAccess.ReadOnly))
        case _ =>
        // No argument data - don't map anything in the input region

    PageMap.create(mappings.toList, memoryMap.pageSize)
