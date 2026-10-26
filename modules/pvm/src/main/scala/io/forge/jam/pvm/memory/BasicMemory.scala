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
  private val pageShift: Int = java.lang.Integer.numberOfTrailingZeros(memoryMap.pageSize.signed)
  private val rdWinStart = Array.fill(BasicMemory.WindowSlots)(Long.MaxValue)
  private val rdWinEnd = new Array[Long](BasicMemory.WindowSlots)
  private val rdWinArr = new Array[Array[Byte]](BasicMemory.WindowSlots)
  private val rdWinOff = new Array[Int](BasicMemory.WindowSlots)
  private val rdWinVer = Array.fill(BasicMemory.WindowSlots)(Int.MinValue)
  private val wrWinStart = Array.fill(BasicMemory.WindowSlots)(Long.MaxValue)
  private val wrWinEnd = new Array[Long](BasicMemory.WindowSlots)
  private val wrWinArr = new Array[Array[Byte]](BasicMemory.WindowSlots)
  private val wrWinOff = new Array[Int](BasicMemory.WindowSlots)
  private val wrWinVer = Array.fill(BasicMemory.WindowSlots)(Int.MinValue)

  // Slot boundaries: regions are laid out in this order, so three unsigned compares pick the slot.
  private val rwStartU: Long = rwStart.toLong & 0xffffffffL
  private val stackLowU: Long = stackLow.toLong & 0xffffffffL
  private val auxStartU: Long = auxStart.toLong & 0xffffffffL

  /** Window slot for an address: 0 = roData, 1 = rwData/heap, 2 = stack, 3 = aux. Only selects a slot; every hit is
    * still checked against the window's own bounds and version.
    */
  private inline def slotOf(ua64: Long): Int =
    if ua64 < rwStartU then 0 else if ua64 < stackLowU then 1 else if ua64 < auxStartU then 2 else 3

  private def invalidateWindows(): Unit =
    var i = 0
    while i < BasicMemory.WindowSlots do
      rdWinStart(i) = Long.MaxValue; rdWinEnd(i) = 0L; rdWinArr(i) = null; rdWinVer(i) = Int.MinValue
      wrWinStart(i) = Long.MaxValue; wrWinEnd(i) = 0L; wrWinArr(i) = null; wrWinVer(i) = Int.MinValue
      i += 1

  /** Fill the read or write window around `addr`. Preconditions: the permission
    * check for [addr, addr+width) just passed AND findRegionBulk /
    * findWritableRegionBulk just succeeded (so the cachedRegion* fields describe
    * the containing region). Never widens beyond what those checks guarantee.
    */
  private def fillWindow(addr: Int, writable: Boolean): Unit =
    if cachedRegionId == 0 then return
    val run = _pageMap.accessibleRun(addr >>> pageShift, writable)
    val runStartAddr = (run >>> 32) << pageShift
    val runEndAddr = (run & 0xffffffffL) << pageShift
    if runEndAddr <= runStartAddr then return

    val regStart = cachedRegionStart.toLong & 0xffffffffL
    val regEnd = cachedRegionEnd.toLong & 0xffffffffL
    // Address just past the last byte the backing array can serve for this region
    val arrEnd = regStart - cachedRegionOffset + cachedRegionArray.length
    val s = math.max(regStart, runStartAddr)
    val e = math.min(math.min(regEnd, arrEnd), runEndAddr)
    if e <= s then return

    val slot = slotOf(addr.toLong & 0xffffffffL)
    if writable then
      wrWinStart(slot) = s; wrWinEnd(slot) = e
      wrWinArr(slot) = cachedRegionArray
      wrWinOff(slot) = (s - regStart).toInt + cachedRegionOffset
      wrWinVer(slot) = _pageMap.version
    else
      rdWinStart(slot) = s; rdWinEnd(slot) = e
      rdWinArr(slot) = cachedRegionArray
      rdWinOff(slot) = (s - regStart).toInt + cachedRegionOffset
      rdWinVer(slot) = _pageMap.version

  /** Loaded value for the most recent successful fast load. Holds any 64-bit pattern. */
  private var _fastValue: Long = 0L

  /** Faulting page address for the most recent fast op that returned FastSegfault. */
  private var _fastFaultPage: UInt = UInt(0)

  /** The value produced by the most recent successful fast load (any bit pattern). */
  def fastValue: Long = _fastValue

  /** The page-aligned faulting address for the most recent FastSegfault. */
  def fastFaultPage: UInt = _fastFaultPage

  /**
   * Allocation-free unsigned load
   */
  def loadUnsignedFast(address: Int, width: Int): Int =
    val ua64 = address.toLong & 0xffffffffL
    val slot = slotOf(ua64)
    val winStart = rdWinStart(slot)
    if rdWinVer(slot) == _pageMap.version && ua64 >= winStart && ua64 + width <= rdWinEnd(slot) then
      val arr = rdWinArr(slot)
      val offset = (ua64 - winStart).toInt + rdWinOff(slot)
      val v: Long = width match
        case 1 => arr(offset) & 0xffL
        case 2 =>
          val lo = arr(offset) & 0xff
          val hi = arr(offset + 1) & 0xff
          ((hi << 8) | lo).toLong & 0xffffL
        case 4 =>
          val b0 = arr(offset) & 0xff
          val b1 = arr(offset + 1) & 0xff
          val b2 = arr(offset + 2) & 0xff
          val b3 = arr(offset + 3) & 0xff
          ((b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)).toLong) & 0xffffffffL
        case _ => BasicMemory.LongLE.get(arr, offset)
      _fastValue = v
      return BasicMemory.FastOk

    val ua = UInt(address)
    if !_pageMap.isReadableFast(ua, width) then
      _fastFaultPage = _pageMap.alignToPageStart(_pageMap.lastFailPageAddress)
      return BasicMemory.FastSegfault

    // Page check passed: read the bytes from the backing region.
    val bulkOffset = findRegionBulk(address, width)
    if bulkOffset >= 0 then
        val arr = _bulkArr
        val offset = bulkOffset
        fillWindow(address, writable = false)
        val v: Long = width match
          case 1 => (arr(offset) & 0xffL)
          case 2 =>
            val lo = arr(offset) & 0xff
            val hi = arr(offset + 1) & 0xff
            ((hi << 8) | lo).toLong & 0xffffL
          case 4 =>
            val b0 = arr(offset) & 0xff
            val b1 = arr(offset + 1) & 0xff
            val b2 = arr(offset + 2) & 0xff
            val b3 = arr(offset + 3) & 0xff
            ((b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)).toLong) & 0xffffffffL
          case _ => BasicMemory.LongLE.get(arr, offset)
        _fastValue = v
        BasicMemory.FastOk
    else
        // Fall back to byte-by-byte (identical to the MemoryResult fallback).
        readBytesInt(address, width) match
          case Some(bytes) =>
            var value = 0L
            var i = 0
            while i < width do
              value |= (bytes(i) & 0xffL) << (i * 8)
              i += 1
            _fastValue = value
            BasicMemory.FastOk
          case None => BasicMemory.FastOutOfBounds

  /**
   * Allocation-free store of the low `width` bytes of `value` (little-endian)
   */
  def storeFast(address: Int, value: Long, width: Int): Int =
    val ua64 = address.toLong & 0xffffffffL
    val slot = slotOf(ua64)
    val winStart = wrWinStart(slot)
    if wrWinVer(slot) == _pageMap.version && ua64 >= winStart && ua64 + width <= wrWinEnd(slot) then
      val arr = wrWinArr(slot)
      val offset = (ua64 - winStart).toInt + wrWinOff(slot)
      if width == 8 then BasicMemory.LongLE.set(arr, offset, value)
      else
        var i = 0
        while i < width do
          arr(offset + i) = ((value >> (i * 8)) & 0xff).toByte
          i += 1
      isDirty = true
      return BasicMemory.FastOk

    val ua = UInt(address)
    if !_pageMap.isWritableFast(ua, width) then
      _fastFaultPage = _pageMap.alignToPageStart(_pageMap.lastFailPageAddress)
      return BasicMemory.FastSegfault

    val bulkOffset = findWritableRegionBulk(address, width)
    if bulkOffset >= 0 then
        val arr = _bulkArr
        val offset = bulkOffset
        fillWindow(address, writable = true)
        var i = 0
        while i < width do
          arr(offset + i) = ((value >> (i * 8)) & 0xff).toByte
          i += 1
        isDirty = true
        BasicMemory.FastOk
    else
        // Fall back to byte-by-byte using a pooled scratch array.
        val bytes =
          if width <= 4 then BasicMemory.pool4.get()
          else BasicMemory.pool8.get()
        var i = 0
        while i < width do
          bytes(i) = ((value >> (i * 8)) & 0xff).toByte
          i += 1
        if writeBytesIntRange(address, bytes, width) then
          isDirty = true
          BasicMemory.FastOk
        else
          BasicMemory.FastOutOfBounds

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
        val bulkOffset = findRegionBulk(addr, 2)
        if bulkOffset >= 0 then
            val arr = _bulkArr
            val offset = bulkOffset
            val lo = arr(offset) & 0xff
            val hi = arr(offset + 1) & 0xff
            MemoryResult.Success(UShort(((hi << 8) | lo).toShort))
        else
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
        val bulkOffset = findRegionBulk(addr, 4)
        if bulkOffset >= 0 then
            val arr = _bulkArr
            val offset = bulkOffset
            // Direct byte extraction (little-endian)
            val b0 = arr(offset) & 0xff
            val b1 = arr(offset + 1) & 0xff
            val b2 = arr(offset + 2) & 0xff
            val b3 = arr(offset + 3) & 0xff
            val value = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
            MemoryResult.Success(UInt(value))
        else
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
        val bulkOffset = findRegionBulk(addr, 8)
        if bulkOffset >= 0 then
            // Use ByteBuffer for efficient little-endian long read
            val value = ByteBuffer.wrap(_bulkArr, bulkOffset, 8)
              .order(ByteOrder.LITTLE_ENDIAN)
              .getLong()
            MemoryResult.Success(ULong(value))
        else
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
        val bulkOffset = findWritableRegionBulk(addr, 2)
        if bulkOffset >= 0 then
            val arr = _bulkArr
            val offset = bulkOffset
            arr(offset) = (v & 0xff).toByte
            arr(offset + 1) = ((v >> 8) & 0xff).toByte
            isDirty = true
            MemoryResult.Success(())
        else
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
        val bulkOffset = findWritableRegionBulk(addr, 4)
        if bulkOffset >= 0 then
            val arr = _bulkArr
            val offset = bulkOffset
            // Direct byte write (little-endian)
            arr(offset) = (v & 0xff).toByte
            arr(offset + 1) = ((v >> 8) & 0xff).toByte
            arr(offset + 2) = ((v >> 16) & 0xff).toByte
            arr(offset + 3) = ((v >> 24) & 0xff).toByte
            isDirty = true
            MemoryResult.Success(())
        else
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
        val bulkOffset = findWritableRegionBulk(addr, 8)
        if bulkOffset >= 0 then
            // Use ByteBuffer for efficient little-endian long write
            ByteBuffer.wrap(_bulkArr, bulkOffset, 8)
              .order(ByteOrder.LITTLE_ENDIAN)
              .putLong(v)
            isDirty = true
            MemoryResult.Success(())
        else
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
        val bulkOffset = findRegionBulk(addr, length)
        if bulkOffset >= 0 then
            // Direct array copy
            val result = new Array[Byte](length)
            System.arraycopy(_bulkArr, bulkOffset, result, 0, length)
            MemoryResult.Success(result)
        else
            // Fall back to byte-by-byte
            readBytesInt(addr, length) match
              case Some(arr) => MemoryResult.Success(arr)
              case None => MemoryResult.OutOfBounds(address)

  def getMemorySliceInto(address: UInt, dest: Array[Byte], destOffset: Int, length: Int): MemoryResult[Unit] =
    if length == 0 then return MemoryResult.Success(())

    checkReadable(address, length) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        val bulkOffset = findRegionBulk(addr, length)
        if bulkOffset >= 0 then
            System.arraycopy(_bulkArr, bulkOffset, dest, destOffset, length)
            MemoryResult.Success(())
        else
            var i = 0
            while i < length do
              readByteInt(addr + i) match
                case Some(b) => dest(destOffset + i) = b
                case None    => return MemoryResult.OutOfBounds(address)
              i += 1
            MemoryResult.Success(())

  override def setMemorySlice(address: UInt, data: Array[Byte]): MemoryResult[Unit] =
    if data.isEmpty then return MemoryResult.Success(())

    checkWritable(address, data.length) match
      case Some(err) => err
      case None =>
        val addr = address.signed
        // Try optimized bulk write if within single region
        val bulkOffset = findWritableRegionBulk(addr, data.length)
        if bulkOffset >= 0 then
            // Direct array copy
            System.arraycopy(data, 0, _bulkArr, bulkOffset, data.length)
            isDirty = true
            MemoryResult.Success(())
        else
            // Fall back to byte-by-byte
            if writeBytesInt(addr, data) then
              isDirty = true
              MemoryResult.Success(())
            else
              MemoryResult.OutOfBounds(address)

  // ============================================================================
  // Heap Operations (sbrk)
  // ============================================================================

  /** Bytes in [dirtyFrom, dirtyTo) of a reused heap buffer may hold a previous owner's data; dirtyFrom is never
    * below the heap end.
    */
  private var dirtyFrom: Int = 0
  private var dirtyTo: Int = 0

  private def zeroDirty(upTo: Int): Unit =
    if upTo > dirtyFrom && dirtyTo > dirtyFrom then
      val end = math.min(upTo, dirtyTo)
      java.util.Arrays.fill(rwData, dirtyFrom, end, 0.toByte)
      dirtyFrom = end

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
    invalidateWindows()

    // The heap lives in the rwData buffer starting at heapBase relative to rwDataAddress. A grow past the buffer takes
    // the large buffer a finished invocation released, if there is one, else grows the buffer by at least half, so a
    // run of grow_heap calls copies linearly, not quadratically. Bytes past the heap end are zero and stay
    // unreachable: every access is bounded by the page map and heapEnd, never by the buffer length.
    val requiredBufferSize = newHeapSize.signed
    zeroDirty(requiredBufferSize)
    if requiredBufferSize > rwData.length then
      val pooled = BasicMemory.takeSpare(requiredBufferSize)
      val oldLength = rwData.length
      val newBuffer =
        if pooled != null then pooled.buffer
        else
          val cap = math.min(memoryMap.maxHeapSize.toLong, BasicMemory.MaxBufferSize.toLong)
          val grown = math.min(oldLength.toLong * 3 / 2, cap).toInt
          new Array[Byte](math.max(requiredBufferSize, grown))
      System.arraycopy(rwData, 0, newBuffer, 0, oldLength)
      rwData = newBuffer
      // The previous owner's bytes past the copy are zeroed as the heap reaches them, not all up front.
      if pooled != null && pooled.dirtyEnd > oldLength then
        dirtyFrom = oldLength
        dirtyTo = pooled.dirtyEnd
        zeroDirty(requiredBufferSize)

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
  private var _bulkArr: Array[Byte] = null

  /**
   * Finds the backing array and offset for a bulk read operation.
   * Returns the offset into `_bulkArr`, or -1 if the range spans multiple
   * regions or is out of bounds.
   */
  private def findRegionBulk(addr: Int, length: Int): Int =
    val endAddr = addr + length - 1

    // Fast path: check cached region first
    if cachedRegionId != 0 && addr >= cachedRegionStart && endAddr < cachedRegionEnd then
      val offset = addr - cachedRegionStart + cachedRegionOffset
      if offset >= 0 && offset + length <= cachedRegionArray.length then
        _bulkArr = cachedRegionArray
        return offset

    // Slow path: find and cache the region
    findAndCacheRegion(addr, endAddr)

  /**
   * Finds the backing array and offset for a bulk write operation.
   * Returns the offset into `_bulkArr`, or -1 if the range spans multiple
   * regions, is out of bounds, or is read-only.
   */
  private def findWritableRegionBulk(addr: Int, length: Int): Int =
    val endAddr = addr + length - 1

    // Fast path: check cached region first (excluding RO region)
    if cachedRegionId > 1 && addr >= cachedRegionStart && endAddr < cachedRegionEnd then
      val offset = addr - cachedRegionStart + cachedRegionOffset
      if offset >= 0 && offset + length <= cachedRegionArray.length then
        _bulkArr = cachedRegionArray
        return offset

    // Slow path: find writable region
    findAndCacheWritableRegion(addr, endAddr)

  /**
   * Finds a region containing the address range and caches it.
   */
  private def findAndCacheRegion(addr: Int, endAddr: Int): Int =
    // Check RO data region
    val roEnd = roStart + roData.length
    if addr >= roStart && endAddr < roEnd then
      val offset = addr - roStart
      cachedRegionId = 1
      cachedRegionStart = roStart
      cachedRegionEnd = roEnd
      cachedRegionArray = roData
      cachedRegionOffset = 0
      _bulkArr = roData
      return offset
    else if addr >= roStart && addr < (roStart + roRegionSize) then
      // Within RO region but may extend past actual data - can't use bulk
      return -1

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
        _bulkArr = rwData
      return offset
      else
        return -1 // Beyond buffer

    // Check stack region
    if addr >= stackLow && endAddr < stackHigh then
      val offset = addr - stackLow
      if offset >= 0 && offset + (endAddr - addr + 1) <= stack.length then
        cachedRegionId = 3
        cachedRegionStart = stackLow
        cachedRegionEnd = stackHigh
        cachedRegionArray = stack
        cachedRegionOffset = 0
        _bulkArr = stack
      return offset
      else
        return -1

    // Check aux data region
    val auxEnd = auxStart + aux.length
    if addr >= auxStart && endAddr < auxEnd then
      val offset = addr - auxStart
      cachedRegionId = 4
      cachedRegionStart = auxStart
      cachedRegionEnd = auxEnd
      cachedRegionArray = aux
      cachedRegionOffset = 0
      _bulkArr = aux
      return offset

    -1

  /**
   * Finds a writable region containing the address range.
   */
  private def findAndCacheWritableRegion(addr: Int, endAddr: Int): Int =
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
        _bulkArr = rwData
      return offset
      else
        return -1

    // Check stack region
    if addr >= stackLow && endAddr < stackHigh then
      val offset = addr - stackLow
      if offset >= 0 && offset + (endAddr - addr + 1) <= stack.length then
        cachedRegionId = 3
        cachedRegionStart = stackLow
        cachedRegionEnd = stackHigh
        cachedRegionArray = stack
        cachedRegionOffset = 0
        _bulkArr = stack
      return offset
      else
        return -1

    // Check aux data region (GP stack portion only)
    if addr >= gpStackLowInt && endAddr < gpStackBaseInt then
      val offset = addr - auxStart
      if offset >= 0 && offset + (endAddr - addr + 1) <= aux.length then
        cachedRegionId = 4
        cachedRegionStart = gpStackLowInt
        cachedRegionEnd = gpStackBaseInt
        cachedRegionArray = aux
        cachedRegionOffset = gpStackLowInt - auxStart
        _bulkArr = aux
      return offset

    -1

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
   * Writes the first `length` bytes of `data` to memory. 
   * Short-circuits on the first failing byte, matching
   * `writeBytesInt`.
   */
  private def writeBytesIntRange(addr: Int, data: Array[Byte], length: Int): Boolean =
    var i = 0
    while i < length do
      if !writeByteInt(addr + i, data(i)) then
        return false
      i += 1
    true

  /**
   * Marks memory as dirty (needs reset before reuse).
   */
  def markDirty(): Unit = isDirty = true

  /** Ends this memory's use: hands a large heap buffer to the next invocation that grows its heap, so it skips the
    * allocation, zeroing and copying of its own. Any later access misses the (now empty) buffer instead of touching it.
    */
  def release(): Unit =
    val buf = rwData
    // Writes only land below the RW region end or the heap end, whichever is higher.
    val written = math.max(math.max(rwRegionSize.toLong, _heapSize.toLong), dirtyTo.toLong)
    val dirtyEnd = math.min(buf.length.toLong, written).toInt
    rwData = Array.emptyByteArray
    dirtyFrom = 0
    dirtyTo = 0
    cachedRegionId = 0
    invalidateWindows()
    BasicMemory.offerSpare(buf, dirtyEnd)

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
    invalidateWindows()

object BasicMemory:
  // Status sentinels for the allocation-free fast-path load/store API.
  inline val FastOk = 0
  inline val FastSegfault = -1
  inline val FastOutOfBounds = -2

  private final val WindowSlots = 4

  /** Largest heap buffer sbrk over-allocates to; a larger heap still gets exactly the size it needs. */
  private inline val MaxBufferSize = Int.MaxValue - 8

  /** A released heap buffer; bytes at `dirtyEnd` and beyond are zero. */
  private final class Spare(val buffer: Array[Byte], val dirtyEnd: Int)

  /** Released heap buffers between these sizes are kept, one at a time (the largest), for the next large grow. */
  private inline val MinSpareBytes = 1 << 20
  private inline val MaxSpareBytes = 256 << 20

  private val spare = new java.util.concurrent.atomic.AtomicReference[Spare](null)

  /** The kept buffer if it holds `required` bytes, else null (and it stays kept). */
  private def takeSpare(required: Int): Spare =
    val s = spare.get()
    if s == null || s.buffer.length < required || !spare.compareAndSet(s, null) then null else s

  private def offerSpare(buffer: Array[Byte], dirtyEnd: Int): Unit =
    if buffer.length >= MinSpareBytes && buffer.length <= MaxSpareBytes then
      var done = false
      while !done do
        val cur = spare.get()
        done = (cur != null && cur.buffer.length >= buffer.length) ||
          spare.compareAndSet(cur, new Spare(buffer, dirtyEnd))

  /** Little-endian 8-byte view of a byte array (unaligned access allowed). */
  private val LongLE: java.lang.invoke.VarHandle =
    java.lang.invoke.MethodHandles.byteArrayViewVarHandle(classOf[Array[Long]], java.nio.ByteOrder.LITTLE_ENDIAN)

  // Thread-local byte array pools to avoid per-operation allocations
  private val pool4: ThreadLocal[Array[Byte]] = ThreadLocal.withInitial(() => new Array[Byte](4))
  private val pool8: ThreadLocal[Array[Byte]] = ThreadLocal.withInitial(() => new Array[Byte](8))

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
    val inputStartAddress = PvmConstants.InputStartAddress.toLong
    val auxStartAddress = memoryMap.auxDataAddress.toLong
    val argOffset = (inputStartAddress - auxStartAddress).toInt
    val alignedArgLen =
      if argumentData.isEmpty then 0
      else AlignmentOps.alignUp(UInt(argumentData.length), memoryMap.pageSize).signed
    val auxBackingSize =
      if argOffset >= 0 && alignedArgLen > 0 then
        math.min(argOffset + alignedArgLen, memoryMap.auxDataSize.signed)
      else 0
    val auxBuffer = new Array[Byte](auxBackingSize)
    if argumentData.nonEmpty then
      if argOffset >= 0 && argOffset + argumentData.length <= auxBuffer.length then
        System.arraycopy(argumentData, 0, auxBuffer, argOffset, argumentData.length)

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
    argumentDataSize: Option[Int]
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
