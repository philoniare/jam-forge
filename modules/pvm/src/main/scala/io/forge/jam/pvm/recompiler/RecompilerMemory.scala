package io.forge.jam.pvm.recompiler

import spire.math.UInt
import io.forge.jam.pvm.{AlignmentOps, MemoryResult}
import io.forge.jam.pvm.engine.InterpretedInstance

/**
 * Describes an interpreter memory map (`MemoryMap` + its `PageMap`) as the
 * native recompiler's region table + one shared backing buffer
 */
object RecompilerMemory:

  /** One guest-memory region ready for `PvmRecompiler.Region` marshalling:
    * `[base, base+len)`, backed by `bufOffset` into the packed backing array
    * returned alongside it by `describe`. */
  final case class RegionDesc(base: Long, len: Long, bufOffset: Long, writable: Boolean)
  final case class Described(regions: Array[RegionDesc], backing: Array[Byte], pageSize: UInt):
    /** `log2(pageSize)`, the `page_shift` FFI parameter — pageSize is always a
      * power of two per `MemoryMap.Builder.build`'s own validation. */
    def pageShift: Int = java.lang.Integer.numberOfTrailingZeros(pageSize.signed)

  final case class DescribedWithHeap(
    described: Described,
    heapRegionIndex: Int,
    heapBase: Long,
    initialHeapEnd: Long,
    maxHeapSize: Long
  )

  /**
   * Build the region table + backing bytes for `instance`'s CURRENT memory
   * state
   */
  def describe(instance: InterpretedInstance): Described =
    describeInternal(instance, heapSlackBytes = 0)._1

  val MaxHeapSlackBytes: Long = 256L * 1024 * 1024

  def describeWithHeapSlack(instance: InterpretedInstance): DescribedWithHeap =
    val memoryMap = instance.module.memoryMap
    val pageSize = memoryMap.pageSize.signed
    val currentHeapSize = instance.basicMemory.heapSize.signed.toLong & 0xFFFFFFFFL
    val maxHeapSize = memoryMap.maxHeapSize.signed.toLong & 0xFFFFFFFFL
    val room = math.max(0L, maxHeapSize - currentHeapSize)
    val slackBytes = AlignmentOps.alignUp(math.min(room, MaxHeapSlackBytes), pageSize.toLong)

    val (described, heapRegionIndex) = describeInternal(instance, slackBytes)
    val heapBase = memoryMap.heapBase.toLong & 0xFFFFFFFFL
    val initialHeapEnd = heapBase + currentHeapSize // mirrors basicMemory.heapEnd == memoryMap.heapBase + _heapSize
    DescribedWithHeap(described, heapRegionIndex, heapBase, initialHeapEnd, maxHeapSize)

  private def describeInternal(instance: InterpretedInstance, heapSlackBytes: Long): (Described, Int) =
    val memoryMap = instance.module.memoryMap
    val pageMap = instance.basicMemory.pageMap
    val pageSize = memoryMap.pageSize

    final case class PendingRegion(base: UInt, bytes: Array[Byte], writable: Boolean, extraSlack: Long)
    val pending = scala.collection.mutable.ArrayBuffer.empty[PendingRegion]
    var heapRegionIndex = -1

    def appendRegion(base: UInt, len: Int, writable: Boolean, extraBackingSlack: Long = 0L): Unit =
      if len > 0 then
        instance.basicMemory.getMemorySlice(base, len) match
          case MemoryResult.Success(bytes) =>
            pending += PendingRegion(base, bytes, writable, extraBackingSlack)
          case _ =>
            () // unreadable despite being a declared region: skip (defensive; should not happen)

    // RO data — ReadOnly (initializePageMap: mappings += (roDataAddress, roSize, ReadOnly)).
    appendRegion(memoryMap.roDataAddress, memoryMap.roDataSize.signed, writable = false)

    // RW data — ReadWrite
    val rwEffectiveLen = math.max(memoryMap.rwDataSize.signed, instance.basicMemory.heapSize.signed)
    val rwEffectiveLenAligned = AlignmentOps.alignUp(rwEffectiveLen, pageSize.signed)
    heapRegionIndex = pending.length
    appendRegion(memoryMap.rwDataAddress, rwEffectiveLenAligned, writable = true, extraBackingSlack = heapSlackBytes)
    if pending.length <= heapRegionIndex then heapRegionIndex = -1 // rwEffectiveLenAligned was 0 (unmapped): no heap region exists

    // Stack — ReadWrite.
    appendRegion(memoryMap.stackAddressLow, memoryMap.stackSize.signed, writable = true)

    // Aux/args — ReadOnly, only the page-aligned prefix actually mapped.
    val auxMappedLen = mappedPrefixLength(pageMap, memoryMap.auxDataAddress, memoryMap.auxDataSize.signed, pageSize)
    appendRegion(memoryMap.auxDataAddress, auxMappedLen, writable = false)

    val totalBackingSize = pending.foldLeft(0L)((acc, p) => acc + p.bytes.length + p.extraSlack)
    val backing = new Array[Byte](totalBackingSize.toInt)
    val regions = new Array[RegionDesc](pending.length)
    var offset = 0L
    pending.indices.foreach { i =>
      val p = pending(i)
      System.arraycopy(p.bytes, 0, backing, offset.toInt, p.bytes.length)
      regions(i) = RegionDesc(p.base.toLong & 0xFFFFFFFFL, p.bytes.length.toLong, offset, p.writable)
      offset += p.bytes.length.toLong + p.extraSlack // slack bytes stay zero-filled (Array[Byte] default)
    }

    (Described(regions, backing, pageSize), heapRegionIndex)

  private def mappedPrefixLength(pageMap: io.forge.jam.pvm.memory.PageMap, base: UInt, maxLen: Int, pageSize: UInt): Int =
    if maxLen <= 0 then 0
    else
      val pageSizeInt = pageSize.signed
      val startPageIndex = UInt(base.signed >>> java.lang.Integer.numberOfTrailingZeros(pageSizeInt))
      val maxPages = (maxLen + pageSizeInt - 1) / pageSizeInt
      var pages = 0
      var continue = true
      while continue && pages < maxPages do
        if pageMap.isPageReadable(UInt(startPageIndex.signed + pages)) then pages += 1
        else continue = false
      math.min(pages * pageSizeInt, maxLen)
