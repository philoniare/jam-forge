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

  /**
   * Build the region table + backing bytes for `instance`'s CURRENT memory
   * state
   */
  def describe(instance: InterpretedInstance): Described =
    val memoryMap = instance.module.memoryMap
    val pageMap = instance.basicMemory.pageMap
    val pageSize = memoryMap.pageSize

    val regions = scala.collection.mutable.ArrayBuffer.empty[RegionDesc]
    val backing = scala.collection.mutable.ArrayBuffer.empty[Byte]

    def appendRegion(base: UInt, len: Int, writable: Boolean): Unit =
      if len > 0 then
        val bufOffset = backing.length
        instance.basicMemory.getMemorySlice(base, len) match
          case MemoryResult.Success(bytes) =>
            backing ++= bytes
            regions += RegionDesc(base.toLong & 0xFFFFFFFFL, len.toLong, bufOffset.toLong, writable)
          case _ =>
            () // unreadable despite being a declared region: skip (defensive; should not happen)

    // RO data — ReadOnly (initializePageMap: mappings += (roDataAddress, roSize, ReadOnly)).
    appendRegion(memoryMap.roDataAddress, memoryMap.roDataSize.signed, writable = false)

    // RW data — ReadWrite
    val rwEffectiveLen = math.max(memoryMap.rwDataSize.signed, instance.basicMemory.heapSize.signed)
    val rwEffectiveLenAligned = AlignmentOps.alignUp(rwEffectiveLen, pageSize.signed)
    appendRegion(memoryMap.rwDataAddress, rwEffectiveLenAligned, writable = true)

    // Stack — ReadWrite.
    appendRegion(memoryMap.stackAddressLow, memoryMap.stackSize.signed, writable = true)

    // Aux/args — ReadOnly, only the page-aligned prefix actually mapped.
    val auxMappedLen = mappedPrefixLength(pageMap, memoryMap.auxDataAddress, memoryMap.auxDataSize.signed, pageSize)
    appendRegion(memoryMap.auxDataAddress, auxMappedLen, writable = false)

    Described(regions.toArray, backing.toArray, pageSize)

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
