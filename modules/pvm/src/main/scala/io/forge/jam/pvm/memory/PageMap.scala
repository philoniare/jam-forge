package io.forge.jam.pvm.memory

import spire.math.UInt
import scala.collection.mutable.ArrayBuffer

/**
 * Page access permissions.
 *
 * - ReadOnly: Page can be read but not written (R)
 * - ReadWrite: Page can be read and written (R/W)
 * - Inaccessible pages are represented by absence in the bitset
 */
enum PageAccess:
  case ReadOnly
  case ReadWrite

  def isReadable: Boolean = true
  def isWritable: Boolean = this == ReadWrite

/**
 * Per-page access control using efficient bitset operations.
 *
 * Uses two parallel bit arrays (readableBits and writableBits) where each bit
 * represents one memory page. This allows O(1) access checking with minimal
 * memory overhead.
 *
 * @param pageSize The size of each memory page in bytes (typically 4096)
 */
final class PageMap(val pageSize: UInt):
  private val pageSizeShift: Int = java.lang.Integer.numberOfTrailingZeros(pageSize.signed)

  private val BitsPerWord = 64

  // Bitset storage: each Long represents 64 pages
  private var readableBits: Array[Long] = Array.empty
  private var writableBits: Array[Long] = Array.empty
  private var maxPageIndex: UInt = UInt(0)

  /**
   * Calculates the number of pages needed to access a memory range.
   */
  def numberOfPagesToAccess(address: UInt, length: Int): Int =
    if length == 0 then 0
    else
      val startPage = address.signed >>> pageSizeShift
      val endAddress = address.signed + length - 1
      val endPage = endAddress >>> pageSizeShift
      endPage - startPage + 1

  /**
   * Ensures the bitset arrays have capacity for the given page index.
   */
  private def ensureCapacity(pageIndex: UInt): Unit =
    if pageIndex.signed >= maxPageIndex.signed then
      maxPageIndex = UInt(pageIndex.signed + 1)
      val wordsNeeded = ((maxPageIndex.signed + 63) / 64)
      val currentSize = readableBits.length

      if wordsNeeded > currentSize then
        readableBits = readableBits.padTo(wordsNeeded, 0L)
        writableBits = writableBits.padTo(wordsNeeded, 0L)

  /**
   * Checks if a single page is readable.
   */
  def isPageReadable(pageIndex: UInt): Boolean =
    val wordIndex = pageIndex.signed / 64
    if wordIndex >= readableBits.length then false
    else
      val bitIndex = pageIndex.signed % 64
      (readableBits(wordIndex) & (1L << bitIndex)) != 0L

  /**
   * Checks if a single page is writable.
   */
  def isPageWritable(pageIndex: UInt): Boolean =
    val wordIndex = pageIndex.signed / 64
    if wordIndex >= writableBits.length then false
    else
      val bitIndex = pageIndex.signed % 64
      (writableBits(wordIndex) & (1L << bitIndex)) != 0L

  /**
   * Sets access for a single page.
   */
  def setPageAccess(pageIndex: UInt, access: PageAccess): Unit =
    ensureCapacity(pageIndex)
    val wordIndex = pageIndex.signed / 64
    val bitIndex = pageIndex.signed % 64
    val mask = 1L << bitIndex

    access match
      case PageAccess.ReadOnly =>
        readableBits(wordIndex) = readableBits(wordIndex) | mask
        writableBits(wordIndex) = writableBits(wordIndex) & ~mask
      case PageAccess.ReadWrite =>
        readableBits(wordIndex) = readableBits(wordIndex) | mask
        writableBits(wordIndex) = writableBits(wordIndex) | mask

  /**
   * Removes all access from a single page.
   */
  def removePageAccess(pageIndex: UInt): Unit =
    val wordIndex = pageIndex.signed / 64
    if wordIndex < readableBits.length then
      val bitIndex = pageIndex.signed % 64
      val mask = ~(1L << bitIndex)
      readableBits(wordIndex) = readableBits(wordIndex) & mask
      writableBits(wordIndex) = writableBits(wordIndex) & mask

  /**
   * Sets access for a range of pages.
   */
  def setPageAccessRange(startIndex: UInt, pages: Int, access: PageAccess): Unit =
    if pages <= 0 then return

    val endPage = UInt(startIndex.signed + pages - 1)
    ensureCapacity(endPage)

    modifyBitsInRangeImpl(startIndex, pages) { (wordIndex, shiftedMask) =>
      access match
        case PageAccess.ReadOnly =>
          readableBits(wordIndex) = readableBits(wordIndex) | shiftedMask
          writableBits(wordIndex) = writableBits(wordIndex) & ~shiftedMask
        case PageAccess.ReadWrite =>
          readableBits(wordIndex) = readableBits(wordIndex) | shiftedMask
          writableBits(wordIndex) = writableBits(wordIndex) | shiftedMask
    }

  /**
   * Core function to modify bits in a range.
   */
  private def modifyBitsInRangeImpl(startIndex: UInt, pages: Int)(modifier: (Int, Long) => Unit): Unit =
    if pages <= 0 then return

    var currentPage = startIndex.signed
    val endIndex = startIndex.signed + pages

    while currentPage < endIndex do
      val wordIndex = currentPage / 64
      val bitIndex = currentPage % 64
      val bitsInThisWord = math.min(BitsPerWord - bitIndex, endIndex - currentPage)

      val mask: Long =
        if bitsInThisWord == BitsPerWord then -1L
        else (1L << bitsInThisWord) - 1L
      val shiftedMask = mask << bitIndex

      modifier(wordIndex, shiftedMask)
      currentPage += bitsInThisWord

  /**
   * Checks if all pages in an address range are readable.
   * Returns (isReadable, firstFailingPageAddress).
   */
  def isReadable(address: UInt, length: Int): (Boolean, UInt) =
    if length == 0 then (true, address)
    else
      val startPageIndex = UInt(address.signed >>> pageSizeShift)
      val pages = numberOfPagesToAccess(address, length)
      val (result, failPage) = isReadablePages(startPageIndex, pages)
      (result, UInt(failPage.signed << pageSizeShift))

  /**
   * Checks if all pages in a range are readable.
   */
  def isReadablePages(pageStart: UInt, pages: Int): (Boolean, UInt) =
    checkPagesInRange(pageStart, pages, readableBits, isPageReadable)

  /**
   * Checks if all pages in an address range are writable.
   * Returns (isWritable, firstFailingPageAddress).
   */
  def isWritable(address: UInt, length: Int): (Boolean, UInt) =
    if length == 0 then (true, address)
    else
      val startPageIndex = UInt(address.signed >>> pageSizeShift)
      val pages = numberOfPagesToAccess(address, length)
      val (result, failPage) = isWritablePages(startPageIndex, pages)
      (result, UInt(failPage.signed << pageSizeShift))

  /**
   * Checks if all pages in a range are writable.
   */
  def isWritablePages(pageStart: UInt, pages: Int): (Boolean, UInt) =
    checkPagesInRange(pageStart, pages, writableBits, isPageWritable)

  /**
   * Core function to check if all pages in a range have a certain property.
   * Returns (result, firstFailingPage) where result is true if all pages pass.
   */
  private def checkPagesInRange(
    pageStart: UInt,
    pages: Int,
    bits: Array[Long],
    singlePageChecker: UInt => Boolean
  ): (Boolean, UInt) =
    if pages == 0 then return (true, pageStart)

    var currentPage = pageStart.signed
    val pageEnd = pageStart.signed + pages

    while currentPage < pageEnd do
      val wordIndex = currentPage / 64
      val bitIndex = currentPage % 64
      val bitsInThisWord = math.min(BitsPerWord - bitIndex, pageEnd - currentPage)

      // Create mask for the bits in this word
      val mask: Long =
        if bitsInThisWord == BitsPerWord then -1L
        else (1L << bitsInThisWord) - 1L
      val shiftedMask = mask << bitIndex

      // Check if all required bits are set
      val wordValue = if wordIndex < bits.length then bits(wordIndex) else 0L
      if (wordValue & shiftedMask) != shiftedMask then
        // Some bits aren't set - find which page failed
        var bit = 0
        while bit < bitsInThisWord do
          val pageIndex = UInt(currentPage + bit)
          if !singlePageChecker(pageIndex) then
            return (false, pageIndex)
          bit += 1

      currentPage += bitsInThisWord

    (true, pageStart)

  /**
   * Updates access for a range of pages (by address).
   */
  def update(address: UInt, length: Int, access: PageAccess): Unit =
    if length == 0 then return
    val startPageIndex = UInt(address.signed >>> pageSizeShift)
    val pages = numberOfPagesToAccess(address, length)
    setPageAccessRange(startPageIndex, pages, access)

  /**
   * Updates access for a range of pages (by page index).
   */
  def updatePages(pageIndex: UInt, pages: Int, access: PageAccess): Unit =
    setPageAccessRange(pageIndex, pages, access)

  /**
   * Removes access for a range of pages.
   */
  def removeAccess(address: UInt, length: Int): Unit =
    if length == 0 then return
    val startPageIndex = UInt(address.signed >>> pageSizeShift)
    val pages = numberOfPagesToAccess(address, length)
    removeAccessPages(startPageIndex, pages)

  /**
   * Removes access for a range of pages (by page index).
   */
  def removeAccessPages(pageIndex: UInt, pages: Int): Unit =
    if pages <= 0 then return

    modifyBitsInRangeImpl(pageIndex, pages) { (wordIndex, shiftedMask) =>
      if wordIndex < readableBits.length then
        readableBits(wordIndex) = readableBits(wordIndex) & ~shiftedMask
        writableBits(wordIndex) = writableBits(wordIndex) & ~shiftedMask
    }

  /**
   * Aligns an address to the start of its containing page.
   */
  def alignToPageStart(address: UInt): UInt =
    UInt((address.signed >>> pageSizeShift) << pageSizeShift)

object PageMap:
  /**
   * Creates a PageMap with initial page mappings.
   *
   * @param pageMap List of (address, length, access) tuples defining initial mappings
   * @param pageSize The size of each memory page in bytes
   */
  def create(
    mappings: List[(UInt, UInt, PageAccess)],
    pageSize: UInt
  ): PageMap =
    val map = new PageMap(pageSize)
    val pageSizeShift = java.lang.Integer.numberOfTrailingZeros(pageSize.signed)

    // First pass: calculate maximum page index needed
    for (address, length, _) <- mappings if length.signed > 0 do
      val startIndex = address.signed >>> pageSizeShift
      val pages = map.numberOfPagesToAccess(address, length.signed)
      val endIndex = startIndex + pages
      if endIndex > map.maxPageIndex.signed then
        map.maxPageIndex = UInt(endIndex)

    // Allocate bit arrays
    val wordsNeeded = ((map.maxPageIndex.signed + 63) / 64)
    map.readableBits = new Array[Long](wordsNeeded)
    map.writableBits = new Array[Long](wordsNeeded)

    // Second pass: set access bits
    for (address, length, access) <- mappings if length.signed > 0 do
      val startIndex = UInt(address.signed >>> pageSizeShift)
      val pages = map.numberOfPagesToAccess(address, length.signed)
      map.setPageAccessRange(startIndex, pages, access)

    map
