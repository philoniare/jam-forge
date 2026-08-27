package io.forge.jam.pvm.memory

import scala.collection.mutable

final class GuestRam:
  import GuestRam.*

  /** Backing storage for materialised pages, keyed by page index. */
  private val pageData = mutable.LongMap.empty[Array[Byte]]

  /** Access rights per page: absent = inaccessible, R = read-only,
    * W = read+write (gp ram access none/R/W).
    */
  private val pageAccess = mutable.LongMap.empty[Byte]

  private inline def pageOf(address: Long): Long = (address & 0xffffffffL) >>> PageShift

  private def accessOf(pageIndex: Long): Byte =
    pageAccess.getOrElse(pageIndex, AccessNone)

  /** True when every byte of [address, address+length) lies in a readable
    * (R or W) page. Zero-length ranges are trivially readable, matching the
    * empty-subset semantics of the spec's range checks.
    */
  def isReadable(address: Long, length: Long): Boolean =
    checkRange(address, length, write = false)

  /** True when every byte of [address, address+length) lies in a writable (W)
    * page.
    */
  def isWritable(address: Long, length: Long): Boolean =
    checkRange(address, length, write = true)

  private def checkRange(address: Long, length: Long, write: Boolean): Boolean =
    if length < 0 then return false
    if length == 0 then return true
    // Guest addresses beyond the 32-bit space are never accessible (no wrap).
    if (address & ~0xffffffffL) != 0 then return false
    val addr = address
    val end = addr + length - 1
    if end > 0xffffffffL then return false
    var p = addr >>> PageShift
    val lastPage = end >>> PageShift
    while p <= lastPage do
      val a = accessOf(p)
      if a == AccessNone then return false
      if write && a != AccessWrite then return false
      p += 1
    true

  /** Page-start address of the first byte in [address, address+length) that
    * fails the access check, or None when the whole range is accessible. Used
    * by the guest interpreter to report the faulting page for load/store
    * segfaults. Ranges reaching past the 32-bit space report the page of the
    * range start (the interpreter converts sub-MinValidAddress pages to
    * panics).
    */
  def faultPage(address: Long, length: Long, write: Boolean): Option[Long] =
    if length <= 0 then return None
    if (address & ~0xffffffffL) != 0 then return Some(address & ~PageMask)
    val end = address + length - 1
    if end > 0xffffffffL then return Some(address & ~PageMask)
    var p = address >>> PageShift
    val lastPage = end >>> PageShift
    while p <= lastPage do
      val a = accessOf(p)
      if a == AccessNone || (write && a != AccessWrite) then
        return Some(p << PageShift)
      p += 1
    None

  /** Copy `out.length` bytes out of the RAM starting at `address` without an
    * access check (untouched pages read as zeros). Callers must have verified
    * accessibility (e.g. via faultPage/isReadable).
    */
  def readUnchecked(address: Long, out: Array[Byte]): Unit =
    copyOut(address, out)

  /** Read `length` bytes at `address`. Caller must have verified readability;
    * returns None when the range is not readable.
    */
  def read(address: Long, length: Int): Option[Array[Byte]] =
    if !isReadable(address, length.toLong) then None
    else
      val out = new Array[Byte](length)
      copyOut(address, out)
      Some(out)

  /** Copy bytes out of the RAM without an access check (pages() zero-fill and
    * invoke() rely on materialised-or-zero semantics).
    */
  private def copyOut(address: Long, out: Array[Byte]): Unit =
    var copied = 0
    var addr = address & 0xffffffffL
    while copied < out.length do
      val page = addr >>> PageShift
      val offInPage = (addr & PageMask).toInt
      val n = math.min(PageSize - offInPage, out.length - copied)
      pageData.get(page) match
        case Some(arr) => System.arraycopy(arr, offInPage, out, copied, n)
        case None      => // untouched page: zeros, `out` already zeroed
      copied += n
      addr += n

  /** Write `data` at `address`. Caller must have verified writability; returns
    * false when the range is not writable.
    */
  def write(address: Long, data: Array[Byte]): Boolean =
    if !isWritable(address, data.length.toLong) then false
    else
      var copied = 0
      var addr = address & 0xffffffffL
      while copied < data.length do
        val page = addr >>> PageShift
        val offInPage = (addr & PageMask).toInt
        val n = math.min(PageSize - offInPage, data.length - copied)
        val arr = pageData.getOrElseUpdate(page, new Array[Byte](PageSize))
        System.arraycopy(data, copied, arr, offInPage, n)
        copied += n
        addr += n
      true

  /** Raw write used by `invoke` when applying guest stores that already passed
    * the interpreter's own access checks.
    */
  def writeUnchecked(address: Long, data: Array[Byte]): Unit =
    var copied = 0
    var addr = address & 0xffffffffL
    while copied < data.length do
      val page = addr >>> PageShift
      val offInPage = (addr & PageMask).toInt
      val n = math.min(PageSize - offInPage, data.length - copied)
      val arr = pageData.getOrElseUpdate(page, new Array[Byte](PageSize))
      System.arraycopy(data, copied, arr, offInPage, n)
      copied += n
      addr += n

  /** True when every page of [pageStart, pageStart+count) is accessible (R or
    * W) — the Omega_Z `r > 2` precondition.
    */
  def pagesAccessible(pageStart: Long, count: Long): Boolean =
    var p = pageStart
    while p < pageStart + count do
      if accessOf(p) == AccessNone then return false
      p += 1
    true

  /** Apply the `pages` host call (Omega_Z) mutation for validated arguments:
    * zero the range's contents when `variant < 3` (preserve when 3/4) and set
    * access to none (0), R (1/3) or W (2/4).
    */
  def applyPages(pageStart: Long, count: Long, variant: Long): Unit =
    val newAccess: Byte = variant match
      case 0     => AccessNone
      case 1 | 3 => AccessRead
      case _     => AccessWrite
    var p = pageStart
    while p < pageStart + count do
      if variant < 3 then pageData.remove(p) // zero contents
      if newAccess == AccessNone then pageAccess.remove(p)
      else pageAccess.update(p, newAccess)
      p += 1

object GuestRam:
  val PageSize: Int = 4096
  val PageShift: Int = 12
  val PageMask: Long = PageSize - 1L
  /** Total pages in the 32-bit space: 2^32 / 4096. */
  val TotalPages: Long = 1L << 20

  private val AccessNone: Byte = 0
  private val AccessRead: Byte = 1
  private val AccessWrite: Byte = 2
