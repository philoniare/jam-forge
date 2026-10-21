package io.forge.jam.pvm.memory

import org.scalatest.funsuite.AnyFunSuite
import spire.math.UInt
import io.forge.jam.pvm.MemoryMap

/** The fast load/store windows and the page-run scan behind them. */
class BasicMemoryWindowSpec extends AnyFunSuite:

  private val PageSize = UInt(4096)

  /** Page-at-a-time reference for [[PageMap.accessibleRun]]. */
  private def naiveRun(map: PageMap, page: Int, writable: Boolean, limit: Int): (Int, Int) =
    def set(p: Int) = p >= 0 && p < limit && (if writable then map.isPageWritable(UInt(p)) else map.isPageReadable(UInt(p)))
    if !set(page) then (page, page)
    else
      var lo = page; var n = 0
      while lo > 0 && n < (1 << 16) && set(lo - 1) do { lo -= 1; n += 1 }
      var hi = page + 1; n = 0
      while hi < limit && n < (1 << 16) && set(hi) do { hi += 1; n += 1 }
      (lo, hi)

  test("accessibleRun matches a page-at-a-time scan"):
    val rnd = new scala.util.Random(7)
    for _ <- 0 until 40 do
      val mappings = List.fill(1 + rnd.nextInt(6)) {
        val start = rnd.nextInt(1500)
        val len = 1 + rnd.nextInt(300)
        val access = if rnd.nextBoolean() then PageAccess.ReadOnly else PageAccess.ReadWrite
        (UInt(start * 4096), UInt(len * 4096), access)
      }
      val map = PageMap.create(mappings, PageSize)
      val limit = ((1800 + 63) / 64) * 64
      for page <- 0 until 1800 by 3; writable <- Seq(false, true) do
        val run = map.accessibleRun(page, writable)
        val got = ((run >>> 32).toInt, (run & 0xffffffffL).toInt)
        assert(got == naiveRun(map, page, writable, limit), s"page $page writable $writable")

  test("accessibleRun caps the run at 64Ki pages each way"):
    val map = PageMap.create(List((UInt(0), UInt(0x7ffff000), PageAccess.ReadWrite)), PageSize)
    val page = 0x40000
    val run = map.accessibleRun(page, writable = true)
    assert((run >>> 32).toInt == page - (1 << 16))
    assert((run & 0xffffffffL).toInt == page + 1 + (1 << 16))

  private def newMemory(): (BasicMemory, MemoryMap, Array[Byte], Array[Byte], Array[Byte]) =
    val rnd = new scala.util.Random(3)
    val ro = Array.fill[Byte](8192)(rnd.nextInt().toByte)
    val rw = Array.fill[Byte](8192)(rnd.nextInt().toByte)
    val args = Array.fill[Byte](4096)(rnd.nextInt().toByte)
    val map = MemoryMap
      .builder(PageSize)
      .withRoDataSize(UInt(ro.length))
      .withRwDataSize(UInt(rw.length))
      .withStackSize(UInt(16384))
      .withAuxDataSize(UInt(16908288))
      .build()
      .fold(e => fail(e), identity)
    (BasicMemory.create(map, ro, rw, UInt(rw.length), args), map, ro, rw, args)

  private def le8(a: Array[Byte], off: Int): Long =
    (0 until 8).foldLeft(0L)((acc, i) => acc | ((a(off + i) & 0xffL) << (8 * i)))

  test("loads and stores cycling through four regions read the right bytes"):
    val (mem, map, ro, rw, args) = newMemory()
    val stackCopy = new Array[Byte](16384)
    val rwCopy = rw.clone()
    val stackLow = map.stackAddressLow.signed
    for i <- 0 until 2000 do
      val off = (i * 8) % 4000
      assert(mem.loadUnsignedFast(map.roDataAddress.signed + off, 8) == BasicMemory.FastOk)
      assert(mem.fastValue == le8(ro, off))
      assert(mem.loadUnsignedFast(map.rwDataAddress.signed + off, 4) == BasicMemory.FastOk)
      assert(mem.fastValue == (le8(rwCopy, off) & 0xffffffffL))
      assert(mem.loadUnsignedFast(map.auxDataAddress.signed + off, 2) == BasicMemory.FastOk)
      assert(mem.fastValue == (le8(args, off) & 0xffffL))
      val v = i.toLong * 0x0101010101010101L
      assert(mem.storeFast(stackLow + off, v, 8) == BasicMemory.FastOk)
      for k <- 0 until 8 do stackCopy(off + k) = (v >>> (8 * k)).toByte
      assert(mem.storeFast(map.rwDataAddress.signed + off + 8, ~v, 8) == BasicMemory.FastOk)
      for k <- 0 until 8 do rwCopy(off + 8 + k) = (~v >>> (8 * k)).toByte
      assert(mem.loadUnsignedFast(stackLow + off, 8) == BasicMemory.FastOk)
      assert(mem.fastValue == le8(stackCopy, off))

  test("accesses past a region's mapped pages fault even after a window covered the region"):
    val (mem, map, _, _, _) = newMemory()
    assert(mem.loadUnsignedFast(map.roDataAddress.signed, 8) == BasicMemory.FastOk)
    assert(mem.loadUnsignedFast(map.roDataAddress.signed + 8192, 1) == BasicMemory.FastSegfault)
    assert(mem.storeFast(map.roDataAddress.signed, 1L, 1) == BasicMemory.FastSegfault)
    assert(mem.loadUnsignedFast(map.auxDataAddress.signed + 4096, 1) == BasicMemory.FastSegfault)

  test("a window does not outlive heap growth"):
    val (mem, map, _, _, _) = newMemory()
    val heapEnd = mem.heapEnd.signed
    assert(mem.storeFast(map.rwDataAddress.signed, 5L, 8) == BasicMemory.FastOk)
    assert(mem.storeFast(heapEnd, 1L, 8) == BasicMemory.FastSegfault)
    assert(mem.sbrk(UInt(4096)).isDefined)
    assert(mem.storeFast(heapEnd, 0x1122334455667788L, 8) == BasicMemory.FastOk)
    assert(mem.loadUnsignedFast(heapEnd, 8) == BasicMemory.FastOk)
    assert(mem.fastValue == 0x1122334455667788L)
    assert(mem.loadUnsignedFast(map.rwDataAddress.signed, 8) == BasicMemory.FastOk)
    assert(mem.fastValue == 5L)
