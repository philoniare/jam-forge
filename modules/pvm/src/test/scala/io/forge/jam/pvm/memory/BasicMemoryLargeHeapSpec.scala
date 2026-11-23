package io.forge.jam.pvm.memory

import org.scalatest.funsuite.AnyFunSuite
import spire.math.{UByte, UInt, ULong, UShort}
import io.forge.jam.pvm.{MemoryMap, MemoryResult}

import scala.util.Random

class BasicMemoryLargeHeapSpec extends AnyFunSuite:

  private val Page = 4096
  private val PageSize = UInt(Page)

  private def memoryMap(rwBytes: Int = Page): MemoryMap =
    MemoryMap.builder(PageSize).withRoDataSize(PageSize).withRwDataSize(UInt(rwBytes))
      .withStackSize(UInt(65536)).withAuxDataSize(PageSize).build().fold(e => fail(e), identity)

  private def u(address: Long): UInt = UInt(address.toInt)

  private def grownTo(heapEnd: Long, rwBytes: Int = Page): BasicMemory =
    val map = memoryMap(rwBytes)
    val mem = BasicMemory.create(map, Array.emptyByteArray, Array.emptyByteArray, initialHeapSize = UInt(rwBytes))
    val grow = heapEnd - (map.heapBase.toLong + rwBytes)
    assert(mem.sbrk(u(grow)).isDefined, s"the heap could not grow by $grow bytes")
    assert(mem.heapEnd.toLong == heapEnd)
    mem

  private def isFault(r: MemoryResult[?]): Boolean = r match
    case MemoryResult.Segfault(_, _) => true
    case _                           => false

  private val ThreeGiB = 0xC0000000L

  test("a heap that ends above 2^31 is readable and writable at every width, up to its last byte"):
    val mem = grownTo(ThreeGiB)
    for a <- Seq(0x20000000L, 0x7ffffff8L, 0x80000000L, 0xbffffff8L) do
      assert(mem.loadU64(u(a)) == MemoryResult.Success(ULong(0)), f"at 0x$a%x")

    val value = 0x1122334455667788L
    val addresses = Seq(0x30000000L, 0x7ffffff0L) ++ (0x7ffffff9L to 0x80000000L) ++ Seq(0x80000008L, 0xa0001234L, ThreeGiB - 8)
    for a <- addresses do
      assert(mem.storeU64(u(a), ULong(value)) == MemoryResult.Success(()), f"store at 0x$a%x")
      assert(mem.loadU64(u(a)) == MemoryResult.Success(ULong(value)), f"load at 0x$a%x")
      assert(mem.loadU32(u(a + 4)) == MemoryResult.Success(UInt((value >>> 32).toInt)), f"upper half at 0x$a%x")
      assert(mem.loadU16(u(a + 6)) == MemoryResult.Success(UShort((value >>> 48).toInt)), f"top two bytes at 0x$a%x")
      assert(mem.loadU8(u(a)) == MemoryResult.Success(UByte(0x88)), f"first byte at 0x$a%x")
    assert(mem.storeU8(u(ThreeGiB - 1), UByte(0x5a)) == MemoryResult.Success(()))
    assert(mem.loadU8(u(ThreeGiB - 1)) == MemoryResult.Success(UByte(0x5a)))
    assert(mem.storeU16(u(0x8000fffeL), UShort(0xbeef)) == MemoryResult.Success(()))
    assert(mem.loadU16(u(0x8000fffeL)) == MemoryResult.Success(UShort(0xbeef)))
    assert(mem.storeU32(u(0x9000fffdL), UInt(0xcafef00d)) == MemoryResult.Success(()))
    assert(mem.loadU32(u(0x9000fffdL)) == MemoryResult.Success(UInt(0xcafef00d)))

    assert(isFault(mem.loadU8(u(ThreeGiB))))
    assert(isFault(mem.storeU8(u(ThreeGiB), UByte(1))))
    assert(isFault(mem.storeU64(u(ThreeGiB - 4), ULong(1))))

  test("the interpreter's own load and store paths agree with the checked ones above 2^31"):
    val mem = grownTo(ThreeGiB)
    val rng = new Random(7)
    for base <- Seq(0x40000000L, 0x7fffff00L, 0x80000000L, 0xb0000000L) do
      for i <- 0 until 2000 do
        val a = base + rng.nextInt(0x300)
        val width = 1 << rng.nextInt(4)
        val v = rng.nextLong()
        assert(mem.storeFast(a.toInt, v, width) == BasicMemory.FastOk, f"store at 0x$a%x")
        assert(mem.loadUnsignedFast(a.toInt, width) == BasicMemory.FastOk, f"load at 0x$a%x")
        val mask = if width == 8 then -1L else (1L << (width * 8)) - 1
        assert(mem.fastValue == (v & mask), f"value at 0x$a%x width $width, pass $i")
        assert(mem.loadU8(u(a)) == MemoryResult.Success(UByte((v & 0xff).toInt)))
    assert(mem.loadUnsignedFast(ThreeGiB.toInt, 1) == BasicMemory.FastSegfault)
    assert(mem.storeFast(ThreeGiB.toInt, 1L, 1) == BasicMemory.FastSegfault)

  test("slices that cross the pieces a large heap is kept in read back what was written"):
    val mem = grownTo(ThreeGiB)
    val rng = new Random(11)
    for (a, n) <- Seq((0x7ffe0000L, 0x50000), (0x8100fff0L, 0x40), (0x90000000L, 0x30001), (0xbffb0000L, 0x40000)) do
      val data = new Array[Byte](n)
      rng.nextBytes(data)
      assert(mem.setMemorySlice(u(a), data) == MemoryResult.Success(()), f"write $n bytes at 0x$a%x")
      mem.getMemorySlice(u(a), n) match
        case MemoryResult.Success(read) => assert(java.util.Arrays.equals(read, data), f"$n bytes at 0x$a%x")
        case other                      => fail(f"read at 0x$a%x: $other")
      val into = new Array[Byte](n + 2)
      assert(mem.getMemorySliceInto(u(a), into, 1, n) == MemoryResult.Success(()))
      assert(java.util.Arrays.equals(into, 1, n + 1, data, 0, n))
      assert(mem.loadU8(u(a - 1)) == MemoryResult.Success(UByte(0)))
      assert(mem.loadU8(u(a + n)) == MemoryResult.Success(UByte(0)))

  test("a heap grown in steps keeps what was written below while it grows past the buffer and past 2^31"):
    val map = memoryMap()
    val mem = BasicMemory.create(map, Array.emptyByteArray, Array.emptyByteArray, initialHeapSize = PageSize)
    val base = map.heapBase.toLong
    val rng = new Random(13)
    val written = scala.collection.mutable.LinkedHashMap[Long, Byte]()
    var end = base + Page
    val steps = Seq.fill(40)(Page.toLong * (1 + rng.nextInt(64))) ++ Seq.fill(11)(0x10000000L + Page * rng.nextInt(100))
    for step <- steps do
      assert(mem.sbrk(u(step)).contains(u(end)), f"growing by 0x$step%x from 0x$end%x")
      val newEnd = end + step
      for _ <- 1 to 20 do
        val a = end + (rng.nextLong() & Long.MaxValue) % step
        val b = (1 + rng.nextInt(255)).toByte
        assert(mem.storeU8(u(a), UByte(b)) == MemoryResult.Success(()), f"store at 0x$a%x")
        written(a) = b
      assert(mem.loadU8(u(newEnd - 1)).isInstanceOf[MemoryResult.Success[?]])
      assert(isFault(mem.loadU8(u(newEnd))))
      end = newEnd
      written.foreach((a, b) => assert(mem.loadU8(u(a)) == MemoryResult.Success(UByte(b)), f"at 0x$a%x after growing to 0x$end%x"))
    assert(end > 0x80000000L, "the steps were meant to pass 2^31")

  test("a program with no read-write data can grow a heap past 2 GiB and use its first page"):
    val map = MemoryMap.builder(PageSize).withRoDataSize(PageSize).withRwDataSize(UInt(0))
      .withStackSize(UInt(65536)).withAuxDataSize(PageSize).build().fold(e => fail(e), identity)
    val mem = BasicMemory.create(map)
    val base = map.heapBase.toLong
    assert(mem.sbrk(u(0x80020000L - base)).contains(map.heapBase))
    assert(mem.storeU8(u(base), UByte(9)) == MemoryResult.Success(()))
    assert(mem.loadU8(u(base)) == MemoryResult.Success(UByte(9)))
    assert(mem.storeU64(u(0x8001fff8L), ULong(-1L)) == MemoryResult.Success(()))
    assert(mem.loadU64(u(0x8001fff8L)) == MemoryResult.Success(ULong(-1L)))

  test("growing a heap by gigabytes allocates only what is written"):
    val mem = grownTo(0xfef00000L)
    val before = mem.heapBackingBytes
    assert(before < (64L << 20), s"$before bytes back a heap nothing was written to")
    for a <- Seq(0x40000000L, 0x80000000L, 0xfeeffff8L) do
      assert(mem.storeU64(u(a), ULong(1)) == MemoryResult.Success(()))
    for a <- 0x50800000L until 0xf0000000L by 0x01000000L do
      assert(mem.loadU64(u(a)) == MemoryResult.Success(ULong(0)))
    assert(mem.heapBackingBytes - before <= (1L << 20), "three small writes took more than a megabyte")

  test("a released memory with a large heap no longer reads or takes writes"):
    val mem = grownTo(ThreeGiB)
    assert(mem.storeU8(u(0x90000000L), UByte(0x5a)) == MemoryResult.Success(()))
    mem.release()
    assert(mem.loadU8(u(0x90000000L)) != MemoryResult.Success(UByte(0x5a)))
    assert(mem.storeU8(u(0x90000000L), UByte(1)) != MemoryResult.Success(()))
    assert(mem.heapBackingBytes == 0)
