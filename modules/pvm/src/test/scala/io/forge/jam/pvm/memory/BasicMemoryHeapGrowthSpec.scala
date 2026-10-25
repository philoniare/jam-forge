package io.forge.jam.pvm.memory

import org.scalatest.funsuite.AnyFunSuite
import spire.math.{UByte, UInt}
import io.forge.jam.pvm.{MemoryMap, MemoryResult}

/** Heap growth through sbrk, which backs grow_heap (Ω_Gemini). */
class BasicMemoryHeapGrowthSpec extends AnyFunSuite:

  private val PageSize = UInt(4096)

  private def memory(): BasicMemory =
    val map = MemoryMap.builder(PageSize).withRoDataSize(PageSize).withRwDataSize(PageSize)
      .withStackSize(UInt(65536)).withAuxDataSize(PageSize).build().fold(e => fail(e), identity)
    BasicMemory.create(map)

  test("grow_heap makes exactly the requested pages writable, however the buffer grows"):
    val mem = memory()
    for _ <- 1 to 50 do
      val end = mem.sbrk(UInt(2 * 4096)).get + UInt(2 * 4096)
      assert(mem.heapEnd == end)
      // The last new byte is writable; the first byte past the heap end faults on load and store.
      assert(mem.storeU8(end - UInt(1), UByte(7)) == MemoryResult.Success(()))
      assert(mem.loadU8(end) match { case MemoryResult.Segfault(_, _) => true; case _ => false })
      assert(mem.storeU8(end, UByte(7)) match { case MemoryResult.Segfault(_, _) => true; case _ => false })

  test("newly writable heap pages read as zero and earlier heap bytes survive growth"):
    val mem = memory()
    val base = mem.sbrk(UInt(4096)).get
    assert(mem.storeU8(base, UByte(0xab)) == MemoryResult.Success(()))
    for _ <- 1 to 20 do
      val prevEnd = mem.sbrk(UInt(3 * 4096)).get
      assert(mem.loadU8(prevEnd) == MemoryResult.Success(UByte(0)))
      assert(mem.loadU8(mem.heapEnd - UInt(1)) == MemoryResult.Success(UByte(0)))
    assert(mem.loadU8(base) == MemoryResult.Success(UByte(0xab)))

  test("a heap buffer reused from a released memory reads as zero in the new owner"):
    val first = memory()
    val base1 = first.sbrk(UInt(2 << 20)).get
    val dirty = Array.fill[Byte](2 << 20)(0x5a)
    assert(first.setMemorySlice(base1, dirty) == MemoryResult.Success(()))
    first.release()
    // A released memory no longer sees, or writes to, the buffer it gave up.
    assert(first.loadU8(base1) != MemoryResult.Success(UByte(0x5a)))
    assert(first.storeU8(base1, UByte(1)) != MemoryResult.Success(()))

    val second = memory()
    val base2 = second.sbrk(UInt(4096)).get
    assert(second.storeU8(base2, UByte(0x11)) == MemoryResult.Success(()))
    second.sbrk(UInt((2 << 20) - 4096)).get
    val all = second.getMemorySlice(base2, 2 << 20) match
      case MemoryResult.Success(bytes) => bytes
      case other                       => fail(s"read failed: $other")
    assert(all(0) == 0x11)
    assert(all.drop(1).forall(_ == 0))
