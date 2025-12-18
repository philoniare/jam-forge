package io.forge.jam.pvm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.{UByte, UInt}
import io.forge.jam.pvm.memory.*

/**
 * Tests for the PVM memory model.
 *
 * These tests verify:
 * 1. PageMap page permission tracking (R/W flags)
 * 2. BasicMemory read/write within bounds
 * 3. Memory segfault on invalid access
 * 4. Heap growth via sbrk
 * 5. Page alignment calculations
 * 6. Memory region validation (roData, rwData, stack, heap, aux)
 */
class MemorySpec extends AnyFlatSpec with Matchers:

  val DefaultPageSize: UInt = UInt(4096) // 4KB standard page size

  // Test 1: PageMap page permission tracking (R/W flags)
  "PageMap" should "correctly track read/write permissions per page" in {
    val pageMap = new PageMap(DefaultPageSize)

    // Initially, no pages should be accessible
    val page0 = UInt(0)
    pageMap.isPageReadable(page0) shouldBe false
    pageMap.isPageWritable(page0) shouldBe false

    // Set page 0 as read-only
    pageMap.setPageAccess(page0, PageAccess.ReadOnly)
    pageMap.isPageReadable(page0) shouldBe true
    pageMap.isPageWritable(page0) shouldBe false

    // Set page 1 as read-write
    val page1 = UInt(1)
    pageMap.setPageAccess(page1, PageAccess.ReadWrite)
    pageMap.isPageReadable(page1) shouldBe true
    pageMap.isPageWritable(page1) shouldBe true

    // Remove access from page 0
    pageMap.removePageAccess(page0)
    pageMap.isPageReadable(page0) shouldBe false
    pageMap.isPageWritable(page0) shouldBe false

    // Page 1 should still be accessible
    pageMap.isPageReadable(page1) shouldBe true
  }

  // Test 2: BasicMemory read/write within bounds
  "BasicMemory" should "support read/write operations within bounds" in {
    val memoryMap = createTestMemoryMap()
    val memory = BasicMemory.create(memoryMap)

    // Write to RW data region
    val rwAddress = memoryMap.rwDataAddress
    memory.storeU8(rwAddress, UByte(0x42))
    memory.loadU8(rwAddress) shouldBe MemoryResult.Success(UByte(0x42))

    // Write 32-bit value
    memory.storeU32(rwAddress + UInt(4), UInt(0xDEADBEEF))
    memory.loadU32(rwAddress + UInt(4)) shouldBe MemoryResult.Success(UInt(0xDEADBEEF))

    // Write to stack region
    val stackAddress = memoryMap.stackAddressHigh - UInt(100)
    memory.storeU8(stackAddress, UByte(0xFF))
    memory.loadU8(stackAddress) shouldBe MemoryResult.Success(UByte(0xFF))
  }

  // Test 3: Memory segfault on invalid access
  it should "return segfault on invalid memory access" in {
    val memoryMap = createTestMemoryMap()
    val memory = BasicMemory.create(memoryMap)

    // Access to unmapped address should segfault
    val invalidAddress = UInt(0x1000) // In null page region
    val result = memory.loadU8(invalidAddress)
    result match
      case MemoryResult.Segfault(addr, _) =>
        // Segfault should report the address
        addr shouldBe invalidAddress
      case other =>
        fail(s"Expected Segfault but got $other")

    // Write to read-only region should segfault
    val roAddress = memoryMap.roDataAddress
    memory.storeU8(roAddress, UByte(0x00)) match
      case MemoryResult.Segfault(_, _) => succeed
      case other => fail(s"Expected Segfault for RO write but got $other")
  }

  // Test 4: Heap growth via sbrk
  it should "support heap growth via sbrk" in {
    val memoryMap = createTestMemoryMap()
    val memory = BasicMemory.create(memoryMap)

    // Get initial heap end
    val initialHeapEnd = memory.heapEnd
    initialHeapEnd shouldBe memoryMap.heapBase

    // Grow heap by one page
    val growSize = DefaultPageSize
    val prevEnd = memory.sbrk(growSize)

    prevEnd match
      case Some(addr) =>
        addr shouldBe initialHeapEnd
        memory.heapEnd shouldBe (initialHeapEnd + growSize)

        // New heap memory should be writable
        memory.storeU8(addr, UByte(0xAB))
        memory.loadU8(addr) shouldBe MemoryResult.Success(UByte(0xAB))
      case None =>
        fail("sbrk should succeed")

    // sbrk(0) should return current heap end
    memory.sbrk(UInt(0)) shouldBe Some(memory.heapEnd)
  }

  // Test 5: Page alignment calculations
  "PageMap" should "correctly calculate page alignment" in {
    val pageMap = new PageMap(DefaultPageSize)

    // Test alignToPageStart
    pageMap.alignToPageStart(UInt(0)) shouldBe UInt(0)
    pageMap.alignToPageStart(UInt(100)) shouldBe UInt(0)
    pageMap.alignToPageStart(UInt(4095)) shouldBe UInt(0)
    pageMap.alignToPageStart(UInt(4096)) shouldBe UInt(4096)
    pageMap.alignToPageStart(UInt(5000)) shouldBe UInt(4096)
    pageMap.alignToPageStart(UInt(8192)) shouldBe UInt(8192)

    // Test numberOfPagesToAccess
    pageMap.numberOfPagesToAccess(UInt(0), 1) shouldBe 1
    pageMap.numberOfPagesToAccess(UInt(0), 4096) shouldBe 1
    pageMap.numberOfPagesToAccess(UInt(0), 4097) shouldBe 2
    pageMap.numberOfPagesToAccess(UInt(100), 4000) shouldBe 2  // Crosses page boundary
    pageMap.numberOfPagesToAccess(UInt(4096), 4096) shouldBe 1
    pageMap.numberOfPagesToAccess(UInt(0), 0) shouldBe 0
  }

  // Test 6: Memory region validation (roData, rwData, stack, heap, aux)
  "BasicMemory" should "correctly validate memory regions" in {
    val memoryMap = createTestMemoryMap()
    val memory = BasicMemory.create(memoryMap)

    // RO data region should be readable but not writable
    val roAddr = memoryMap.roDataAddress
    memory.loadU8(roAddr) match
      case MemoryResult.Success(_) => succeed
      case other => fail(s"RO region should be readable, got $other")

    memory.storeU8(roAddr, UByte(0)) match
      case MemoryResult.Segfault(_, _) => succeed
      case other => fail(s"RO region should not be writable, got $other")

    // RW data region should be readable and writable
    val rwAddr = memoryMap.rwDataAddress
    memory.storeU8(rwAddr, UByte(0x12))
    memory.loadU8(rwAddr) shouldBe MemoryResult.Success(UByte(0x12))

    // Stack region should be readable and writable
    val stackAddr = memoryMap.stackAddressHigh - UInt(8)
    memory.storeU8(stackAddr, UByte(0x34))
    memory.loadU8(stackAddr) shouldBe MemoryResult.Success(UByte(0x34))

    // Aux data region should be readable (may or may not be writable depending on GP stack overlap)
    val auxAddr = memoryMap.auxDataAddress
    memory.loadU8(auxAddr) match
      case MemoryResult.Success(_) => succeed
      case MemoryResult.Segfault(_, _) => succeed // OK if aux size is 0
      case other => fail(s"Aux region behavior unexpected: $other")
  }

  // Helper: Create a test MemoryMap
  private def createTestMemoryMap(): MemoryMap =
    MemoryMap.builder(DefaultPageSize)
      .withRoDataSize(UInt(4096))
      .withRwDataSize(UInt(8192))
      .withStackSize(UInt(16384))
      .withAuxDataSize(UInt(4096))
      .build() match
        case Right(mm) => mm
        case Left(err) => fail(s"Failed to create MemoryMap: $err")
