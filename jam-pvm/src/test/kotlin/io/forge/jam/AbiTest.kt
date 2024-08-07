package io.forge.jam

import org.junit.jupiter.api.assertDoesNotThrow
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class AbiTest {

    @Test
    fun testConstantRequirements() {
        assertDoesNotThrow {
            require(VM_MIN_PAGE_SIZE <= VM_MAX_PAGE_SIZE) { "io.forge.jam.VM_MIN_PAGE_SIZE must be less than or equal to io.forge.jam.VM_MAX_PAGE_SIZE" }
        }
        assertDoesNotThrow {
            require(VM_ADDR_RETURN_TO_HOST and 0b11U == 0U) { "io.forge.jam.VM_ADDR_RETURN_TO_HOST must be aligned to 4 bytes" }
        }
    }

    @Test
    fun testMemoryMapBasicCase() {
        val map = MemoryMap.new(0x4000U, 1U, 1U, 1U).getOrThrow()
        assertEquals(0x10000U, map.roDataAddress())
        assertEquals(0x4000U, map.roDataSize)
        assertEquals(0x30000U, map.rwDataAddress())
        assertEquals(0x4000U, map.rwDataSize)
        assertEquals(0x4000U, map.stackSize)
        assertEquals(0xffff0000U, map.stackAddressHigh())
        assertEquals(0xfffec000U, map.stackAddressLow())
        assertEquals(0x30001U, map.heapBase)
        assertEquals(
            (ADDRESS_SPACE_SIZE - VM_MAX_PAGE_SIZE.toULong() * 3UL - map.heapBase.toULong()).toUInt(),
            map.maxHeapSize
        )
    }

    @Test
    fun testMemoryMapMaxReadOnlyData() {
        val maxSize = (ADDRESS_SPACE_SIZE - VM_MAX_PAGE_SIZE.toULong() * 4UL).toUInt()
        println("MaxSize: $maxSize")
        val map = MemoryMap.new(0x4000U, maxSize, 0U, 0U).getOrThrow()
        assertEquals(0x10000U, map.roDataAddress())
        assertEquals(maxSize, map.roDataSize)
        assertEquals(map.roDataAddress() + VM_MAX_PAGE_SIZE + maxSize, map.rwDataAddress())
        assertEquals(0U, map.rwDataSize)
        assertEquals(VM_ADDR_USER_STACK_HIGH, map.stackAddressHigh())
        assertEquals(VM_ADDR_USER_STACK_HIGH, map.stackAddressLow())
        assertEquals(0U, map.stackSize)
        assertEquals(map.rwDataAddress(), map.heapBase)
        assertEquals(0U, map.maxHeapSize)
    }

    @Test
    fun testMemoryMapMaxReadWriteData() {
        val maxSize = (ADDRESS_SPACE_SIZE - VM_MAX_PAGE_SIZE.toULong() * 4UL).toUInt()
        val map = MemoryMap.new(0x4000U, 0U, maxSize, 0U).getOrThrow()
        assertEquals(VM_MAX_PAGE_SIZE, map.roDataAddress())
        assertEquals(0U, map.roDataSize)
        assertEquals(VM_MAX_PAGE_SIZE * 2U, map.rwDataAddress())
        assertEquals(maxSize, map.rwDataSize)
        assertEquals(VM_ADDR_USER_STACK_HIGH, map.stackAddressHigh())
        assertEquals(VM_ADDR_USER_STACK_HIGH, map.stackAddressLow())
        assertEquals(0U, map.stackSize)
        assertEquals(map.rwDataAddress() + map.rwDataSize, map.heapBase)
        assertEquals(0U, map.maxHeapSize)
    }

    @Test
    fun testMemoryMapMaxStack() {
        val maxSize = (ADDRESS_SPACE_SIZE - VM_MAX_PAGE_SIZE.toULong() * 4UL).toUInt()
        val map = MemoryMap.new(0x4000U, 0U, 0U, maxSize).getOrThrow()
        assertEquals(VM_MAX_PAGE_SIZE, map.roDataAddress())
        assertEquals(0U, map.roDataSize)
        assertEquals(VM_MAX_PAGE_SIZE * 2U, map.rwDataAddress())
        assertEquals(0U, map.rwDataSize)
        assertEquals(VM_ADDR_USER_STACK_HIGH, map.stackAddressHigh())
        assertEquals(VM_ADDR_USER_STACK_HIGH - maxSize, map.stackAddressLow())
        assertEquals(maxSize, map.stackSize)
        assertEquals(map.rwDataAddress(), map.heapBase)
        assertEquals(0U, map.maxHeapSize)
    }

    @Test
    fun testMemoryMapErrorCases() {
        val maxSize = (ADDRESS_SPACE_SIZE - VM_MAX_PAGE_SIZE.toULong() * 4UL).toUInt()
        assertFailsWith<IllegalArgumentException> { MemoryMap.new(0x4000U, maxSize + 1U, 0U, 0U).getOrThrow() }
        assertFailsWith<IllegalArgumentException> { MemoryMap.new(0x4000U, maxSize, 1U, 0U).getOrThrow() }
        assertFailsWith<IllegalArgumentException> { MemoryMap.new(0x4000U, maxSize, 0U, 1U).getOrThrow() }
    }
}
