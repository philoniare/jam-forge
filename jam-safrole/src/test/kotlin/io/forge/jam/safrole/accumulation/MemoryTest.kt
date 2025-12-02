package io.forge.jam.safrole.accumulation

import io.forge.jam.pvm.Abi
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.ArcBytes
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.program.ProgramParts
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

/**
 * Tests for PVM memory implementation.
 */
class MemoryTest {

    private val defaultPageSize = 4096u

    // Minimal valid code blob: 0 jump table entries, 0 entry size, 0 code length
    private val minimalCode = byteArrayOf(0, 0, 0)

    private fun createInstance(
        roData: ByteArray = byteArrayOf(),
        rwData: ByteArray = byteArrayOf(),
        stackSize: UInt = 4096u
    ): Pair<Module, RawInstance> {
        val config = ModuleConfig.new(dynamicPaging = false)
        val parts = ProgramParts(
            isaKind = InstructionSetKind.Latest64,
            roDataSize = if (roData.isEmpty()) 0u else defaultPageSize,
            rwDataSize = if (rwData.isEmpty()) 0u else defaultPageSize,
            stackSize = stackSize,
            roData = ArcBytes.fromStatic(roData),
            rwData = ArcBytes.fromStatic(rwData),
            codeAndJumpTable = ArcBytes.fromStatic(minimalCode)
        )
        val blob = ProgramBlob.fromParts(parts).getOrThrow()
        val engine = Engine(BackendKind.Interpreter, null, true, false, EngineState(null, null), false)
        val module = Module.fromBlob(engine, config, blob).getOrThrow()
        val instance = module.instantiate().getOrThrow()
        return Pair(module, instance)
    }

    @Nested
    inner class MemoryMapBuilderTests {

        @Test
        fun `build with valid configuration`() {
            val memoryMap = Abi.MemoryMapBuilder.new(defaultPageSize)
                .roDataSize(100u)
                .rwDataSize(200u)
                .stackSize(1024u)
                .build()
                .getOrThrow()

            assertEquals(defaultPageSize, memoryMap.pageSize)
            // Sizes should be page-aligned
            assertTrue(memoryMap.roDataSize >= 100u)
            assertTrue(memoryMap.rwDataSize >= 200u)
            assertTrue(memoryMap.stackSize >= 1024u)
        }

        @Test
        fun `page size must be power of two`() {
            val result = Abi.MemoryMapBuilder.new(4097u) // Not power of two
                .roDataSize(100u)
                .rwDataSize(200u)
                .stackSize(1024u)
                .build()

            assertTrue(result.isFailure)
        }

        @Test
        fun `page size too small`() {
            val result = Abi.MemoryMapBuilder.new(512u) // Less than VM_MIN_PAGE_SIZE (4096)
                .roDataSize(100u)
                .rwDataSize(200u)
                .stackSize(1024u)
                .build()

            assertTrue(result.isFailure)
        }

        @Test
        fun `page size too big`() {
            val result = Abi.MemoryMapBuilder.new(0x20000u) // Greater than VM_MAX_PAGE_SIZE (65536)
                .roDataSize(100u)
                .rwDataSize(200u)
                .stackSize(1024u)
                .build()

            assertTrue(result.isFailure)
        }

        @Test
        fun `memory regions are properly aligned`() {
            val memoryMap = Abi.MemoryMapBuilder.new(defaultPageSize)
                .roDataSize(1u) // Tiny size
                .rwDataSize(1u)
                .stackSize(1u)
                .build()
                .getOrThrow()

            // All sizes should be aligned to page size
            assertEquals(0u, memoryMap.roDataSize % defaultPageSize)
            assertEquals(0u, memoryMap.rwDataSize % defaultPageSize)
            assertEquals(0u, memoryMap.stackSize % defaultPageSize)
        }

        @Test
        fun `memory regions do not overlap`() {
            val memoryMap = Abi.MemoryMapBuilder.new(defaultPageSize)
                .roDataSize(4096u)
                .rwDataSize(4096u)
                .stackSize(4096u)
                .auxDataSize(4096u)
                .build()
                .getOrThrow()

            val roRange = memoryMap.roDataRange()
            val rwRange = memoryMap.rwDataRange()
            val stackRange = memoryMap.stackRange()
            val auxRange = memoryMap.auxDataRange()

            // RO should end before RW starts
            assertTrue(roRange.last <= rwRange.first)
            // RW should end before stack starts
            assertTrue(rwRange.last <= stackRange.first)
            // Stack should end before aux starts
            assertTrue(stackRange.last <= auxRange.first)
        }
    }

    @Nested
    inner class StandardMemoryTests {
        private lateinit var module: Module
        private lateinit var instance: RawInstance
        private lateinit var memoryMap: Abi.MemoryMap

        private val readOnlyData = byteArrayOf(1, 2, 3)
        private val readWriteData = byteArrayOf(4, 5, 6)

        @BeforeEach
        fun setup() {
            val (mod, inst) = createInstance(
                roData = readOnlyData,
                rwData = readWriteData,
                stackSize = 4096u
            )
            module = mod
            instance = inst
            memoryMap = module.memoryMap()
        }

        @Test
        fun `read RO data`() {
            val roStart = memoryMap.roDataAddress()
            val buffer = ByteArray(3)
            instance.readMemoryInto(roStart, buffer).getOrThrow()
            assertArrayEquals(byteArrayOf(1, 2, 3), buffer)
        }

        @Test
        fun `read RO data with padding`() {
            val roStart = memoryMap.roDataAddress()
            val buffer = ByteArray(4)
            instance.readMemoryInto(roStart, buffer).getOrThrow()
            assertArrayEquals(byteArrayOf(1, 2, 3, 0), buffer)
        }

        @Test
        fun `read RW data`() {
            val rwStart = memoryMap.rwDataAddress
            val buffer = ByteArray(3)
            instance.readMemoryInto(rwStart, buffer).getOrThrow()
            assertArrayEquals(byteArrayOf(4, 5, 6), buffer)
        }

        @Test
        fun `read RW data with padding`() {
            val rwStart = memoryMap.rwDataAddress
            val buffer = ByteArray(4)
            instance.readMemoryInto(rwStart, buffer).getOrThrow()
            assertArrayEquals(byteArrayOf(4, 5, 6, 0), buffer)
        }

        @Test
        fun `write to RW data`() {
            val rwStart = memoryMap.rwDataAddress
            instance.writeMemory(rwStart, byteArrayOf(44)).getOrThrow()

            val buffer = ByteArray(4)
            instance.readMemoryInto(rwStart, buffer).getOrThrow()
            assertArrayEquals(byteArrayOf(44, 5, 6, 0), buffer)
        }

        @Test
        fun `write multiple bytes to RW data`() {
            val rwStart = memoryMap.rwDataAddress
            instance.writeMemory(rwStart, byteArrayOf(10, 20, 30)).getOrThrow()

            val buffer = ByteArray(4)
            instance.readMemoryInto(rwStart, buffer).getOrThrow()
            assertArrayEquals(byteArrayOf(10, 20, 30, 0), buffer)
        }

        @Test
        fun `read from stack`() {
            val stackStart = memoryMap.stackAddressLow()
            val buffer = ByteArray(2)
            instance.readMemoryInto(stackStart, buffer).getOrThrow()
            // Stack is zero-initialized
            assertArrayEquals(byteArrayOf(0, 0), buffer)
        }

        @Test
        fun `write to stack`() {
            val stackStart = memoryMap.stackAddressLow()
            instance.writeMemory(stackStart, byteArrayOf(1, 2)).getOrThrow()

            val buffer = ByteArray(2)
            instance.readMemoryInto(stackStart, buffer).getOrThrow()
            assertArrayEquals(byteArrayOf(1, 2), buffer)
        }

        @Test
        fun `stack end boundary`() {
            val stackEnd = memoryMap.stackAddressHigh
            val buffer = ByteArray(3)
            instance.readMemoryInto(stackEnd - 3u, buffer).getOrThrow()
            assertArrayEquals(byteArrayOf(0, 0, 0), buffer)
        }

        @Test
        fun `initial heap size is zero`() {
            assertEquals(0u, instance.heapSize())
        }

        @Test
        fun `sbrk allocates heap memory`() {
            val pageSize = memoryMap.pageSize
            val initialHeapSize = instance.heapSize()
            assertEquals(0u, initialHeapSize)

            // Allocate 1.25 pages worth
            val allocSize = pageSize + (pageSize / 4u)
            val newHeapEnd = instance.sbrk(allocSize)

            assertNotNull(newHeapEnd)
            assertEquals(allocSize, instance.heapSize())
        }

        @Test
        fun `sbrk returns previous heap end address`() {
            val heapBase = memoryMap.heapBase
            val allocSize = 4096u

            // sbrk returns the PREVIOUS heap end (start of newly allocated memory)
            // Following POSIX sbrk semantics and Boka implementation
            val prevHeapEnd = instance.sbrk(allocSize)

            assertNotNull(prevHeapEnd)
            // Initial heapSize was 0, so previous heap end equals heapBase
            assertEquals(heapBase, prevHeapEnd)
            // After allocation, heapSize should be updated
            assertEquals(allocSize, instance.heapSize())
        }

        @Test
        fun `write to allocated heap`() {
            val heapTop = instance.sbrk(4096u)
            assertNotNull(heapTop)

            // Write to beginning of new heap area
            val writeAddr = memoryMap.heapBase
            instance.writeMemory(writeAddr, byteArrayOf(42)).getOrThrow()

            val buffer = ByteArray(1)
            instance.readMemoryInto(writeAddr, buffer).getOrThrow()
            assertArrayEquals(byteArrayOf(42), buffer)
        }

        @Test
        fun `sbrk with zero size returns current heap top`() {
            // First allocate some heap
            instance.sbrk(4096u)
            val currentHeapTop = memoryMap.heapBase + instance.heapSize()

            // sbrk(0) should return current top without changing size
            val heapSize = instance.heapSize()
            val top = instance.sbrk(0u)

            assertEquals(heapSize, instance.heapSize())
            assertEquals(currentHeapTop, top)
        }

        @Test
        fun `multiple sbrk calls accumulate`() {
            val first = instance.sbrk(1024u)
            assertNotNull(first)
            assertEquals(1024u, instance.heapSize())

            val second = instance.sbrk(1024u)
            assertNotNull(second)
            assertEquals(2048u, instance.heapSize())

            val third = instance.sbrk(512u)
            assertNotNull(third)
            assertEquals(2560u, instance.heapSize())
        }
    }

    @Nested
    inner class MemoryBoundaryTests {
        private lateinit var module: Module
        private lateinit var instance: RawInstance
        private lateinit var memoryMap: Abi.MemoryMap

        @BeforeEach
        fun setup() {
            val (mod, inst) = createInstance(
                roData = byteArrayOf(1, 2, 3),
                rwData = byteArrayOf(4, 5, 6),
                stackSize = 4096u
            )
            module = mod
            instance = inst
            memoryMap = module.memoryMap()
        }

        @Test
        fun `cannot read from address zero`() {
            val buffer = ByteArray(1)
            val result = instance.readMemoryInto(0u, buffer)
            assertTrue(result.isFailure)
        }

        @Test
        fun `cannot read before RO data start`() {
            val roStart = memoryMap.roDataAddress()
            val buffer = ByteArray(1)
            // Try reading 1 byte before RO data starts
            val result = instance.readMemoryInto(roStart - 1u, buffer)
            assertTrue(result.isFailure)
        }

        @Test
        fun `read at RO data boundary`() {
            val roStart = memoryMap.roDataAddress()
            val buffer = ByteArray(1)
            val result = instance.readMemoryInto(roStart, buffer)
            assertTrue(result.isSuccess)
            assertArrayEquals(byteArrayOf(1), buffer)
        }

        @Test
        fun `read at RO data end`() {
            val roEnd = memoryMap.roDataAddress() + memoryMap.roDataSize - 1u
            val buffer = ByteArray(1)
            val result = instance.readMemoryInto(roEnd, buffer)
            assertTrue(result.isSuccess)
        }
    }

    @Nested
    inner class HeapGrowthTests {
        private lateinit var module: Module
        private lateinit var instance: RawInstance
        private lateinit var memoryMap: Abi.MemoryMap

        @BeforeEach
        fun setup() {
            val (mod, inst) = createInstance(
                roData = byteArrayOf(),
                rwData = byteArrayOf(),
                stackSize = 4096u
            )
            module = mod
            instance = inst
            memoryMap = module.memoryMap()
        }

        @Test
        fun `heap grows in page increments`() {
            val pageSize = memoryMap.pageSize

            // Allocate less than a page
            instance.sbrk(100u)

            // Should be able to write to full page
            val writeAddr = memoryMap.heapBase
            instance.writeMemory(writeAddr, ByteArray(pageSize.toInt())).getOrThrow()
        }

        @Test
        fun `heap respects max heap size`() {
            val maxHeap = memoryMap.maxHeapSize

            // Try to allocate more than max heap - should fail
            val result = instance.sbrk(maxHeap + 1u)
            assertNull(result)
        }

        @Test
        fun `heap data persists across allocations`() {
            val heapBase = memoryMap.heapBase

            // First allocation
            instance.sbrk(4096u)
            instance.writeMemory(heapBase, byteArrayOf(1, 2, 3)).getOrThrow()

            // Second allocation
            instance.sbrk(4096u)

            // Original data should still be there
            val buffer = ByteArray(3)
            instance.readMemoryInto(heapBase, buffer).getOrThrow()
            assertArrayEquals(byteArrayOf(1, 2, 3), buffer)
        }
    }

    @Nested
    inner class MemoryResetTests {
        private lateinit var module: Module
        private lateinit var instance: RawInstance
        private lateinit var memoryMap: Abi.MemoryMap

        @BeforeEach
        fun setup() {
            val (mod, inst) = createInstance(
                roData = byteArrayOf(1, 2, 3),
                rwData = byteArrayOf(4, 5, 6),
                stackSize = 4096u
            )
            module = mod
            instance = inst
            memoryMap = module.memoryMap()
        }

        @Test
        fun `reset restores RW data`() {
            val rwStart = memoryMap.rwDataAddress

            // Modify RW data
            instance.writeMemory(rwStart, byteArrayOf(99, 99, 99)).getOrThrow()

            // Verify modification
            val buffer1 = ByteArray(3)
            instance.readMemoryInto(rwStart, buffer1).getOrThrow()
            assertArrayEquals(byteArrayOf(99, 99, 99), buffer1)

            // Reset memory
            instance.resetMemory().getOrThrow()

            // RW data should be restored to original values
            val buffer2 = ByteArray(3)
            instance.readMemoryInto(rwStart, buffer2).getOrThrow()
            assertArrayEquals(byteArrayOf(4, 5, 6), buffer2)
        }

        @Test
        fun `reset clears heap size when memory is dirty`() {
            // Allocate some heap
            instance.sbrk(4096u)
            assertTrue(instance.heapSize() > 0u)

            // Write something to make memory dirty
            val rwStart = memoryMap.rwDataAddress
            instance.writeMemory(rwStart, byteArrayOf(99)).getOrThrow()

            // Reset memory (will call forceReset since memory is dirty)
            instance.resetMemory().getOrThrow()

            // Heap size should be zero after reset
            assertEquals(0u, instance.heapSize())
        }

        @Test
        fun `reset clears stack`() {
            val stackStart = memoryMap.stackAddressLow()

            // Write to stack
            instance.writeMemory(stackStart, byteArrayOf(1, 2, 3)).getOrThrow()

            // Reset memory
            instance.resetMemory().getOrThrow()

            // Stack should be zero
            val buffer = ByteArray(3)
            instance.readMemoryInto(stackStart, buffer).getOrThrow()
            assertArrayEquals(byteArrayOf(0, 0, 0), buffer)
        }
    }
}
