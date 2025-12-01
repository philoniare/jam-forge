package io.forge.jam.safrole.accumulation

import io.forge.jam.pvm.program.ArcBytes
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.program.ProgramParts
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.nio.ByteBuffer
import java.nio.ByteOrder

/**
 * Tests for JAM blob format parsing.
 */
class StandardProgramTest {

    /**
     * Helper to create a JAM blob with specified parameters.
     */
    private fun createBlob(
        readOnlyLen: Int = 256,
        readWriteLen: Int = 512,
        heapPages: Int = 4,
        stackSize: Int = 1024,
        codeData: ByteArray = byteArrayOf(0, 0, 2, 1, 2, 0) // minimal code section
    ): ByteArray {
        val buffer = ByteBuffer.allocate(11 + readOnlyLen + readWriteLen + 4 + codeData.size)
            .order(ByteOrder.LITTLE_ENDIAN)

        // E_3(|o|) - 3 bytes little-endian
        buffer.put((readOnlyLen and 0xFF).toByte())
        buffer.put(((readOnlyLen shr 8) and 0xFF).toByte())
        buffer.put(((readOnlyLen shr 16) and 0xFF).toByte())

        // E_3(|w|) - 3 bytes little-endian
        buffer.put((readWriteLen and 0xFF).toByte())
        buffer.put(((readWriteLen shr 8) and 0xFF).toByte())
        buffer.put(((readWriteLen shr 16) and 0xFF).toByte())

        // E_2(z) - 2 bytes little-endian (heap pages)
        buffer.put((heapPages and 0xFF).toByte())
        buffer.put(((heapPages shr 8) and 0xFF).toByte())

        // E_3(s) - 3 bytes little-endian (stack size)
        buffer.put((stackSize and 0xFF).toByte())
        buffer.put(((stackSize shr 8) and 0xFF).toByte())
        buffer.put(((stackSize shr 16) and 0xFF).toByte())

        // o (ro_data)
        buffer.put(ByteArray(readOnlyLen) { 0x01 })

        // w (rw_data)
        buffer.put(ByteArray(readWriteLen) { 0x02 })

        // E_4(|c|) - 4 bytes little-endian (code length)
        buffer.put((codeData.size and 0xFF).toByte())
        buffer.put(((codeData.size shr 8) and 0xFF).toByte())
        buffer.put(((codeData.size shr 16) and 0xFF).toByte())
        buffer.put(((codeData.size shr 24) and 0xFF).toByte())

        // c (code+jumptable)
        buffer.put(codeData)

        return buffer.array()
    }

    @Test
    fun `parse JAM blob with default parameters`() {
        val blob = createBlob()
        val parts = ProgramParts.fromJamBytes(ArcBytes.fromStatic(blob)).getOrThrow()

        assertEquals(256u, parts.roDataSize)
        // rwDataSize includes heap pages (4 * 4096 = 16384)
        assertEquals(512u + (4u * 4096u), parts.rwDataSize)
        assertEquals(1024u, parts.stackSize)
        assertEquals(256, parts.roData.toByteArray().size)
        assertEquals(512, parts.rwData.toByteArray().size)
    }

    @Test
    fun `parse JAM blob and create ProgramBlob`() {
        val blob = createBlob()
        val parts = ProgramParts.fromJamBytes(ArcBytes.fromStatic(blob)).getOrThrow()
        val programBlob = ProgramBlob.fromParts(parts).getOrThrow()

        // Code section: [0, 0, 2, 1, 2, 0] = 0 jump table entries, 0 entry size, 2 bytes code
        assertEquals(0.toByte(), programBlob.jumpTableEntrySize)
        assertEquals(2, programBlob.code.toByteArray().size)
    }

    @Test
    fun `parse JAM blob with minimal data`() {
        val blob = createBlob(
            readOnlyLen = 0,
            readWriteLen = 0,
            heapPages = 0,
            stackSize = 4096 // minimum stack size
        )
        val parts = ProgramParts.fromJamBytes(ArcBytes.fromStatic(blob)).getOrThrow()

        assertEquals(0u, parts.roDataSize)
        assertEquals(0u, parts.rwDataSize)
        assertEquals(4096u, parts.stackSize)
    }

    @Test
    fun `parse JAM blob with large data sections`() {
        val blob = createBlob(
            readOnlyLen = 4096,
            readWriteLen = 8192,
            heapPages = 16,
            stackSize = 16384
        )
        val parts = ProgramParts.fromJamBytes(ArcBytes.fromStatic(blob)).getOrThrow()

        assertEquals(4096u, parts.roDataSize)
        assertEquals(8192u + (16u * 4096u), parts.rwDataSize) // rw + heap
        assertEquals(16384u, parts.stackSize)
        assertEquals(4096, parts.roData.toByteArray().size)
        assertEquals(8192, parts.rwData.toByteArray().size)
    }

    @Test
    fun `ro data content is preserved`() {
        val blob = createBlob(readOnlyLen = 3, readWriteLen = 0, heapPages = 0, stackSize = 4096)
        // Manually patch the ro_data bytes (after header at offset 11)
        val blobBytes = blob.copyOf()
        blobBytes[11] = 0xAA.toByte()
        blobBytes[12] = 0xBB.toByte()
        blobBytes[13] = 0xCC.toByte()

        val parts = ProgramParts.fromJamBytes(ArcBytes.fromStatic(blobBytes)).getOrThrow()

        assertEquals(3, parts.roData.toByteArray().size)
        assertEquals(0xAA.toByte(), parts.roData.toByteArray()[0])
        assertEquals(0xBB.toByte(), parts.roData.toByteArray()[1])
        assertEquals(0xCC.toByte(), parts.roData.toByteArray()[2])
    }

    @Test
    fun `rw data content is preserved`() {
        val blob = createBlob(readOnlyLen = 0, readWriteLen = 3, heapPages = 0, stackSize = 4096)
        // Manually patch the rw_data bytes (after header at offset 11)
        val blobBytes = blob.copyOf()
        blobBytes[11] = 0x11.toByte()
        blobBytes[12] = 0x22.toByte()
        blobBytes[13] = 0x33.toByte()

        val parts = ProgramParts.fromJamBytes(ArcBytes.fromStatic(blobBytes)).getOrThrow()

        assertEquals(3, parts.rwData.toByteArray().size)
        assertEquals(0x11.toByte(), parts.rwData.toByteArray()[0])
        assertEquals(0x22.toByte(), parts.rwData.toByteArray()[1])
        assertEquals(0x33.toByte(), parts.rwData.toByteArray()[2])
    }
}
