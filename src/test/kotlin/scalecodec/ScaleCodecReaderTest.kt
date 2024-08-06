package scalecodec

import org.apache.commons.codec.binary.Hex
import org.junit.jupiter.api.assertThrows
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ScaleCodecReaderTest {

    @Test
    fun `Reads unsigned 8-bit integer`() {
        val codec = ScaleCodecReader(Hex.decodeHex("45"))
        assertTrue(codec.hasNext())
        assertEquals(69.toByte(), codec.readByte())
        assertFalse(codec.hasNext())
    }

    @Test
    fun `Reads unsigned 16-bit integer`() {
        val codec = ScaleCodecReader(Hex.decodeHex("2a00"))
        assertTrue(codec.hasNext())
        assertEquals(42, codec.readUint16())
        assertFalse(codec.hasNext())
    }

    @Test
    fun `Seek and read`() {
        val codec = ScaleCodecReader(Hex.decodeHex("2a00"))
        assertEquals(42, codec.readUint16())
        assertFalse(codec.hasNext())
        codec.seek(0)
        assertTrue(codec.hasNext())
        assertEquals(42, codec.readUint16())
        assertFalse(codec.hasNext())
    }

    @Test
    fun `Cannot seek below`() {
        val codec = ScaleCodecReader(Hex.decodeHex("2a00"))
        assertThrows<IllegalArgumentException> {
            codec.seek(-1)
        }
    }

    @Test
    fun `Cannot seek over`() {
        val codec = ScaleCodecReader(Hex.decodeHex("2a00"))
        assertThrows<IllegalArgumentException> {
            codec.seek(5)
        }
        assertThrows<IllegalArgumentException> {
            codec.seek(2)
        }
    }

    @Test
    fun `Can skip backwards`() {
        val codec = ScaleCodecReader(Hex.decodeHex("2a00"))
        assertEquals(42, codec.readUint16())
        codec.skip(-2)
        assertEquals(42, codec.readUint16())
    }

    @Test
    fun `Cannot skip below`() {
        val codec = ScaleCodecReader(Hex.decodeHex("2a00"))
        codec.readUint16()
        assertThrows<IllegalArgumentException> {
            codec.skip(-3)
        }
    }

    @Test
    fun `Error to read with null reader`() {
        val codec = ScaleCodecReader(Hex.decodeHex("2a00"))
        assertThrows<IllegalArgumentException> {
            codec.read(null)
        }
    }

    @Test
    fun `Reads java-negative byte`() {
        val codec = ScaleCodecReader(Hex.decodeHex("f0"))
        assertTrue(codec.hasNext())
        assertEquals(240, codec.readUByte())
        assertFalse(codec.hasNext())
    }

    @Test
    fun `Read status message`() {
        val msg =
            Hex.decodeHex(
                "000600000003000000017d010000bb931fd17f85fb26e8209eb7af5747258163df29a7dd8f87fa7617963fcfa1aab0a8d493285c2df73290dfb7e61f870f17b41801197a149ca93654499ea3dafe0400"
            )
        val rdr = ScaleCodecReader(msg)

        assertEquals(1536, rdr.readUint32()) // version
        assertEquals(768, rdr.readUint32()) // min version
        assertEquals(0.toByte(), rdr.readByte()) // roles
        rdr.skip(1) // ?
        assertEquals(381, rdr.readUint32()) // height
        assertEquals(
            "bb931fd17f85fb26e8209eb7af5747258163df29a7dd8f87fa7617963fcfa1aa",
            Hex.encodeHexString(rdr.readUint256())
        ) // best hash
        assertEquals(
            "b0a8d493285c2df73290dfb7e61f870f17b41801197a149ca93654499ea3dafe",
            Hex.encodeHexString(rdr.readUint256())
        ) // genesis hash
    }

    @Test
    fun `Read byte array`() {
        val testCases = listOf(
            "00" to "",
            "0401" to "01"
        )

        testCases.forEach { (hex, expectedValue) ->
            assertEquals(expectedValue, Hex.encodeHexString(ScaleCodecReader(Hex.decodeHex(hex)).readByteArray()))
        }
    }

    @Test
    fun `Read fixed byte array`() {
        val hex = "bb931fd17f85fb26e8209eb7af5747258163df29a7dd8f87fa7617963fcfa1aa"
        assertEquals(hex, Hex.encodeHexString(ScaleCodecReader(Hex.decodeHex(hex)).readByteArray(32)))
    }
}
