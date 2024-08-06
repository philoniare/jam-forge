package scalecodec

import org.apache.commons.codec.binary.Hex
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.assertThrows
import org.mockito.Mockito.mock
import org.mockito.Mockito.verify
import java.io.ByteArrayOutputStream
import java.io.OutputStream
import kotlin.test.Test
import kotlin.test.assertEquals

class ScaleCodecWriterTest {

    private lateinit var buf: ByteArrayOutputStream
    private lateinit var codec: ScaleCodecWriter

    @BeforeEach
    fun setup() {
        buf = ByteArrayOutputStream()
        codec = ScaleCodecWriter(buf)
    }

    @Test
    fun `Flush on output stream`() {
        val os = mock(OutputStream::class.java)
        val codec = ScaleCodecWriter(os)
        codec.flush()
        verify(os).flush()
    }

    @Test
    fun `Close on output stream`() {
        val os = mock(OutputStream::class.java)
        val codec = ScaleCodecWriter(os)
        codec.close()
        verify(os).close()
    }

    @Test
    fun `Write unsigned 8-bit integer`() {
        codec.directWrite(0)
        val act = buf.toByteArray()
        assertEquals("00", Hex.encodeHexString(act))
    }

    @Test
    fun `Write java-negative integer`() {
        codec.directWrite(240)
        val act = buf.toByteArray()
        assertEquals("f0", Hex.encodeHexString(act))
    }

    @Test
    fun `Write unsigned 16-bit integer`() {
        codec.writeUint16(42)
        val act = buf.toByteArray()
        assertEquals("2a00", Hex.encodeHexString(act))
    }

    @Test
    fun `Write unsigned 32-bit integer`() {
        codec.writeUint32(16777215)
        val act = buf.toByteArray()
        assertEquals("ffffff00", Hex.encodeHexString(act))
    }

    @Test
    fun `Write unsigned 32-bit long`() {
        codec.writeUint32(16777215L)
        val act = buf.toByteArray()
        assertEquals("ffffff00", Hex.encodeHexString(act))
    }

    fun `Write byte array`(hex: String, value: String) {
        codec.writeAsList(Hex.decodeHex(value))
        assertEquals(hex, Hex.encodeHexString(buf.toByteArray()))
    }

    @Test
    fun `Write 256 bit value`() {
        codec.writeUint256(Hex.decodeHex("bb931fd17f85fb26e8209eb7af5747258163df29a7dd8f87fa7617963fcfa1aa"))
        val act = buf.toByteArray()
        assertEquals("bb931fd17f85fb26e8209eb7af5747258163df29a7dd8f87fa7617963fcfa1aa", Hex.encodeHexString(act))
    }

    @Test
    fun `Error to write short 256 bit value`() {
        assertThrows<IllegalArgumentException> {
            codec.writeUint256(Hex.decodeHex("bb931fd17f85fb26e8209eb7af5747258163df29a7dd8f87fa7617963fcfa1"))
        }
    }

    @Test
    fun `Write status message`() {
        codec.writeUint32(1536) // version
        codec.writeUint32(768) // min version
        codec.directWrite(0) // roles
        codec.directWrite(1) //?
        codec.writeUint32(381) // height
        codec.writeUint256(Hex.decodeHex("bb931fd17f85fb26e8209eb7af5747258163df29a7dd8f87fa7617963fcfa1aa")) // best hash
        codec.writeUint256(Hex.decodeHex("b0a8d493285c2df73290dfb7e61f870f17b41801197a149ca93654499ea3dafe")) // genesis
        codec.writeUint16(0x0004) //
        val act = buf.toByteArray()
        assertEquals(
            "000600000003000000017d010000bb931fd17f85fb26e8209eb7af5747258163df29a7dd8f87fa7617963fcfa1aab0a8d493285c2df73290dfb7e61f870f17b41801197a149ca93654499ea3dafe0400",
            Hex.encodeHexString(act)
        )
    }
}
