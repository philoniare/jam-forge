package scalecodec

import scalecodec.writer.*
import java.io.Closeable
import java.io.IOException
import java.io.OutputStream
import java.math.BigInteger

class ScaleCodecWriter(private val out: OutputStream) : Closeable {

    companion object {
        val COMPACT_UINT = CompactUIntWriter()
        val COMPACT_BIGINT = CompactBigIntWriter()
        val UINT16 = UInt16Writer()
        val UINT32 = UInt32Writer()
        val UINT128 = UInt128Writer()
        val ULONG32 = ULong32Writer()
        val BOOL = BoolWriter()
        val BOOL_OPT = BoolOptionalWriter()
    }

    @Throws(IOException::class)
    fun writeUint256(value: ByteArray) {
        require(value.size == 32) { "Value must be 32 byte array" }
        writeByteArray(value)
    }

    @Throws(IOException::class)
    fun writeByteArray(value: ByteArray) {
        out.write(value, 0, value.size)
    }

    @Throws(IOException::class)
    fun writeAsList(value: ByteArray) {
        writeCompact(value.size)
        out.write(value, 0, value.size)
    }

    /**
     * Write the byte into output stream as-is directly, the input is supposed to be already encoded
     *
     * @param b byte to write
     * @throws IOException if failed to write
     */
    @Throws(IOException::class)
    fun directWrite(b: Int) {
        out.write(b)
    }

    /**
     * Write the bytes into output stream as-is directly, the input is supposed to be already encoded
     *
     * @param b   bytes to write
     * @param off offset
     * @param len length
     * @throws IOException if failed to write
     */
    @Throws(IOException::class)
    fun directWrite(b: ByteArray, off: Int, len: Int) {
        out.write(b, off, len)
    }

    @Throws(IOException::class)
    fun flush() {
        out.flush()
    }

    @Throws(IOException::class)
    override fun close() {
        out.close()
    }

    @Throws(IOException::class)
    fun <T> write(writer: ScaleWriter<T>, value: T) {
        writer.write(this, value)
    }

    @Throws(IOException::class)
    fun writeByte(value: Int) {
        directWrite(value)
    }

    @Throws(IOException::class)
    fun writeByte(value: Byte) {
        directWrite(value.toInt())
    }

    @Throws(IOException::class)
    fun writeUint16(value: Int) {
        UINT16.write(this, value)
    }

    @Throws(IOException::class)
    fun writeUint32(value: Int) {
        UINT32.write(this, value)
    }

    @Throws(IOException::class)
    fun writeUint32(value: Long) {
        ULONG32.write(this, value)
    }

    @Throws(IOException::class)
    fun writeUint128(value: BigInteger) {
        UINT128.write(this, value)
    }

    @Throws(IOException::class)
    fun writeCompact(value: Int) {
        COMPACT_UINT.write(this, value)
    }

    @Throws(IOException::class)
    @Suppress("UNCHECKED_CAST")
    fun <T> writeOptional(writer: ScaleWriter<T>, value: T?) {
        when (writer) {
            is BoolOptionalWriter -> (writer as ScaleWriter<Boolean?>).write(this, value as? Boolean?)
            is BoolWriter -> BOOL_OPT.write(this, value as? Boolean)
            else -> {
                BOOL.write(this, value != null)
                if (value != null) {
                    writer.write(this, value)
                }
            }
        }
    }
}
