package scalecodec

import scalecodec.reader.*
import java.math.BigInteger
import kotlin.math.abs

/**
 * SCALE codec reader
 */
class ScaleCodecReader(private val source: ByteArray) {

    companion object {
        val UBYTE = UByteReader()
        val UINT16 = UInt16Reader()
        val UINT32 = UInt32Reader()
        val UINT128 = UInt128Reader()
        val INT32 = Int32Reader()
        val COMPACT_UINT = CompactUIntReader()
        val COMPACT_BIGINT = CompactBigIntReader()
        val BOOL = BoolReader()
        val BOOL_OPTIONAL = BoolOptionalReader()
        val STRING = StringReader()
    }

    private var pos = 0

    /**
     * @return true if it has more elements
     */
    fun hasNext(): Boolean = pos < source.size

    /**
     * Move reader position forward (or backward for negative value)
     *
     * @param len amount to bytes to skip
     */
    fun skip(len: Int) {
        if (len < 0 && abs(len) > pos) {
            throw IllegalArgumentException("Position cannot be negative: $pos $len")
        }
        pos += len
    }

    /**
     * Specify a new position
     *
     * @param pos position
     */
    fun seek(pos: Int) {
        when {
            pos < 0 -> throw IllegalArgumentException("Position cannot be negative: $pos")
            pos >= source.size -> throw IllegalArgumentException(
                "Position $pos must be strictly smaller than source length: ${source.size}"
            )

            else -> this.pos = pos
        }
    }

    /**
     * @return a next single byte from reader
     */
    fun readByte(): Byte {
        if (!hasNext()) {
            throw IndexOutOfBoundsException("Cannot read $pos of ${source.size}")
        }
        return source[pos++]
    }

    /**
     * Read complex value from the reader
     *
     * @param scaleReader reader implementation
     * @return read value
     */
    fun <T> read(scaleReader: ScaleReader<T>?): T {
        requireNotNull(scaleReader) { "ScaleReader cannot be null" }
        return scaleReader.read(this)
    }

    fun readUByte(): Int = UBYTE.read(this)

    fun readUint16(): Int = UINT16.read(this)

    fun readUint32(): Long = UINT32.read(this)

    fun readUint128(): BigInteger = UINT128.read(this)

    fun readCompactInt(): Int = COMPACT_UINT.read(this)

    fun readBoolean(): Boolean = BOOL.read(this)

    /**
     * Read optional value from the reader
     *
     * @param scaleReader reader implementation
     * @return optional read value
     */
    @Suppress("UNCHECKED_CAST")
    fun <T> readOptional(scaleReader: ScaleReader<T>): T? {
        return when (scaleReader) {
            is BoolReader -> BOOL.read(this) as T?
            is BoolOptionalReader -> BOOL_OPTIONAL.read(this) as T?
            else -> if (readBoolean()) read(scaleReader) else null
        }
    }

    fun readUint256(): ByteArray = readByteArray(32)

    fun readByteArray(): ByteArray {
        val len = readCompactInt()
        return readByteArray(len)
    }

    fun readByteArray(len: Int): ByteArray {
        return source.slice(pos until (pos + len)).toByteArray().also {
            pos += len
        }
    }

    /**
     * Read string, encoded as UTF-8 bytes
     *
     * @return string value
     */
    fun readString(): String = String(readByteArray())
}
