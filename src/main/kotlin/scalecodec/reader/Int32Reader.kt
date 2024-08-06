package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader
import java.nio.ByteBuffer
import java.nio.ByteOrder

/**
 * Read Kotlin Int encoded as 4 byte SCALE value. Please note that since Kotlin Int is a signed type, it may
 * read negative values for some of the byte representations (i.e. when the highest bit is set to 1). If you expect
 * to read positive numbers for all the possible range, you should use UInt32Reader, which returns UInt values.
 *
 * @see UInt32Reader
 */
class Int32Reader : ScaleReader<Int> {
    override fun read(rdr: ScaleCodecReader): Int {
        return ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).apply {
            put(rdr.readByte())
            put(rdr.readByte())
            put(rdr.readByte())
            put(rdr.readByte())
            flip()
        }.int
    }
}
