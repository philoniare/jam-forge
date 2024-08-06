package scalecodec.writer

import scalecodec.CompactMode
import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import java.io.IOException
import java.math.BigInteger

class CompactBigIntWriter : ScaleWriter<BigInteger> {
    companion object {
        private val LONG_WRITER = CompactULongWriter()
    }

    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: BigInteger) {
        val mode = CompactMode.forNumber(value)

        val data = value.toByteArray()
        var length = data.size
        var pos = data.size - 1
        var limit = 0

        if (mode != CompactMode.BIGINT) {
            LONG_WRITER.write(wrt, value.toLong())
            return
        }

        // skip the first byte if it's 0
        if (data[0] == 0.toByte()) {
            length--
            limit++
        }

        wrt.directWrite(((length - 4) shl 2) + mode.value)
        while (pos >= limit) {
            wrt.directWrite(data[pos].toInt())
            pos--
        }
    }
}
