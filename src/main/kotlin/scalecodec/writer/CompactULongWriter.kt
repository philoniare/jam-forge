package scalecodec.writer

import scalecodec.CompactMode
import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import java.io.IOException
import java.math.BigInteger

class CompactULongWriter : ScaleWriter<Long> {
    companion object {
        private val BIGINT_WRITER = CompactBigIntWriter()
    }

    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: Long) {
        val mode = CompactMode.forNumber(value)
        var compact: Long
        var bytes: Int
        when (mode) {
            CompactMode.BIGINT -> {
                BIGINT_WRITER.write(wrt, BigInteger.valueOf(value))
                return
            }

            CompactMode.SINGLE -> {
                compact = (value shl 2) + mode.value
                bytes = 1
            }

            CompactMode.TWO -> {
                compact = (value shl 2) + mode.value
                bytes = 2
            }

            else -> {
                compact = (value shl 2) + mode.value
                bytes = 4
            }
        }
        while (bytes > 0) {
            wrt.directWrite((compact and 0xffL).toInt())
            compact = compact ushr 8
            bytes--
        }
    }
}
