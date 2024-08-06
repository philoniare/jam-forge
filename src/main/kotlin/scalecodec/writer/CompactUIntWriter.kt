package scalecodec.writer

import scalecodec.CompactMode
import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import java.io.IOException

class CompactUIntWriter : ScaleWriter<Int> {
    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: Int) {
        val mode = CompactMode.forNumber(value)
        var compact: Int
        var bytes: Int
        when (mode) {
            CompactMode.BIGINT -> {
                wrt.directWrite(mode.value.toInt())
                compact = value
                bytes = 4
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
            wrt.directWrite(compact and 0xff)
            compact = compact ushr 8
            bytes--
        }
    }
}
