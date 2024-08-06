package scalecodec.writer

import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import java.io.IOException

class UInt32Writer : ScaleWriter<Int> {
    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: Int) {
        require(value >= 0) { "Negative values are not supported: $value" }
        wrt.directWrite(value and 0xff)
        wrt.directWrite((value shr 8) and 0xff)
        wrt.directWrite((value shr 16) and 0xff)
        wrt.directWrite((value shr 24) and 0xff)
    }
}
