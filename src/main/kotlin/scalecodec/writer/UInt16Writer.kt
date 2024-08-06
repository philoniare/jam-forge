package scalecodec.writer

import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import java.io.IOException

class UInt16Writer : ScaleWriter<Int> {
    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: Int) {
        wrt.directWrite(value and 0xff)
        wrt.directWrite((value shr 8) and 0xff)
    }
}
