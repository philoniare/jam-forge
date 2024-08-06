package scalecodec.writer

import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import java.io.IOException

class UByteWriter : ScaleWriter<Int> {
    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: Int) {
        require(value in 0..0xff) { "Only values in range 0..255 are supported: $value" }
        wrt.directWrite(value)
    }
}
