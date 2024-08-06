package scalecodec.writer

import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import java.io.IOException

class BoolWriter : ScaleWriter<Boolean> {
    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: Boolean) {
        wrt.directWrite(if (value) 1 else 0)
    }
}
