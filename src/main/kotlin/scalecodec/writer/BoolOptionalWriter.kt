package scalecodec.writer

import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import java.io.IOException

class BoolOptionalWriter : ScaleWriter<Boolean?> {
    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: Boolean?) {
        when (value) {
            null -> wrt.directWrite(0)
            true -> wrt.directWrite(2)
            false -> wrt.directWrite(1)
        }
    }
}
