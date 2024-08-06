package scalecodec.writer

import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import java.io.IOException

class ListWriter<T>(private val scaleWriter: ScaleWriter<T>) : ScaleWriter<List<T>> {
    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: List<T>) {
        wrt.writeCompact(value.size)
        for (item in value) {
            scaleWriter.write(wrt, item)
        }
    }
}
