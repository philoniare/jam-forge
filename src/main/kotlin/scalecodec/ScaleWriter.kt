package scalecodec

import java.io.IOException

interface ScaleWriter<T> {
    @Throws(IOException::class)
    fun write(wrt: ScaleCodecWriter, value: T)
}
