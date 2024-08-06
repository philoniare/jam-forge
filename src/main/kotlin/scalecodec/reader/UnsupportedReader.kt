package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader

class UnsupportedReader<T>(
    private val message: String = "Reading an unsupported value"
) : ScaleReader<T> {

    override fun read(rdr: ScaleCodecReader): T {
        throw IllegalStateException(message)
    }
}
