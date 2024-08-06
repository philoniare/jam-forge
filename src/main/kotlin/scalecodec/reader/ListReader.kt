package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader

class ListReader<T>(private val scaleReader: ScaleReader<T>) : ScaleReader<List<T>> {

    override fun read(rdr: ScaleCodecReader): List<T> {
        val size = rdr.readCompactInt()
        return List(size) { rdr.read(scaleReader) }
    }
}
