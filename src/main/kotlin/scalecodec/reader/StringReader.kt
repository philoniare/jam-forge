package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader

/**
 * Read string, encoded as UTF-8 bytes
 */
class StringReader : ScaleReader<String> {
    override fun read(rdr: ScaleCodecReader): String = rdr.readString()
}
