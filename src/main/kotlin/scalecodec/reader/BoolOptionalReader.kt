package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader

class BoolOptionalReader : ScaleReader<Boolean?> {
    override fun read(rdr: ScaleCodecReader): Boolean? {
        return when (val b = rdr.readByte().toInt()) {
            0 -> null
            1 -> false
            2 -> true
            else -> throw IllegalStateException("Not a boolean option: $b")
        }
    }
}
