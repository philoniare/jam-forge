package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader

class BoolReader : ScaleReader<Boolean> {
    override fun read(rdr: ScaleCodecReader): Boolean {
        return when (val b = rdr.readByte().toInt()) {
            0 -> false
            1 -> true
            else -> throw IllegalStateException("Not a boolean value: $b")
        }
    }
}
