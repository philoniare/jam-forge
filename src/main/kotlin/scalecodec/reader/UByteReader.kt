package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader

class UByteReader : ScaleReader<Int> {
    override fun read(rdr: ScaleCodecReader): Int {
        val x = rdr.readByte().toInt()
        return if (x < 0) {
            256 + x
        } else {
            x
        }
    }
}
