package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader

class UInt16Reader : ScaleReader<Int> {
    override fun read(rdr: ScaleCodecReader): Int {
        return rdr.readUByte() or (rdr.readUByte() shl 8)
    }
}
