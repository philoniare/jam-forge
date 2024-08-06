package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader

class UInt32Reader : ScaleReader<Long> {
    override fun read(rdr: ScaleCodecReader): Long {
        return rdr.readUByte().toLong() or
            (rdr.readUByte().toLong() shl 8) or
            (rdr.readUByte().toLong() shl 16) or
            (rdr.readUByte().toLong() shl 24)
    }
}
