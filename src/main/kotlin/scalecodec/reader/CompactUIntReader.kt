package scalecodec.reader

import scalecodec.CompactMode
import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader

class CompactUIntReader : ScaleReader<Int> {

    /**
     * @param rdr reader with the encoded data
     * @return integer value
     * @throws UnsupportedOperationException if the value is encoded with more than four bytes (use CompactBigIntReader)
     */
    override fun read(rdr: ScaleCodecReader): Int {
        val i = rdr.readUByte()
        return when (val mode = CompactMode.byValue((i and 0b11).toByte())) {
            CompactMode.SINGLE -> i shr 2
            CompactMode.TWO -> (i shr 2) + (rdr.readUByte() shl 6)
            CompactMode.FOUR -> (i shr 2) +
                (rdr.readUByte() shl 6) +
                (rdr.readUByte() shl (6 + 8)) +
                (rdr.readUByte() shl (6 + 2 * 8))

            else -> throw UnsupportedOperationException("Mode $mode is not implemented")
        }
    }
}
