package scalecodec.reader

import scalecodec.CompactMode
import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader
import java.math.BigInteger

class CompactBigIntReader : ScaleReader<BigInteger> {

    companion object {
        private val intReader = CompactUIntReader()
    }

    override fun read(rdr: ScaleCodecReader): BigInteger {
        val type = rdr.readUByte()
        val mode = CompactMode.byValue((type and 0b11).toByte())
        if (mode != CompactMode.BIGINT) {
            rdr.skip(-1)
            val value = intReader.read(rdr)
            return BigInteger.valueOf(value.toLong())
        }
        val len = (type shr 2) + 4
        val value = rdr.readByteArray(len)
        // LE encoded, so need to reverse it
        value.reverse()
        // unsigned, i.e. always positive, signum=1
        return BigInteger(1, value)
    }
}
