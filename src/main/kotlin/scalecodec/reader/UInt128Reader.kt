package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader
import java.math.BigInteger

class UInt128Reader : ScaleReader<BigInteger> {
    companion object {
        const val SIZE_BYTES = 16

        fun reverse(value: ByteArray) {
            value.reverse()
        }
    }

    override fun read(rdr: ScaleCodecReader): BigInteger {
        val value = rdr.readByteArray(SIZE_BYTES)
        reverse(value)
        return BigInteger(1, value)
    }
}
