package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader
import java.math.BigInteger

class UInt64Reader : ScaleReader<BigInteger> {
    override fun read(rdr: ScaleCodecReader): BigInteger {
        return (0..7).fold(BigInteger.ZERO) { acc, i ->
            acc.add(BigInteger.valueOf(rdr.readUByte().toLong()).shiftLeft(8 * i))
        }
    }
}
