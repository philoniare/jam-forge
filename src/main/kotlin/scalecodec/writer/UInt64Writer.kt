package scalecodec.writer

import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import java.io.IOException
import java.math.BigInteger

class UInt64Writer : ScaleWriter<BigInteger> {
    companion object {
        val MAX_UINT64 = BigInteger("18446744073709551615")
    }

    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: BigInteger) {
        require(value >= BigInteger.ZERO) { "Negative values are not supported: $value" }
        require(value <= MAX_UINT64) { "Value is too big for 64 bits: $value" }

        for (shift in 0..56 step 8) {
            wrt.directWrite(value.shiftRight(shift).and(BigInteger.valueOf(255)).toInt())
        }
    }
}
