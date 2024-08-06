package scalecodec.writer

import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import scalecodec.reader.UInt128Reader
import java.io.IOException
import java.math.BigInteger

class UInt128Writer : ScaleWriter<BigInteger> {
    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: BigInteger) {
        require(value.signum() >= 0) { "Negative numbers are not supported by Uint128" }
        val array = value.toByteArray()
        var pos = 0
        // sometimes BigInteger gives an extra zero byte at the start of the array
        if (array[0] == 0.toByte()) {
            pos++
        }
        val len = array.size - pos
        require(len <= UInt128Reader.SIZE_BYTES) { "Value is too big for 128 bits. Has: ${len * 8} bits" }
        val encoded = ByteArray(UInt128Reader.SIZE_BYTES)
        array.copyInto(encoded, encoded.size - len, pos, array.size)
        UInt128Reader.reverse(encoded)
        wrt.directWrite(encoded, 0, UInt128Reader.SIZE_BYTES)
    }
}
