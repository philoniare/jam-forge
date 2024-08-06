package scalecodec.writer

import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import java.io.IOException

class ULong32Writer : ScaleWriter<Long> {
    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: Long) {
        require(value >= 0) { "Negative values are not supported: $value" }
        require(value <= 0xff_ff_ff_ffL) { "Value is too high: $value" }
        wrt.directWrite((value and 0xff).toInt())
        wrt.directWrite(((value shr 8) and 0xff).toInt())
        wrt.directWrite(((value shr 16) and 0xff).toInt())
        wrt.directWrite(((value shr 24) and 0xff).toInt())
    }
}
