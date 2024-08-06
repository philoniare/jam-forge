package scalecodec.writer

import scalecodec.ScaleCodecWriter
import scalecodec.ScaleWriter
import scalecodec.UnionValue
import java.io.IOException

class UnionWriter<T> : ScaleWriter<UnionValue<T>> {
    private val mapping: List<ScaleWriter<T>>

    constructor(mapping: List<ScaleWriter<out T>>) {
        @Suppress("UNCHECKED_CAST")
        this.mapping = mapping as List<ScaleWriter<T>>
    }

    constructor(vararg mapping: ScaleWriter<out T>) : this(mapping.toList())

    @Throws(IOException::class)
    override fun write(wrt: ScaleCodecWriter, value: UnionValue<T>) {
        wrt.directWrite(value.index)
        mapping[value.index].write(wrt, value.value)
    }
}
