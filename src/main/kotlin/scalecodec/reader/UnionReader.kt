package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader
import scalecodec.UnionValue

class UnionReader<T> : ScaleReader<UnionValue<T>> {

    private val mapping: List<ScaleReader<out T>>

    constructor(mapping: List<ScaleReader<out T>>) {
        this.mapping = mapping
    }

    constructor(vararg mapping: ScaleReader<out T>) : this(mapping.toList())

    override fun read(rdr: ScaleCodecReader): UnionValue<T> {
        val index = rdr.readUByte()
        if (mapping.size <= index) {
            throw IllegalStateException("Unknown type index: $index")
        }
        val value = mapping[index].read(rdr)
        return UnionValue(index, value)
    }
}
