package scalecodec.reader

import scalecodec.ScaleCodecReader
import scalecodec.ScaleReader

/**
 * Read a Kotlin enum value. The reader reads one byte and returns an Enum value whose ordinal value is equal to it.
 *
 * If you need to read an enumeration with assigned value, i.e. Rust style enum, you should use UnionReader instead.
 *
 * @param T type of Enum
 * @see UnionReader
 */
class EnumReader<T : Enum<T>>(private val values: Array<T>) : ScaleReader<T> {

    init {
        require(values.isNotEmpty()) { "List of enums is empty" }
    }

    /**
     * Define reader by specifying list of possible values. In most of the cases it would be:
     * EnumReader(MyEnum.values())
     *
     * @param values list of enum values
     */
    constructor(enumClass: Class<T>) : this(
        enumClass.enumConstants ?: throw IllegalArgumentException("${enumClass.name} is not an enum class")
    )

    override fun read(rdr: ScaleCodecReader): T {
        val id = rdr.readUByte()
        return values.find { it.ordinal == id }
            ?: throw IllegalStateException("Unknown enum value: $id")
    }
}
