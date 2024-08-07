package io.forge.jam

object abi {
    /// The minimum required alignment of runtime code pointers.
    const val VM_CODE_ADDRESS_ALIGNMENT: UInt = 2u

    /// The maximum byte size of the code blob.
    val VM_MAXIMUM_CODE_SIZE: UInt = 32u * 1024u * 1024u

    /// The maximum number of functions the program can import.
    const val VM_MAXIMUM_IMPORT_COUNT: UInt = 1024u

    /// The maximum number of entries in the jump table.
    val VM_MAXIMUM_JUMP_TABLE_ENTRIES: UInt = 16u * 1024u * 1024u


}

object Constants {
    // The magic bytes with which every program blob must start with.
    val BLOB_MAGIC: UByteArray =
        ubyteArrayOf('P'.code.toByte().toUByte(), 'V'.code.toByte().toUByte(), 'M'.code.toByte().toUByte(), 0.toUByte())

    const val MAX_VARINT_LENGTH: Int = 5
    val MAX_INSTRUCTION_LENGTH: Int = 2 + MAX_VARINT_LENGTH * 2

    const val SECTION_MEMORY_CONFIG: UByte = 1u
    const val SECTION_RO_DATA: UByte = 2u
    const val SECTION_RW_DATA: UByte = 3u
    const val SECTION_IMPORTS: UByte = 4u
    const val SECTION_EXPORTS: UByte = 5u
    const val SECTION_CODE_AND_JUMP_TABLE: UByte = 6u
    const val SECTION_OPT_DEBUG_STRINGS: UByte = 128u
    const val SECTION_OPT_DEBUG_LINE_PROGRAMS: UByte = 129u
    const val SECTION_OPT_DEBUG_LINE_PROGRAM_RANGES: UByte = 130u
    const val SECTION_END_OF_FILE: UByte = 0u

    const val BLOB_VERSION_V1: UByte = 1u

    const val VERSION_DEBUG_LINE_PROGRAM_V1: UByte = 1u

    const val BITMASK_MAX: Int = 24
}

class JumpTable(
    private val blob: UByteArray,
    private val entrySize: UInt
) {
    fun isEmpty(): Boolean = size == 0u

    val size: UInt
        get() = if (entrySize == 0u) 0u else (blob.size.toUInt() / entrySize)

    fun getByAddress(address: UInt): UInt? {
        if ((address and (abi.VM_CODE_ADDRESS_ALIGNMENT - 1u)) != 0u || address == 0u) {
            return null
        }
        return getByIndex((address - abi.VM_CODE_ADDRESS_ALIGNMENT) / abi.VM_CODE_ADDRESS_ALIGNMENT)
    }

    fun getByIndex(index: UInt): UInt? {
        if (entrySize == 0u) {
            return null
        }

        val start = (index * entrySize).toLong()
        val end = (start + entrySize.toLong()).coerceAtMost(blob.size.toLong())
        if (start >= blob.size || start < 0 || end > blob.size || end <= start) {
            return null
        }

        return when (entrySize.toInt()) {
            1 -> blob[start.toInt()].toUInt()
            2 -> (blob[start.toInt()].toUInt() or (blob[start.toInt() + 1].toUInt() shl 8))
            3 -> (blob[start.toInt()].toUInt() or
                (blob[start.toInt() + 1].toUInt() shl 8) or
                (blob[start.toInt() + 2].toUInt() shl 16))

            4 -> (blob[start.toInt()].toUInt() or
                (blob[start.toInt() + 1].toUInt() shl 8) or
                (blob[start.toInt() + 2].toUInt() shl 16) or
                (blob[start.toInt() + 3].toUInt() shl 24))

            else -> throw IllegalStateException("Unexpected entry size")
        }
    }

    fun iterator(): Iterator<UInt> = JumpTableIterator(this)
}

class JumpTableIterator(private val jumpTable: JumpTable) : Iterator<UInt> {
    private var index: UInt = 0u

    override fun hasNext(): Boolean = index < jumpTable.size

    override fun next(): UInt {
        if (!hasNext()) throw NoSuchElementException()
        val value = jumpTable.getByIndex(index) ?: throw IllegalStateException("Unexpected null value")
        index++
        return value
    }
}

// Extension function to make JumpTable iterable
operator fun JumpTable.iterator(): Iterator<UInt> = this.iterator()


fun parseBitmaskSlow(bitmask: UByteArray, offset: Int): Pair<Int, Int>? {
    if (bitmask.isEmpty()) {
        return null
    }

    var currentOffset = offset + 1
    var argsLength = 0
    while (currentOffset shr 3 < bitmask.size) {
        val byte = bitmask[currentOffset shr 3].toInt() and 0xFF
        val shift = currentOffset and 7
        val mask = byte shr shift
        val length = if (mask == 0) {
            8 - shift
        } else {
            val trailingZeros = mask.countTrailingZeroBits()
            if (trailingZeros == 0) {
                break
            }
            trailingZeros
        }

        val newArgsLength = argsLength + length
        if (newArgsLength >= Constants.BITMASK_MAX) {
            currentOffset += Constants.BITMASK_MAX - argsLength
            argsLength = Constants.BITMASK_MAX
            break
        }

        argsLength = newArgsLength
        currentOffset += length
    }

    return Pair(currentOffset, argsLength)
}


fun parseBitmaskFast(bitmask: UByteArray, offset: Int): Pair<Int, Int>? {
    var currentOffset = offset + 1

    val startIndex = currentOffset shr 3
    if (startIndex + 4 > bitmask.size) {
        return null
    }

    val shift = currentOffset and 7
    val mask: Int = (bitmask.sliceArray(startIndex until startIndex + 4)
        .foldIndexed(0) { index, acc, byte -> acc or ((byte.toInt() and 0xFF) shl (index * 8)) }
        shr shift) or (1 shl Constants.BITMASK_MAX)

    val argsLength = mask.countTrailingZeroBits()
    assert(argsLength <= Constants.BITMASK_MAX) { "argsLength should not exceed BITMASK_MAX" }
    currentOffset += argsLength

    return Pair(currentOffset, argsLength)
}

//class Instructions(
//    private val code: UByteArray,
//    private val bitmask: UByteArray,
//    private var offset: Int
//) : Iterator<ParsedInstruction> {
//
//    fun offset(): UInt = offset.toUInt()
//
//    fun <T> visit(visitor: InstructionVisitor<T>): T? {
//        // TODO: Make this directly dispatched?
//        return next().visit(visitor)
//    }
//
//    override fun next(): ParsedInstruction {
//        return parseInstruction(code, bitmask, offset).also { parsedInstruction ->
//            offset = parsedInstruction?.offset?.toInt() ?: offset
//        }
//    }
//
//    fun sizeHint(): Pair<Int, Int?> {
//        return Pair(0, (code.size - minOf(offset, code.size)).takeIf { it >= 0 })
//    }
//
//    companion object {
//        fun new(code: UByteArray, bitmask: UByteArray, offset: UInt): Instructions {
//            return Instructions(code, bitmask, offset.toInt())
//        }
//    }
//}
//
//data class ParsedInstruction(
//    val kind: Instruction,
//    val offset: UInt,
//    val length: UInt
//) {
//    operator fun component1(): Instruction = kind
//
//    override fun toString(): String {
//        return "%7d: %s".format(offset, kind)
//    }
//}

//interface InstructionVisitor<T> {
//    fun visit(instruction: ParsedInstruction): T
//}


enum class LineProgramOp(val value: Int) {
    FinishProgram(0),
    SetMutationDepth(1),
    SetKindEnter(2),
    SetKindCall(3),
    SetKindLine(4),
    SetNamespace(5),
    SetFunctionName(6),
    SetPath(7),
    SetLine(8),
    SetColumn(9),
    SetStackDepth(10),
    IncrementLine(11),
    AddLine(12),
    SubLine(13),
    FinishInstruction(14),
    FinishMultipleInstructions(15),
    FinishInstructionAndIncrementStackDepth(16),
    FinishMultipleInstructionsAndIncrementStackDepth(17),
    FinishInstructionAndDecrementStackDepth(18),
    FinishMultipleInstructionsAndDecrementStackDepth(19);

    companion object {
        fun fromInt(value: Int): LineProgramOp? {
            return entries.find { it.value == value }
        }
    }
}

class DisplayName(private val prefix: String, private val suffix: String) {
    override fun toString(): String {
        return buildString {
            append(prefix)
            if (prefix.isNotEmpty()) {
                append("::")
            }
            append(suffix)
        }
    }
}


sealed class SourceLocation {
    data class Path(val path: String) : SourceLocation()
    data class PathAndLine(val path: String, val line: Int) : SourceLocation()
    data class Full(val path: String, val line: Int, val column: Int) : SourceLocation()

    // The path to the original source file.
    fun path(): String = when (this) {
        is Path -> path
        is PathAndLine -> path
        is Full -> path
    }

    // The line in the original source file.
    fun line(): Int? = when (this) {
        is Path -> null
        is PathAndLine -> line
        is Full -> line
    }

    // The column in the original source file.
    fun column(): Int? = when (this) {
        is Path -> null
        is PathAndLine -> null
        is Full -> column
    }

    override fun toString(): String = when (this) {
        is Path -> path
        is PathAndLine -> "$path:$line"
        is Full -> "$path:$line:$column"
    }
}


/**
 * A binary search implementation which can work on chunks of items, and guarantees that it
 * will always return the first item if there are multiple identical consecutive items.
 */
fun binarySearch(
    bytes: ByteArray,
    chunkSize: Int,
    compare: (ByteArray) -> Int
): Result<Int> {
    var size = bytes.size / chunkSize
    if (size == 0) {
        return Result.failure(IndexOutOfBoundsException("Empty array"))
    }

    var base = 0
    while (size > 1) {
        val half = size / 2
        val mid = base + half
        val item = bytes.sliceArray(mid * chunkSize until (mid + 1) * chunkSize)
        when (compare(item)) {
            1 -> {
                // The value we're looking for is to the left of the midpoint.
                size -= half
            }

            -1 -> {
                // The value we're looking for is to the right of the midpoint.
                size -= half
                base = mid
            }

            0 -> {
                // We've found the value, but it might not be the first value.
                val previousItem = bytes.sliceArray((mid - 1) * chunkSize until mid * chunkSize)
                if (compare(previousItem) != 0) {
                    // It is the first value.
                    return Result.success(mid * chunkSize)
                }

                // It's not the first value. Let's continue.
                //
                // We could do a linear search here which in the average case
                // would probably be faster, but keeping it as a binary search
                // will avoid a worst-case O(n) scenario.
                size -= half
            }
        }
    }

    val item = bytes.sliceArray(base * chunkSize until (base + 1) * chunkSize)
    val comparison = compare(item)
    return if (comparison == 0) {
        Result.success(base * chunkSize)
    } else {
        Result.failure(IndexOutOfBoundsException((base + if (comparison < 0) 1 else 0) * chunkSize))
    }
}
