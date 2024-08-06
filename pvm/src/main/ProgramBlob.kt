import jdk.jshell.spi.ExecutionControl.NotImplementedException
import java.lang.Exception
import java.nio.ByteBuffer

class ProgramBlob {
    var roDataSize: UInt = 0u
    var rwDataSize: UInt = 0u
    var stackSize: UInt = 0u

    var roData: ByteBuffer = ByteBuffer.allocate(0)
    var rwData: ByteBuffer = ByteBuffer.allocate(0)
    var code: ByteBuffer = ByteBuffer.allocate(0)
    var jumpTable: ByteBuffer = ByteBuffer.allocate(0)
    var jumpTableEntrySize: UByte = 0u
    var bitmask: ByteBuffer = ByteBuffer.allocate(0)
    var importOffsets: ByteBuffer = ByteBuffer.allocate(0)
    var importSymbols: ByteBuffer = ByteBuffer.allocate(0)
    var exports: ByteBuffer = ByteBuffer.allocate(0)

    var debugStrings: ByteBuffer = ByteBuffer.allocate(0)
    var debugLineProgramRanges: ByteBuffer = ByteBuffer.allocate(0)
    var debugLinePrograms: ByteBuffer = ByteBuffer.allocate(0)

    companion object {
        fun parse(bytes: ByteBuffer): Result<ProgramBlob> {
            val parts = ProgramParts.fromBytes(bytes)
            return parts.flatMap { fromParts(it) }
        }

        fun fromParts(parts: ProgramParts): Result<ProgramBlob> {
            val blob = ProgramBlob().apply {
                roDataSize = parts.roDataSize
                rwDataSize = parts.rwDataSize
                stackSize = parts.stackSize

                roData = parts.roData
                rwData = parts.rwData
                exports = parts.exports
                importSymbols = parts.importSymbols
                importOffsets = parts.importOffsets
                debugStrings = parts.debugStrings
                debugLineProgramRanges = parts.debugLineProgramRanges
                debugLinePrograms = parts.debugLinePrograms
            }

            if (blob.roData.remaining() > blob.roDataSize.toInt()) {
                return Result.failure(Exception("size of the read-only data payload exceeds the declared size of the section"))
            }

            if (blob.rwData.remaining() > blob.rwDataSize.toInt()) {
                return Result.failure(Exception("size of the read-write data payload exceeds the declared size of the section"))
            }

            if (parts.codeAndJumpTable.remaining() == 0) {
                return Result.failure(Exception("no code found"))
            }

            // Parse code and jump table
            val reader = Reader(parts.codeAndJumpTable)
            val initialPosition = reader.position

            val jumpTableEntryCount = reader.readVarint()
            if (jumpTableEntryCount > VM_MAXIMUM_JUMP_TABLE_ENTRIES) {
                return Result.failure(Exception("the jump table section is too long"))
            }

            blob.jumpTableEntrySize = reader.readByte().toUByte()
            val codeLength = reader.readVarint()
            if (codeLength > VM_MAXIMUM_CODE_SIZE) {
                return Result.failure(Exception("the code section is too long"))
            }

            if (blob.jumpTableEntrySize.toInt() !in 0..4) {
                return Result.failure(Exception("invalid jump table entry size"))
            }

            val jumpTableLength = jumpTableEntryCount * blob.jumpTableEntrySize.toUInt()
            blob.jumpTable = reader.readSliceAsBytes(jumpTableLength.toInt())
            blob.code = reader.readSliceAsBytes(codeLength.toInt())

            val bitmaskLength = parts.codeAndJumpTable.remaining() - (reader.position - initialPosition)
            blob.bitmask = reader.readSliceAsBytes(bitmaskLength)

            val expectedBitmaskLength = (blob.code.remaining() + 7) / 8
            if (blob.bitmask.remaining() != expectedBitmaskLength) {
                return Result.failure(Exception("the bitmask length doesn't match the code length"))
            }

            return Result.success(blob)
        }
    }

    fun roData(): ByteArray = roData.array()
    fun roDataSize(): UInt = roDataSize
    fun rwData(): ByteArray = rwData.array()
    fun rwDataSize(): UInt = rwDataSize
    fun stackSize(): UInt = stackSize
    fun code(): ByteArray = code.array()
    fun bitmask(): ByteArray = bitmask.array()

    fun imports(): Imports = Imports(importOffsets, importSymbols)

    fun exports(): Iterator<ProgramExport<ByteArray>> {
        return object : Iterator<ProgramExport<ByteArray>> {
            private var state: ExportState = ExportState.Uninitialized
            private val reader = Reader(exports)

            override fun hasNext(): Boolean {
                return when (state) {
                    is ExportState.Uninitialized -> true
                    is ExportState.Pending -> (state as ExportState.Pending).remaining > 0
                    is ExportState.Finished -> false
                }
            }

            override fun next(): ProgramExport<ByteArray> {
                val remaining = when (val currentState = state) {
                    is ExportState.Uninitialized -> reader.readVarint()
                    is ExportState.Pending -> currentState.remaining
                    is ExportState.Finished -> return throw NoSuchElementException()
                }

                if (remaining == 0u) {
                    state = ExportState.Finished
                    throw NoSuchElementException()
                }

                val targetCodeOffset = reader.readVarint()
                val symbol = reader.readBytesWithLength()
                val export = ProgramExport(
                    targetCodeOffset = targetCodeOffset,
                    symbol = ProgramSymbol(symbol)
                )

                state = ExportState.Pending(remaining - 1u)
                return export
            }
        }
    }

    fun instructions(): Instructions = Instructions(code(), bitmask(), 0)

    fun instructionsAt(offset: UInt): Instructions? {
        val bitmaskArray = bitmask()
        return if ((bitmaskArray[(offset / 8u).toInt()] shr (offset % 8u).toInt()) and 1 == 0) {
            null
        } else {
            Instructions(code(), bitmaskArray, offset.toInt())
        }
    }

    fun jumpTable(): JumpTable = JumpTable(jumpTable, jumpTableEntrySize.toUInt())

    fun getDebugString(offset: UInt): Result<String> {
        val reader = Reader(debugStrings)
        reader.skip(offset.toInt())
        return reader.readStringWithLength()
    }

    fun getDebugLineProgramAt(nthInstruction: UInt): Result<LineProgram?> {
        if (debugLineProgramRanges.remaining() == 0 || debugLinePrograms.remaining() == 0) {
            return Result.success(null)
        }

        if (debugLinePrograms.get(0).toInt() != VERSION_DEBUG_LINE_PROGRAM_V1) {
            return Result.failure(Exception("the debug line programs section has an unsupported version"))
        }

        val ENTRY_SIZE = 12
        val slice = debugLineProgramRanges

        if (slice.remaining() % ENTRY_SIZE != 0) {
            return Result.failure(Exception("the debug function ranges section has an invalid size"))
        }

        val offset = binarySearch(slice.array(), ENTRY_SIZE) { xs ->
            val begin = ByteBuffer.wrap(xs, 0, 4).int
            when {
                nthInstruction < begin.toUInt() -> 1
                nthInstruction >= ByteBuffer.wrap(xs, 4, 4).int.toUInt() -> -1
                else -> 0
            }
        }

        if (offset !is Result.Success) {
            return Result.success(null)
        }

        val xs = slice.array().copyOfRange(offset.value, offset.value + ENTRY_SIZE)
        val indexBegin = ByteBuffer.wrap(xs, 0, 4).int.toUInt()
        val indexEnd = ByteBuffer.wrap(xs, 4, 4).int.toUInt()
        val infoOffset = ByteBuffer.wrap(xs, 8, 4).int.toUInt()

        if (nthInstruction < indexBegin || nthInstruction >= indexEnd) {
            return Result.failure(Exception("binary search for function debug info failed"))
        }

        val reader = Reader(debugLinePrograms)
        reader.skip(infoOffset.toInt())

        return Result.success(
            LineProgram(
                entryIndex = offset.value / ENTRY_SIZE,
                regionCounter = 0,
                blob = this,
                reader = reader,
                isFinished = false,
                programCounter = indexBegin,
                stack = Array(16) { LineProgramFrame() },
                stackDepth = 0u,
                mutationDepth = 0u
            )
        )
    }

    // Helper classes and functions

    private sealed class ExportState {
        object Uninitialized : ExportState()
        data class Pending(val remaining: UInt) : ExportState()
        object Finished : ExportState()
    }

    companion object {
        const val VM_MAXIMUM_JUMP_TABLE_ENTRIES: UInt = 1024u * 1024u
        const val VM_MAXIMUM_CODE_SIZE: UInt = 16u * 1024u * 1024u
        const val VERSION_DEBUG_LINE_PROGRAM_V1: Int = 1
    }
}

// Additional classes needed for ProgramBlob
class ProgramParts {
    var roDataSize: UInt = 0u
    var rwDataSize: UInt = 0u
    var stackSize: UInt = 0u

    var roData: ByteBuffer = ByteBuffer.allocate(0)
    var rwData: ByteBuffer = ByteBuffer.allocate(0)
    var codeAndJumpTable: ByteBuffer = ByteBuffer.allocate(0)
    var importOffsets: ByteBuffer = ByteBuffer.allocate(0)
    var importSymbols: ByteBuffer = ByteBuffer.allocate(0)
    var exports: ByteBuffer = ByteBuffer.allocate(0)

    var debugStrings: ByteBuffer = ByteBuffer.allocate(0)
    var debugLineProgramRanges: ByteBuffer = ByteBuffer.allocate(0)
    var debugLinePrograms: ByteBuffer = ByteBuffer.allocate(0)

    companion object {
        fun fromBytes(bytes: ByteBuffer): Result<ProgramParts> {
            throw NotImplementedException
        }
    }
}

class Imports(val offsets: ByteBuffer, val symbols: ByteBuffer)

data class ProgramExport<T>(val targetCodeOffset: UInt, val symbol: ProgramSymbol<T>)

class ProgramSymbol<T>(val value: T)

class Instructions(val code: ByteArray, val bitmask: ByteArray, val offset: Int)

class JumpTable(val blob: ByteBuffer, val entrySize: UInt)

class LineProgram(
    val entryIndex: Int,
    var regionCounter: Int,
    val blob: ProgramBlob,
    val reader: Reader,
    var isFinished: Boolean,
    var programCounter: UInt,
    val stack: Array<LineProgramFrame>,
    var stackDepth: UInt,
    var mutationDepth: UInt
)

class LineProgramFrame {
    var kind: FrameKind? = null
    var namespaceOffset: UInt = 0u
    var functionNameOffset: UInt = 0u
    var pathOffset: UInt = 0u
    var line: UInt = 0u
    var column: UInt = 0u
}

enum class FrameKind {
    ENTER, CALL, LINE
}

class Reader(private val buffer: ByteBuffer) {
    var position: Int
        get() = buffer.position()
        set(value) {
            buffer.position(value)
        }

    fun readByte(): Byte = buffer.get()

    fun readVarint(): UInt {
        // Implementation of variable-length integer reading
        throw Exception
    }

    fun readSliceAsBytes(length: Int): ByteBuffer {
        val slice = buffer.slice()
        slice.limit(length)
        buffer.position(buffer.position() + length)
        return slice
    }

    fun readBytesWithLength(): ByteArray {
        val length = readVarint().toInt()
        val bytes = ByteArray(length)
        buffer.get(bytes)
        return bytes
    }

    fun readStringWithLength(): Result<String> {
        val bytes = readBytesWithLength()
        return Result.runCatching { String(bytes, Charsets.UTF_8) }
    }

    fun skip(count: Int) {
        buffer.position(buffer.position() + count)
    }
}

fun binarySearch(array: ByteArray, chunkSize: Int, compare: (ByteArray) -> Int): Result<Int> {
    // Implementation of binary search
    throw Exception
}
