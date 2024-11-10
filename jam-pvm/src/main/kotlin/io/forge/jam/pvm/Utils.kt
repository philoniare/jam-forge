package io.forge.jam.pvm

object Utils {
    data class ArcBytes private constructor(
        private val data: ByteArray,
        private val offset: Int,
        private val length: Int
    ) {
        companion object {
            fun empty(): ArcBytes = ArcBytes(ByteArray(0), 0, 0)

            fun fromStatic(bytes: ByteArray): ArcBytes = ArcBytes(bytes, 0, bytes.size)
        }

        fun subslice(range: IntRange): ArcBytes {
            val start = range.first
            val end = range.last + 1
            if (start == end) {
                return empty()
            }

            assert(end >= start) { "Invalid range: end must be >= start" }
            val length = end - start
            assert(length <= this.length) { "Subslice length exceeds available length" }
            return ArcBytes(data, offset + start, length)
        }

        fun asByteArray(): ByteArray = data.copyOfRange(offset, offset + length)

        override fun equals(other: Any?): Boolean {
            if (this === other) return true
            if (other !is ArcBytes) return false
            return asByteArray().contentEquals(other.asByteArray())
        }

        override fun hashCode(): Int = asByteArray().contentHashCode()
    }

    fun alignToNextPageInt(pageSize: Int, value: Int): Int? {
        require(pageSize != 0 && (pageSize and (pageSize - 1)) == 0) {
            "page size is not a power of two"
        }

        return if (value and (pageSize - 1) == 0) {
            value
        } else {
            val maxValue = Int.MAX_VALUE
            if (value <= maxValue - pageSize) {
                (value + pageSize) and (-pageSize)
            } else {
                null
            }
        }
    }

    fun alignToNextPageLong(pageSize: Long, value: Long): Long? {
        require(pageSize.toInt() != 0 && (pageSize and (pageSize - 1)).toInt() == 0) {
            "page size is not a power of two"
        }

        return if ((value and (pageSize - 1)).toInt() == 0) {
            value
        } else {
            val maxValue = Long.MAX_VALUE
            if (value <= maxValue - pageSize) {
                (value + pageSize) and (-pageSize)
            } else {
                null
            }
        }
    }

    // Parse immediate value
    fun parseImm(text: String): Int? {
        val trimmed = text.trim()
        return when {
            trimmed.startsWith("0x") -> {
                trimmed.substring(2).toIntOrNull(16)
            }

            trimmed.startsWith("0b") -> {
                trimmed.substring(2).toIntOrNull(2)
            }

            else -> {
                trimmed.toIntOrNull() ?: trimmed.toUIntOrNull()?.toInt()
            }
        }
    }

    // Register parsing
    enum class Reg {
        R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12;

        fun lowercaseName(): String = this.name.lowercase()

        companion object {
            val ALL = values()

            private val ALT_NAMES = ALL.map { it.name.lowercase() }

            fun parse(text: String): Reg? {
                val trimmed = text.trim().lowercase()
                return ALL.firstOrNull { reg ->
                    trimmed == reg.lowercaseName() || trimmed == "r${reg.ordinal}"
                }
            }
        }
    }

    // ByteArray initialization helper
    fun initializeByteArray(destination: ByteArray, source: ByteArray) {
        require(destination.size == source.size) {
            "Destination and source arrays must have the same size"
        }
        source.copyInto(destination)
    }
}
