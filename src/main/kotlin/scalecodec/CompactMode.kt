package scalecodec

import java.math.BigInteger

enum class CompactMode(val value: Byte) {
    SINGLE(0b00),
    TWO(0b01),
    FOUR(0b10),
    BIGINT(0b11);

    companion object {
        private val MAX = BigInteger.TWO.pow(536) - BigInteger.ONE

        fun byValue(value: Byte): CompactMode = entries.first { it.value == value }

        fun forNumber(number: Int): CompactMode = forNumber(number.toLong())

        fun forNumber(number: Long): CompactMode {
            require(number >= 0) { "Negative numbers are not supported" }
            return when {
                number <= 0x3f -> SINGLE
                number <= 0x3fff -> TWO
                number <= 0x3fffffff -> FOUR
                else -> BIGINT
            }
        }

        fun forNumber(number: BigInteger): CompactMode {
            require(number.signum() >= 0) { "Negative numbers are not supported" }
            require(number <= MAX) { "Numbers larger than 2**536-1 are not supported" }
            return when {
                number > BigInteger.valueOf(0x3fffffff) -> BIGINT
                number > BigInteger.valueOf(0x3fff) -> FOUR
                number > BigInteger.valueOf(0x3f) -> TWO
                else -> SINGLE
            }
        }
    }
}
