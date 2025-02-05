package io.forge.jam.pvm

import java.math.BigInteger

object ArithmeticOps {
    fun minSigned(lhs: Int, rhs: Int): Int {
        return if (lhs <= rhs) lhs else rhs
    }

    fun minSigned64(lhs: Long, rhs: Long): Long {
        return if (lhs <= rhs) lhs else rhs
    }

    fun maxSigned(lhs: Int, rhs: Int): Int {
        return if (lhs >= rhs) lhs else rhs
    }

    fun maxSigned64(lhs: Long, rhs: Long): Long {
        return if (lhs >= rhs) lhs else rhs
    }

    fun divu(lhs: UInt, rhs: UInt): UInt {
        return if (rhs == 0u) {
            UInt.MAX_VALUE
        } else {
            lhs / rhs
        }
    }

    fun divu64(lhs: ULong, rhs: ULong): ULong {
        return if (rhs == 0uL) {
            ULong.MAX_VALUE
        } else {
            lhs / rhs
        }
    }

    fun div(lhs: Int, rhs: Int): Int {
        return when {
            rhs == 0 -> -1
            lhs == Int.MIN_VALUE && rhs == -1 -> lhs
            else -> lhs / rhs
        }
    }

    fun div64(lhs: Long, rhs: Long): Long {
        return when {
            rhs == 0L -> -1L
            lhs == Long.MIN_VALUE && rhs == -1L -> lhs
            else -> lhs / rhs
        }
    }

    fun remu(lhs: UInt, rhs: UInt): UInt {
        return if (rhs == 0u) {
            lhs
        } else {
            lhs % rhs
        }
    }

    fun remu64(lhs: ULong, rhs: ULong): ULong {
        return if (rhs == 0uL) {
            lhs
        } else {
            lhs % rhs
        }
    }

    fun rem(lhs: Int, rhs: Int): Int {
        return when {
            rhs == 0 -> lhs
            lhs == Int.MIN_VALUE && rhs == -1 -> 0
            else -> lhs % rhs
        }
    }

    fun rem64(lhs: Long, rhs: Long): Long {
        return when {
            rhs == 0L -> lhs
            lhs == Long.MIN_VALUE && rhs == -1L -> 0L
            else -> lhs % rhs
        }
    }

    fun mulh(lhs: Int, rhs: Int): Int {
        return ((lhs.toLong() * rhs.toLong()) shr 32).toInt()
    }

    fun mulh64(lhs: Long, rhs: Long): Long {
        return ((BigInteger.valueOf(lhs) * BigInteger.valueOf(rhs)) shr 64).toLong()
    }

    fun mulhsu(lhs: Int, rhs: UInt): Int {
        return ((lhs.toLong() * rhs.toLong()) shr 32).toInt()
    }

    fun mulhsu64(a: Long, b: ULong): Long {
        val aWide = a.toBigInteger()
        val bWide = b.toString().toBigInteger()
        return (aWide * bWide shr 64).toLong()
    }

    fun mulhu(lhs: UInt, rhs: UInt): UInt {
        return ((lhs.toLong() * rhs.toLong()) shr 32).toUInt()
    }

    fun mulhu64(lhs: ULong, rhs: ULong): ULong {
        return ((BigInteger.valueOf(lhs.toLong()) * BigInteger.valueOf(rhs.toLong())) shr 64).toLong().toULong()
    }
}
